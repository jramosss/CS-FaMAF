{-# Language  TemplateHaskell, DoAndIfThenElse,OverloadedStrings #-}
{-| Algoritmo de chequeo e inferencia de tipos para pre-expre-
siones. Este algoritmo es esencialmente el de Hindley-Milner-Damas
para el cálculo lambda: si tenemos informacion en la pre-expresion
entonces se verifica que sea unificable con el tipo de inferido. A
diferencia de esos algoritmos, no se hay un contexto donde se declara
el tipo de las variabes, ya que la informacion de las variables
(símbolos de función y constantes son tratadas exactamente de la misma
manera) está contenida en la expresión misma (en este aspecto se
parece más al chequeo de un cálculo à la Church).
-}
module Equ.TypeChecker.State
    ( module Equ.TypeChecker.Error      
      -- * Algoritmo de unificaci&#243;n con relaci&#243;n de orden.
    , TySubst
    , emptySubst
    , unifyTest
    , unificate
    , rewrite
    , rewriteS
    , unifyS
    , unifyListS
    , tyerr
    , match
    , match2
    , mgu
      -- * Algoritmo de TypeChecking.
    , TCState(..)
    , initCtx
    , initTCState
    , TCCtx(..)
    , TyState
    , TMErr
    , Env
    , getCtx
    , getFreshTyVar
    -- * Otras funciones útiles
    , updAss
    , updAss'
    , updateAss
    -- * Manipulacion del estado
    , withTypes
    , extCtxV
    , extCtxVar
    , initCtxE
    , setType'
    , setTypeS
    -- * 
    , quants
    , ops
    , vars
    , cons
    , ctx
    , CtxSyn
    , mkCtxVar
    )
    where

import           Equ.Syntax
import           Equ.PreExpr
import           Equ.Types
import           Equ.Theories.AbsName
import           Equ.TypeChecker.Error
import           Equ.TypeChecker.Unification

import qualified Data.Map as M
import qualified Data.Text as T

import           Control.Applicative((<$>))
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State
import qualified Data.Set as Set (elems)

import           Control.Lens hiding (cons,rewrite,op)

-- | Ciertos s&#237;mbolos deben tener un &#250;nico tipo en toda una expresi&#243;n;
-- un contexto lleva la cuenta de todos los tipos que vamos viendo. En
-- principio s&#243;lo deber&#237;a un tipo a lo sumo.
type CtxSyn s = M.Map s Type
type Env = M.Map VarName Type
    
-- | El contexto global es un conjunto con los contextos de cada tipo
-- de s&#237;mbolo; el contexto para los cuantificadores es fijo,
-- inicialmente tiene los cuantificadores "homog&#233;neos" (por ejemplo,
-- sumatoria est&#225;, pero forall no est&#225;).
data TCCtx = TCCtx { _vars :: CtxSyn VarName
                   , _ops  :: CtxSyn OpName 
                   , _cons :: CtxSyn ConName
                   , _quants :: CtxSyn QuantName
                   }
         deriving Show

$(makeLenses ''TCCtx)

type TMErr = TyErr

-- TODO: cambiar: el estado tendr&#237;a el contexto adem&#225;s de la
-- sustituci&#243;n.
-- | La m&#243;nada de estado del type-checker.
-- type TyState = MonadTraversal TMErr TCState

type TyState a = StateT TCState (Either TyErr) a

data TCState = TCState { _subst :: TySubst
                       , _ctx   :: TCCtx
                       , _fTyVar :: Int
                       }
             deriving Show

$(makeLenses ''TCState)

-- | Generaci&#243;n de mensaje de Error.
tyerr :: TyErr -> TyState a
tyerr = lift . Left

getSub :: TyState TySubst
getSub = use subst

getCtx :: TyState TCCtx
getCtx = use ctx

getFreshTyVar :: TyState Type
getFreshTyVar = use fTyVar >>= \n -> 
                fTyVar %= (1+) >>
                return (TyVar $ T.pack (show n))

modCtx :: (TCCtx -> TCCtx) -> TyState ()
modCtx f = ctx %= f

modSubst :: (TySubst -> TySubst) -> TyState ()
modSubst f = subst %= f >> getSub >>= updateCtxS


{- 

   Las PreExpresiones y las Expresiones son b&#225;sicamente &#225;rboles de
   sintaxis abstracta con distinta informaci&#243;n en cada nodo.  Por
   ejemplo, podr&#237;a ser que las PreExpresiones tengan un componente de
   tipo 'Maybe Type', mientras que el mismo componente en una Expresi&#243;n
   ser&#225; de tipo 'Type'. Esto nos permite ver las PreExpresiones c&#243;mo
   Expresiones parcialmente tipadas.

   Una cosa que s&#237; necesitamos es informaci&#243;n sobre por qu&#233; fall&#243; un
   chequeo/inferencia de tipos. 

   El type-checker usar&#225; en lo posible la informaci&#243;n de tipos de las
   PreExpresiones; de esta manera podremos tener un chequeo incremental.

-}

extCtx :: (Syntactic s,Ord k) => (s -> k) -> s -> Type -> CtxSyn k -> CtxSyn k
extCtx f s = M.insertWith (flip const) (f s)

extCtxV :: Variable -> Type -> TyState Type
extCtxV v t = modCtx (vars %~ extCtx varName v t) >> return t

extCtxVar :: VarName -> Type -> TyState ()
extCtxVar v t = modCtx (vars %~ M.insertWith (flip const) v t)

extCtxOp :: Operator -> Type -> TyState ()
extCtxOp o t = modCtx (ops %~ extCtx opName o t)

extCtxCon :: Constant -> Type -> TyState ()
extCtxCon c t = modCtx (cons %~ extCtx conName c t)

extCtxQuan :: Quantifier -> Type -> TyState ()
extCtxQuan q t = modCtx (quants %~ extCtx quantName q t)

errSynNoType :: (Syntactic s) => s -> TyState a
errSynNoType = tyerr . ErrNoType

withTypes :: (Syntactic s) => s -> Maybe Type -> TyState Type
withTypes s Nothing = errSynNoType s
withTypes _ (Just t) = rewriteS t


updAss :: Env -> TyState ()
updAss ass = getSub >>= \subs -> 
             (ctx . vars) .= updateAss subs ass >>
             updateCtxS subs

updAss' :: TyState ()
updAss' = getSub >>= \subs -> 
          (ctx . vars) %= updateAss subs >>
          updateCtxS subs


-- | Actualiza los tipos en el contexto.
updateCtx :: TCCtx -> TySubst -> TCCtx
updateCtx ct subs = execState (do vars %= apSubs ;
                                  ops  %= apSubs ;
                                  cons %= apSubs ;
                                  quants %= apSubs ;
                                  ) ct
          where apSubs = updateAss subs
          
updateAss :: TySubst -> M.Map k Type -> M.Map k Type
updateAss s = M.map (rewrite s)


updateCtxS :: TySubst -> TyState ()
updateCtxS = modCtx . flip updateCtx


extSub :: TySubst -> TyState ()
extSub s = modSubst (@@ s)

unifyS :: Type -> Type -> TyState TySubst
unifyS t t' = do s <- getSub
                 case mgu (app s t) (app s t') of
                      Left err -> tyerr err
                      Right s' -> extSub s'
                 getSub

unifyListS :: [Type] -> TyState TySubst
unifyListS [] = getSub
unifyListS [_] = getSub
unifyListS (t:t':ts) = unifyS t t' >> unifyListS (t':ts)

rewriteS :: Type -> TyState Type
rewriteS t = flip rewrite t <$> getSub

initCtx :: TCCtx
initCtx = TCCtx { _vars = M.empty
                , _ops  = M.empty
                , _cons = M.empty
                , _quants = M.empty
                }


initTCState :: TCState
initTCState = TCState { _subst = emptySubst
                      , _ctx = initCtx
                      , _fTyVar = 0
                      }

-- | Construye un contexto con variables frescas para las variables
-- que no tienen un tipo 
mkCtxVar :: PreExpr -> TyState ()
mkCtxVar e = mapM_ updCtx vs
    where vs = Set.elems $ freeVars e
          updCtx v = renTy M.empty (varTy v) >>= extCtxV v . fst

mkCtxOps :: PreExpr -> TyState ()
mkCtxOps = mapM_ updCtx . getOperators
    where updCtx op = renTy M.empty (opTy op) >>= extCtxOp op . fst

mkCtxCon :: PreExpr -> TyState ()
mkCtxCon = mapM_ updCtx . getConstants
    where updCtx con = renTy M.empty (conTy con) >>= extCtxCon con . fst

mkCtxQuan :: PreExpr -> TyState ()
mkCtxQuan = mapM_ updCtx . getQuants
    where updCtx quan = renTy M.empty (quantTy quan) >>= extCtxQuan quan . fst

-- | Dado un tipo, reemplaza todas las variables libres del
-- tipo por variables frescas.
renTy :: TySubst -> Type -> TyState (Type,TySubst)
renTy s TyUnknown = getFreshTyVar >>= \t -> return (t,s)
renTy s t@(TyAtom _) = return (t,s)
renTy s (TyVar v) = maybe newVar (\t -> return (t,s)) $ M.lookup v s
    where newVar = getFreshTyVar >>= \w ->
                   return (w, M.insert v w s)
renTy s (TyList t) = renTy s t >>= \(t',s') -> return (TyList t',s')
renTy s (t :-> t') = renTy s t >>= \(t1,s') -> 
                     renTy s' t' >>= \(t2,s'') -> return (t1 :-> t2,s'')

initCtxE :: PreExpr -> TyState ()
initCtxE e = mkCtxVar e >> mkCtxOps e >> mkCtxCon e >> mkCtxQuan e

mkSubst :: TCCtx -> (Variable -> Type, Constant -> Type, Operator -> Type)
mkSubst (TCCtx vs os cs _) = (updVar,updCons,updOps)
    where updVar = M.foldrWithKey (go varName) tyUnk vs
          updCons = M.foldrWithKey (go conName) tyUnk cs
          updOps =  M.foldrWithKey (go opName) tyUnk os
          tyUnk _ = TyUnknown          
          go acc k ty f v = if acc v == k then ty else f v


setType' :: PreExpr -> TCCtx -> PreExpr
setType' e ct = setType v c o e
         where (v,c,o) = mkSubst ct

setTypeS :: PreExpr -> TyState PreExpr
setTypeS e = liftM (setType' e) $ use ctx
