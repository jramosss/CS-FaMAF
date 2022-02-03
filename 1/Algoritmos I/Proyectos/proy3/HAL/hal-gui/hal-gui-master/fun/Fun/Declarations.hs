----------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  (c) Proyecto Theona, 2012-2013
--                (c) Alejandro Gadea, Emmanuel Gunther, Miguel Pagano
-- License     :  <license>
-- 
-- Maintainer  :  miguel.pagano+theona@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Definición de los distintos tipos de declaraciones y de los chequeos
-- asociados a cada uno de ellos.
-- 
----------------------------------------------------------------------------
{-# Language TemplateHaskell, ViewPatterns #-}
module Fun.Declarations where

import Equ.Syntax hiding (Func)
import Equ.Expr (Expr(..))
import qualified Equ.PreExpr as PE (PreExpr, freeVars)
import Equ.Proof hiding (setCtx, getCtx)
import Equ.Proof.Proof 
import Equ.Types
import Equ.Theories.FOL (true,equiv)

import Fun.Theories
import Fun.Theory
import Fun.Decl
import Fun.Decl.Error
import Fun.Derivation.Error
import Equ.IndType

import qualified Data.List as L
import qualified Data.Set as S 
import qualified Data.Map as M
import Data.Text hiding (map,concatMap,unlines,reverse,foldl)
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Monoid
import Text.Parsec.Pos (newPos)
import Control.Arrow(second,(&&&))

import Control.Monad.Trans.State
import Control.Lens


data DefDuplicated = DSpec | DFun | DThm | DProp | DVal
    deriving Eq

class Duplicated d where
    dupName :: d -> DefDuplicated

instance Duplicated SpecDecl where
    dupName _ = DSpec

instance Duplicated FunDecl where
    dupName _ = DFun

instance Duplicated ThmDecl where
    dupName _ = DThm

instance Duplicated PropDecl where
    dupName _ = DProp

instance Duplicated ValDecl where
    dupName _ = DVal

data InvalidDeclarations = 
        InvalidDeclarations { inSpecs     :: [ErrInDecl  SpecDecl]
                            , inFunctions :: [ErrInDecl  FunDecl]
                            , inTheorems  :: [ErrInDecl  ThmDecl]
                            , inProps     :: [ErrInDecl  PropDecl]
                            , inVals      :: [ErrInDecl  ValDecl]
                            , inDerivs    :: [ErrInDeriv DerivDecl]
                            }
    deriving Show



data Declarations = 
    Declarations { _specs     :: [Annot SpecDecl]
                 , _functions :: [Annot FunDecl]
                 , _theorems  :: [Annot ThmDecl]
                 , _props     :: [Annot PropDecl]
                 , _vals      :: [Annot ValDecl]
                 , _derivs    :: [Annot DerivDecl]
                 , _indTypes  :: [(Type,IndType)] -- Si luego extendemos para declarar tipos, este campo del environment va agregando cada uno de
                                           -- los nuevos tipos declarados. Por ahora usaremos solo el valor inicial que le pasamos,
                                           -- el cual contiene los tipos basicos de Equ.
                 }

$(makeLenses ''Declarations)

type WithError d = Either (ErrInDecl d) d

bare :: Getting [Annot d] Declarations [Annot d] -> Declarations -> [d]
bare f = map snd . (^. f)

bareThms :: Declarations -> [ThmDecl]
bareThms = bare theorems

instance Monoid Declarations where
    mempty = Declarations [] [] [] [] [] [] []
    mappend d' = execState $ do specs     %= (++ (d' ^. specs )) ;
                                functions %= (++ (d' ^. functions)) ;
                                theorems  %= (++ (d' ^. theorems))  ;
                                props     %= (++ (d' ^. props));
                                vals      %= (++ (d' ^. vals));
                                derivs    %= (++ (d' ^. derivs));
                                indTypes  %= (++ (d' ^. indTypes))

filterValidDecls :: Declarations -> InvalidDeclarations -> Declarations                 
filterValidDecls vds ivds = 
             Declarations
                (L.filter (`notIn` (inSpecs ivds)) $ _specs vds)
                (L.filter (`notIn` (inFunctions ivds)) $ _functions vds)
                (L.filter (`notIn` (inTheorems ivds)) $ _theorems vds)
                (L.filter (`notIn` (inProps ivds)) $ _props vds)
                (L.filter (`notIn` (inVals ivds)) $ _vals vds)
                (L.filter (`notIn'` (inDerivs ivds)) $ _derivs vds)
                []
    where
        notIn' :: (Eq d, Decl d) => Annot d -> [ErrInDeriv d] -> Bool
        notIn' (_,d) errds = d `L.notElem` (L.map snd errds)
        notIn :: (Eq d, Decl d) => Annot d -> [ErrInDecl d] -> Bool
        notIn (_,d) errds = d `L.notElem` (L.map eDecl errds)


instance Show Declarations where
    show decls = unlines [ ""
                         , "Especificaciones: "
                         ,  show (_specs decls)
                         , ""
                         , "Funcioness:  " 
                         , show (bare functions decls) 
                         , ""
                         , "Teoremas:  "
                         , show (bare theorems decls) 
                         , ""
                         , "Proposiciones: " 
                         , show (bare props decls) 
                         , ""
                         , "Valores:  " 
                         , show (bare vals decls)
                         , "" 
                         , "Derivaciones:  "
                         , "[" ++ concatMap showDer (decls ^. derivs) ++ "]"
                         ]
        where
            showDer :: Annot DerivDecl -> String
            showDer (dPos, Deriv v v' fps) = 
                    "(" ++ show dPos ++ "," ++ 
                    "Deriv " ++ show v ++ " " ++
                    show v' ++ " " ++ 
                    ("[" ++ concatMap (show . fst) fps ++ "]") ++ 
                    ")"

envAddFun :: Annot FunDecl -> Declarations -> Declarations
envAddFun = over functions . (:) 

envAddSpec :: Annot SpecDecl -> Declarations -> Declarations
envAddSpec = over specs . (:) 

envAddVal :: Annot ValDecl -> Declarations -> Declarations
envAddVal = over vals . (:)

envAddTheorem :: Annot ThmDecl -> Declarations -> Declarations
envAddTheorem = over theorems . (:)

envAddProp :: Annot PropDecl -> Declarations -> Declarations
envAddProp = over props . (:)

envAddDeriv :: Annot DerivDecl -> Declarations -> Declarations
envAddDeriv = over derivs . (:)

valsDef :: Declarations -> [Variable]
valsDef = L.map (^. (_2 . valVar)) . (^. vals)

funcsDef :: Declarations -> [Variable]
funcsDef = L.map (^. (_2 . funDeclName)) . (^. functions)

mkErrInDecl :: Decl d => Annot d -> [DeclError] -> ErrInDecl d
mkErrInDecl ann err = ErrInDecl (ann ^. _1) err (ann ^. _2)

okDecl :: (Decl d) => Annot d -> [DeclError] -> Either (ErrInDecl d) d
okDecl ann [] = Right (ann ^. _2)
okDecl ann err = Left $ mkErrInDecl ann err

checkDecls :: (Decl d) =>
               Getting [Annot d] Declarations [Annot d]
                   -> Declarations
                   -> Declarations
                   -> (Declarations -> Annot d -> [DeclError])
                   -> [Either (ErrInDecl d) d]
checkDecls decl ds imds checks = over traverse (\ann -> okDecl ann $ (checks declsWithImports) ann) (ds ^. decl)
    where declsWithImports = ds <> imds

-- | Chequeo de declaración: chequeamos que no esté duplicada, además
-- de algún chequeo propio de la declaración.
checkDecl :: (Eq d,Decl d,Duplicated d) => (d -> Declarations -> [DeclError]) -> 
              Declarations -> Annot d -> [DeclError]
checkDecl chk decls ann = mconcat [ chk (ann ^. _2) , checkDoubleDef ann ] decls

-- | Chequeo de especificaciones; el único control que hacemos es que
-- todas las varibles estén declaradas.
checkSpecs :: Declarations -> Declarations -> 
              [Either (ErrInDecl SpecDecl) SpecDecl]
checkSpecs ds imds = checkDecls specs ds imds $ \d -> mconcat [checkDecl checkDefVar d,chkDups]
    where chkDups :: Annot SpecDecl -> [DeclError]
          chkDups (_,Spec f vs _) = checkArgsNotDup (f:vs)

-- | Chequeo de funciones; además del chequeo de variables,
-- verificamos que el cuerpo sea un programa.
checkFuns :: Declarations ->  Declarations -> [Either (ErrInDecl FunDecl) FunDecl]
checkFuns ds imds = checkDecls functions ds imds $ \d -> mconcat [checkDecl checkDefVar d, chkPrg,chkDups]
    where
        chkPrg :: Annot FunDecl -> [DeclError]
        chkPrg (checkIsPrg . (^. _2) -> False) = [InvalidPrgDeclaration]
        chkPrg _  = []
        chkDups :: Annot FunDecl -> [DeclError]
        chkDups (_,Fun f vs _ _ ) = checkArgsNotDup (f:vs)

-- | Chequeo de declaración de valores
checkVals :: Declarations -> Declarations ->
             [Either (ErrInDecl ValDecl) ValDecl]
checkVals ds imds = checkDecls vals ds imds $ checkDecl checkDefVarVal

        
-- | Chequeo de teoremas; además del chequeo de nombres duplicados,
-- verificamos que la prueba sea correcta. A diferencia de las otras
-- entidades, para los teoremas nos aseguramos que los teoremas usados
-- sean correctos.
checkThm :: Declarations -> Declarations ->
            [Either (ErrInDecl ThmDecl) ThmDecl]
checkThm ds imds = merge' $ foldl chkThm ([],[]) thmDefs
    where chkThm :: ([ErrInDecl ThmDecl],[ThmDecl]) -> Annot ThmDecl -> ([ErrInDecl ThmDecl],[ThmDecl])
          chkThm prevs decl = check prevs decl $ L.concat 
                                                  [ chkThmProof (snd prevs) (decl ^. _2)
                                                  , checkDoubleDef decl (ds <> imds)
                                                  , chkThmExpr (decl ^. _2) 
                                                  ]

          prfWithDecls thms (Thm t _) = addDeclHypothesis ds (thms ++ bareThms imds) imds (thProof t)
          thmDefs :: [Annot ThmDecl]
          thmDefs = reverse $ ds ^. theorems
          merge' (ers,oks) = map Left ers ++ map Right oks
          
          check :: ([ErrInDecl ThmDecl],[ThmDecl]) -> Annot ThmDecl -> [DeclError] -> ([ErrInDecl ThmDecl],[ThmDecl])
          check prevs (_,thm) [] = over _2 (thm:) prevs
          check prevs decl ers   = over _1 (mkErrInDecl decl ers:) prevs

          chkThmProof :: [ThmDecl] -> ThmDecl -> [DeclError]
          chkThmProof thms = either (return . InvalidProofForThm) (const []) . 
                               validateProof . prfWithDecls thms

          -- | La expresión del teorema es la misma que la de la prueba.
          -- O la expresión del teorema es @p@ y la prueba es @p == True@.
          chkThmExpr :: ThmDecl -> [DeclError]
          chkThmExpr (Thm t e) = if e == e' || (equiv (Expr e) true) == (Expr e') 
                                 then [] 
                                 else [InvalidExprForThm e e']
                     where (Expr e') = thExpr t

hypListFromDecls :: Declarations -> [ThmDecl] -> [Hypothesis]
hypListFromDecls decls thms = mapMaybe createHypDecl thms <>
                              mconcat [ hyps _specs
                                      , hyps _functions
                                      , hyps _vals
                                      , hyps _props] decls 
                              
    where hyps :: Decl d => (Declarations -> [Annot d]) -> Declarations -> [Hypothesis]
          hyps f ds = mapMaybe (createHypDecl . snd) $ f ds
        
-- Esta funcion agrega a una prueba las hipótesis correspondientes a
-- todas las declaraciones definidas y los teoremas validos.
addDeclHypothesis :: Declarations -> [ThmDecl] -> Declarations -> Proof -> Proof
addDeclHypothesis decls validThms mImportDecls pr = 
    foldl (\p h -> addCtxJust (addHypothesis' h M.empty) p) pr $ hypListFromDecls dswi validThms
--     foldl addHyps pr $ hypListFromDecls dswi validThms

    where dswi :: Declarations 
          dswi = decls <> mImportDecls


-- | Chequeo de que las variables usadas estén dentro del scope.
checkDefVar :: Decl d => d -> Declarations -> [DeclError]
checkDefVar d ds = concatMap inScope . S.toList . PE.freeVars . getFocusDecl $ d
    where
        inScope :: Variable -> [DeclError]
        inScope v = if v `L.elem` vars then [] else [NotInScopeVar v]
        vars = valsDef ds ++ funcsDef ds ++ fromMaybe [] (getVarsDecl d)

-- | Chequeo de que las variables que ocurren en una declaración
-- de valor estén 
checkDefVarVal :: ValDecl ->  Declarations -> [DeclError]
checkDefVarVal d ds = checkDefVar d $ (vals  %~ L.filter ((d/=) . snd)) ds

-- | Chequeo de que no haya argumentos repetidos.
checkArgsNotDup :: [Variable] -> [DeclError]
checkArgsNotDup = map (ArgDuplicated . varName) . getDups
                where getDups = go S.empty S.empty
                      go _ dups [] = S.toList dups
                      go seen dups (x:xs) = go (x `S.insert` seen) dups' xs
                         where dups' = if x `S.member` seen 
                                       then (x `S.insert` dups) 
                                       else dups

getFocusDecl :: Decl d => d -> PE.PreExpr
getFocusDecl = fromMaybe (error "Not an expression!") .  getExprDecl

checkIsPrg :: Decl d => d -> Bool
checkIsPrg = isPrg . getFocusDecl

-- | Chequeo por nombres de entidades duplicados. Puesto que las
-- funciones pueden tener especificaciones, filtramos las
-- especificaciones cuando comprobamos funciones y viceversa.
checkDoubleDef :: (Duplicated d,Decl d,Eq d) => Annot d -> 
                                   Declarations -> [DeclError]
checkDoubleDef (dPos,decl) = mconcat [ checkDoubleDef' . (^. vals)
                                     , checkDoubleDef' . (^. props)
                                     , checkDoubleDef' . (^. theorems)
                                     , whenL (dupName decl /= DSpec) . checkDoubleDef' . (^. functions)
                                     , whenL (dupName decl /= DFun) . checkDoubleDef' . (^. specs)
                                     ]
    where
        whenL :: Bool -> [DeclError] -> [DeclError]
        whenL b ds = if b then ds else []
        mErr :: (Decl d, Eq d) => Annot d -> [DeclError]
        mErr (dPos',d') = if getNameDecl decl == getNameDecl d' && dPos /= dPos'
                          then [DuplicateName $ getNameDecl decl]
                          else []
        checkDoubleDef' :: (Decl d, Eq d) => [Annot d] -> [DeclError]
        checkDoubleDef' decls = concatMap mErr decls

initThms :: [Theorem]
initThms = concatMap theorytheorems [arithTheory,listTheory,folTheory]

mapIndTypes :: [(Type' TyVarName, IndType)]
mapIndTypes = [ (TyAtom ATyNat,natural)
              , (TyAtom ATyBool,bool)
              , (TyList (tyVar "A"), list)
              ]

emptyInDecls :: InvalidDeclarations
emptyInDecls = InvalidDeclarations { inSpecs      = []
                                   , inFunctions  = []
                                   , inTheorems   = []
                                   , inProps      = []
                                   , inVals       = []
                                   , inDerivs     = []
                                   }

initDeclarations :: Declarations
initDeclarations = flip execState mempty $ do theorems .= map (const initDeclPos &&& mkThm) initThms;
                                              indTypes .= mapIndTypes
    where
        initDeclPos = DeclPos initPosThms initPosThms (pack "")
        initPosThms = newPos "TeoremasIniciales" 0 0
        mkThm thm = Thm thm e
            where (Expr e) = thExpr thm

modifyFunDecl :: (FunDecl -> FunDecl) -> Declarations -> Declarations
modifyFunDecl f = over functions (map (second f))
