
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
-- Definición de los tipos de declaraciones que tenemos en el lenguaje.
-- 
----------------------------------------------------------------------------

{-# Language NoMonomorphismRestriction #-}
module Fun.Decl where

import qualified Equ.PreExpr as PE
import Equ.Syntax
import Equ.Rule hiding (rel)
import Equ.Proof
import Equ.Types
import Equ.Expr
import Equ.TypeChecker (getType)
import Equ.Theories (createHypothesis,makeExprFromRelation, createHypothesis')
import Data.Text (pack,unpack,Text)
import Control.Lens hiding (op)

import Fun.Decl.Error

-- | Una declaración anotada tiene su posición.
type Annot a = (DeclPos,a)

-- | Especificaciones de funciones.
data SpecDecl = Spec Variable [Variable] PE.PreExpr

instance Show SpecDecl where
    show (Spec f args e ) = Prelude.unlines [ show f ++ " :: " ++ show (varTy f)
                                            , show f ++ " " ++ show args ++ " = " ++ show e
                                            ]


specName :: Lens' SpecDecl Variable
specName = lens g s
    where g (Spec f _ _) = f
          s (Spec _ as spec) f = Spec f as spec

specArgs :: Lens' SpecDecl [Variable]
specArgs = lens g s
    where g (Spec _ as _) = as
          s (Spec f _ spec) as = Spec f as spec

specSpec :: Lens' SpecDecl PE.PreExpr
specSpec = lens g s
    where g (Spec _ _ spec) = spec
          s (Spec f as _) spec = Spec f as spec


instance Eq SpecDecl where
    (Spec f _ _) == (Spec f' _ _) = f == f'

-- | Declaración de una propiedad, útil para declarar hipótesis.
data PropDecl = Prop Text PE.PreExpr
    deriving Show

instance Eq PropDecl where
    (Prop t _) == (Prop t' _) = t == t'

-- | Declaración de un teorema.
data ThmDecl = Thm Theorem PE.PreExpr

instance Show ThmDecl where
    show (Thm _ e) = show $ e

instance Eq ThmDecl where
    thm == thm' = getNameDecl thm == getNameDecl thm'

-- | Declaración de una función.
data FunDecl = Fun Variable [Variable] PE.PreExpr (Maybe Text) -- Puede tener la verificación o no.


funDeclName :: Lens' FunDecl Variable
funDeclName = lens getFunName setFunName
    where setFunName :: FunDecl -> Variable -> FunDecl
          setFunName (Fun _ args e t) f = Fun f args e t

          getFunName :: FunDecl -> Variable
          getFunName (Fun f _ _ _) = f

funDeclArgs :: Lens' FunDecl [Variable]
funDeclArgs = lens getFunArgs setFunArgs
    where setFunArgs :: FunDecl -> [Variable] -> FunDecl
          setFunArgs (Fun f _ e t) as = Fun f as e t

          getFunArgs :: FunDecl -> [Variable]
          getFunArgs (Fun _ as _ _) = as


funDeclBody :: Lens' FunDecl PE.PreExpr
funDeclBody = lens getFunBody setFunBody
    where setFunBody :: FunDecl -> PE.PreExpr -> FunDecl
          setFunBody (Fun f as _ t) e = Fun f as e t

          getFunBody :: FunDecl -> PE.PreExpr
          getFunBody (Fun _ _ e _) = e


instance Show FunDecl where
    show (Fun f args e _) = Prelude.unlines [ show f ++ " :: " ++ show (varTy f)
                                            , show f ++ " " ++ show args ++ " = " ++ show e
                                            ]

-- POR QUE ESTA INSTANCIA ESTA DEFINIDA ASI??????
instance Eq FunDecl where
    (Fun f _ _ _) == (Fun f' _ _ _) = f == f'

-- | otra forma de definir igualdad de declaración de funciones!!!
isEq :: FunDecl -> FunDecl -> Bool
isEq (Fun v vs expr _) (Fun v' vs' expr' _) = v==v' && vs==vs' && expr==expr'


-- | Declaración de un valor.
data ValDecl = Val Variable PE.PreExpr

instance Show ValDecl where
    show (Val v e) = Prelude.unlines [ show v ++ " :: " ++ show (varTy v)
                                     , show v ++ " = " ++ show e
                                     ]


valVar :: Lens ValDecl ValDecl Variable Variable
valVar = lens g s
    where g (Val v _) = v
          s (Val _ e) v = Val v e

valExp :: Lens ValDecl ValDecl PE.PreExpr PE.PreExpr
valExp = lens g s
    where g (Val _ e) = e
          s (Val v _) e = Val v e
    
instance Eq ValDecl where
    (Val v _) == (Val v' _) = v == v'


-- | Declaración de un operador.
data OpDecl = OpDecl Operator [Variable] PE.PreExpr
    deriving Show

instance Eq OpDecl where
    (OpDecl op _ _) == (OpDecl op' _ _) = op == op'

-- | Declaración de una derivación, el primer argumento es el nombre
-- de la función y el segundo es la variable recursiva (según Manu).
data DerivDecl = Deriv Variable Variable [(PE.Focus,Proof)]

derFun :: Lens' DerivDecl Variable
derFun = lens g s
    where g (Deriv fun _ _) = fun
          s (Deriv _ v css) fun = Deriv fun v css

derVar :: Lens' DerivDecl Variable
derVar = lens g s
    where g (Deriv _ v _) = v
          s (Deriv fun _ css) v = Deriv fun v css
          
derCases :: Lens' DerivDecl [(PE.Focus,Proof)]
derCases = lens g s
    where g (Deriv _ _ css) = css
          s (Deriv fun v _) css = Deriv fun v css


instance Show DerivDecl where
    show (Deriv v v' fps) = "Deriv "
                          ++ show v ++ " " ++ show v' ++ " " 
                          ++ show (map fst fps)

instance Eq DerivDecl where
    (Deriv v _ _) == (Deriv v' _ _) = v == v'
    

-- | Declaración de un tipo.
data TypeDecl = NewType Type [Constant] [(Operator,[Variable],PE.PreExpr)] -- Para implementar a futuro.
    deriving (Eq,Show)

getFunDerivingFrom :: FunDecl -> Maybe Text
getFunDerivingFrom (Fun _ _ _ mt) = mt

class Decl a where
    getNameDecl   :: a -> Text
    getFuncDecl   :: a -> Maybe Variable
    getExprDecl   :: a -> Maybe PE.PreExpr
    getVarsDecl   :: a -> Maybe [Variable]
    getFocusProof :: a -> Maybe [(PE.Focus,Proof)]
    createHypDecl :: a -> Maybe Hypothesis

instance Decl SpecDecl where
    getNameDecl (Spec f _ _) = tRepr f
    getFuncDecl (Spec f _ _) = Just f
    getExprDecl (Spec _ _ e) = Just e
    getVarsDecl (Spec _ vs _) = Just vs
    getFocusProof _ = Nothing
    createHypDecl (Spec f vs e) = getType e >>= return . createHyp "spec" f vs e

instance Decl PropDecl where
    getNameDecl (Prop t _) = t
    getFuncDecl _ = Nothing
    getExprDecl (Prop _ e) = Just e
    getVarsDecl _ = Nothing
    getFocusProof _ = Nothing
    createHypDecl (Prop t e) = Just $ createHypothesis (pack $ "prop "++ unpack t)
                                 (Expr e) (GenConditions [])
    
instance Decl ThmDecl where
    getNameDecl (Thm t _) =  truthName t
    getFuncDecl _ = Nothing
    getExprDecl (Thm _ e) = Just e
    getVarsDecl _ = Nothing
    getFocusProof _ = Nothing
    createHypDecl (Thm t _) = 
        Just $ createHypothesis (truthName t) (thExpr t) (GenConditions [])
    
instance Decl FunDecl where
    getNameDecl (Fun f _ _ _) =  tRepr f
    getFuncDecl (Fun f _ _ _) = Just f
    getExprDecl (Fun _ _ p _) = Just p
    getVarsDecl (Fun _ vs _ _) = Just vs
    getFocusProof _ = Nothing
    createHypDecl (Fun f vs e _) = getType e >>= return . createHyp "fun" f vs e

createHyp :: String -> Variable -> [Variable] -> PE.PreExpr -> Type -> Hypothesis
createHyp kind f vs e t = createHypothesis' (pack $ kind ++ " " ++ show f)
                                           expr
                                           rel
                                           noCondition
    where rel = getRelationFromType t
          expr = makeExprFromRelation rel (PE.exprApply f vs) e

instance Decl ValDecl where
    getNameDecl (Val v _) =  tRepr v
    getFuncDecl _ = Nothing
    getExprDecl (Val _ p) = Just p
    getVarsDecl _ = Nothing
    getFocusProof _ = Nothing
    createHypDecl (Val v e) =
        getType e >>= \te -> (\rel ->
        return $ createHypothesis (pack $ "val "++ show v)
                                  (makeExprFromRelation rel (PE.Var v) e)
                                  (GenConditions [])) (getRelationFromType te)

instance Decl DerivDecl where
    getNameDecl   (Deriv fun _ _) = tRepr fun
    getFuncDecl   (Deriv fun _ _) = Just fun
    getVarsDecl   (Deriv _ v _) = Just [v]
    getFocusProof (Deriv _ _ fps) = Just fps
    getExprDecl _ = Nothing
    createHypDecl (Deriv _ _ _) = Nothing

instance Decl TypeDecl where
    getNameDecl _ =  pack ""
    getFuncDecl _ = Nothing
    getExprDecl _ = Nothing
    getVarsDecl _ = Nothing
    getFocusProof _ = Nothing
    createHypDecl _ = Nothing

sameDecl :: (Decl d,Decl d') => d' -> d -> Bool
sameDecl d d' = getNameDecl d == getNameDecl d'


isPrg :: PE.PreExpr -> Bool
isPrg (PE.Quant _ _ _ _) = False
isPrg (PE.PrExHole _) = False
isPrg (PE.UnOp _ pe) = isPrg pe
isPrg (PE.BinOp _ pe pe') = isPrg pe && isPrg pe'
isPrg (PE.App pe pe') = isPrg pe && isPrg pe'
isPrg (PE.If c e1 e2) = isPrg c && isPrg e1 && isPrg e2
isPrg (PE.Case e patterns) = isPrg e && all (uncurry (&&) . over both isPrg) patterns
isPrg (PE.Paren pe) = isPrg pe
isPrg _ = True
