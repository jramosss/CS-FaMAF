{-# LANGUAGE TypeSynonymInstances,FlexibleInstances, OverloadedStrings #-}
module Equ.PreExpr.Internal where

import Equ.Syntax
import Equ.Types
import Control.Applicative ((<$>), (<*>),Applicative(..))
import Test.QuickCheck(Arbitrary, arbitrary, oneof)

import Data.Serialize(Serialize, get, getWord8, put, putWord8)
import Control.Arrow ((***))
import Data.Foldable(foldMap)
import qualified Data.Foldable as F
import qualified Data.Monoid as M
import Data.Function (on)
import Data.List(nub)

-- | Nuestras expresiones son un funtor cuya variable representa las
-- variables.
data PreExpr' a = Var a                -- ^ Variables
                | Con !Constant        -- ^ Constantes
                | PrExHole !Hole       -- ^ Expresión incompleta con cierta información
                | UnOp !Operator (PreExpr' a) -- ^ Operador unario con su argumento
                | BinOp !Operator (PreExpr' a) (PreExpr' a) -- ^ Operador binario con sus argumentos.
                | App (PreExpr' a) (PreExpr' a)  -- ^ Aplicación de dos expresiones
                | Quant !Quantifier a (PreExpr' a) (PreExpr' a) -- ^ Expresiones cuantificadas
                | Paren (PreExpr' a)          -- ^ Expresiones parentizadas explícitamente
                | If (PreExpr' a) (PreExpr' a) (PreExpr' a) -- ^ Expresión condicional
                | Case (PreExpr' a) [((PreExpr' a),(PreExpr' a))] -- ^ Análisis por casos
                  deriving Eq

instance Serialize a => Serialize (PreExpr' a) where
    put (Var a) = putWord8 0 >> put a
    put (Con c) = putWord8 1 >> put c
    put (PrExHole h) = putWord8 3 >> put h
    put (UnOp op pe) = putWord8 4 >> put op >> put pe
    put (BinOp op pe pe') = putWord8 5 >> put op >> put pe >> put pe'
    put (App pe pe') = putWord8 6 >> put pe >> put pe'
    put (Quant q a pe pe') = putWord8 7 >> put q >> put a >> put pe >> put pe'
    put (Paren pe) = putWord8 8 >> put pe
    put (If c e1 e2) = putWord8 9 >> put c >> put e1 >> put e2
    put (Case e patterns) = putWord8 10 >> put e >> put patterns

    get = do
    tag_ <- getWord8
    case tag_ of
        0 -> Var <$> get
        1 -> Con <$> get
        3 -> PrExHole <$> get
        4 -> UnOp <$> get <*> get
        5 -> BinOp <$> get <*> get <*> get
        6 -> App <$> get <*> get
        7 -> Quant <$> get <*> get <*> get <*> get
        8 -> Paren <$> get
        9 -> If <$> get <*> get <*> get
        10 -> Case <$> get <*> get
        _ -> fail $ "SerializeErr (PreExpr' a) " ++ show tag_

instance Functor PreExpr' where
    fmap f (Var a) = Var $ f a
    fmap _ (Con c) = Con c
    fmap _ (PrExHole h) = PrExHole h
    fmap f (UnOp op e) = UnOp op $ fmap f e
    fmap f (BinOp op e e') = BinOp op (fmap f e) (fmap f e')
    fmap f (App e e') = App (fmap f e) (fmap f e')
    fmap f (Quant q a e e') = Quant q (f a) (fmap f e) (fmap f e')
    fmap f (Paren e) = Paren $ fmap f e
    fmap f (If c e1 e2) = If (fmap f c) (fmap f e1) (fmap f e2)
    fmap f (Case e patterns) = Case (fmap f e) $ map (fmap f *** fmap f) patterns
    
instance F.Foldable PreExpr' where
    foldMap f (Var a) = f a 
    foldMap _ (Con _) = M.mempty
    foldMap _ (PrExHole _) = M.mempty
    foldMap f (UnOp _ e) = foldMap f e
    foldMap f (BinOp _ e e') = foldMap f e `M.mappend` foldMap f e'
    foldMap f (App e e')  = foldMap f e `M.mappend` foldMap f e'
    foldMap f (Quant _ a e e') = f a `M.mappend` foldMap f e `M.mappend` foldMap f e'
    foldMap f (Paren e) = foldMap f e
    foldMap f (If c e1 e2) = foldMap f c `M.mappend` foldMap f e1 `M.mappend` foldMap f e2
    foldMap f (Case e ps) =  M.mconcat (foldMap f e:map (uncurry (M.mappend `on` foldMap f)) ps)

-- | Instancia arbitrary para las preExpresiones.
instance Arbitrary PreExpr where
    arbitrary =
        oneof [   Var <$> arbitrary
                , Con <$> arbitrary
                , PrExHole <$> arbitrary
                , UnOp <$> arbitrary <*> arbitrary
                , BinOp <$> arbitrary <*> arbitrary <*> arbitrary
                , App <$> arbitrary <*> arbitrary
                , Quant <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                , Paren <$> arbitrary
                , If <$> arbitrary <*> arbitrary <*> arbitrary
                , Case <$> arbitrary <*> arbitrary
                ]

-- | Las expresiones concretas de nuestros lenguajes.
type PreExpr = PreExpr' Variable

                
-- | Pretty print para las preExpresiones.
instance Show PreExpr where
     show = showExpr'

-- | Pretty-printing con parentizado de expresiones.
showExpr' :: PreExpr -> String
showExpr' (BinOp op e1 e2) = showParentised op e1 ++ show op ++ showParentised op e2                
    where showParentised oper e = case e of
           (BinOp op' _ _) -> if opPrec oper >= opPrec op'
                             then "("++showExpr' e++")"
                             else showExpr' e
           _ -> showExpr' e
           
showExpr' (UnOp op e) = show op ++ " " ++ showParentised e 
    where showParentised e' = case e of
            (BinOp _ _ _) -> "(" ++ showExpr' e' ++ ")"
            (App _ _) -> "(" ++ showExpr' e' ++ ")"
            (Quant _ _ _ _) -> "(" ++ showExpr' e' ++ ")"
            _ -> showExpr' e'
                        
showExpr' (App e1 e2) = showExpr' e1 ++ "." ++ showExpr' e2
showExpr' (Quant q v e1 e2) = "〈" ++ show q ++ show v ++ ":" 
                              ++ showExpr' e1 ++ ":" 
                              ++ showExpr' e2 ++ "〉"
showExpr' (Paren e) = "(" ++ showExpr' e ++ ")"
showExpr' (Var x) = show x
showExpr' (Con k) = show k
showExpr' (PrExHole h) = show h
showExpr' (If c e1 e2) = "if " ++ showExpr' c ++ " then " ++ showExpr' e1 ++ " else " ++ showExpr' e2
showExpr' (Case e patterns) = "case " ++ showExpr' e ++ " of\n" ++ showPatterns patterns
    where showPatterns = unlines . map showPattern
          showPattern (p,e') = "\t" ++ showExpr' p ++ " → " ++ showExpr' e'

{-- | Funcion que, dada una PreExpr, elimina las expresiones "Paren" que son necesarias
    para desambiguar expresiones. Ejemplo:
    unParen ( Paren ( or (Paren $ equiv p q) r ) ) = Paren ( or ( equiv p q ) r )
    El parentesis que estaba en la expresión (equiv p q) fue necesario introducirlo
    para poder diferenciar la expresión " or (equiv p q) r " de la expresión
    equiv p (or (q r)).
    --}
unParen :: PreExpr -> PreExpr
unParen (BinOp op e1 e2) = BinOp op (checkParen e1 op) (checkParen e2 op)
    where checkParen e o = case e of
            (Paren (BinOp op_e e1' e2')) -> if opPrec o >= opPrec op_e
                                           then unParen (BinOp op_e e1' e2')
                                           else unParen e
            (Paren (UnOp op_e e1')) -> if opPrec o >= opPrec op_e
                                          then unParen (UnOp op_e e1')
                                          else unParen e
            _ -> unParen e
            
unParen (UnOp op e) = UnOp op (checkParen e)
    where checkParen e' = case e' of
            (Paren e'') -> case e'' of
                            (BinOp _ _ _) -> unParen e''
                            (App _ _) -> unParen e''
                            (Quant _ _ _ _) -> unParen e''  -- VER SI HACE FALTA ESTE CASO
                            (UnOp _ _) -> unParen e''
                            _ -> unParen e'
            _ -> e'
unParen (App e1 e2) = App (unParen e1) (unParen e2)
unParen (Quant q v e1 e2) = Quant q v (unParen e1) (unParen e2)
unParen (Paren e) = Paren (unParen e)
unParen (If c e1 e2) = If (unParen c) (unParen e1) (unParen e2)
unParen (Case e patterns) = Case (unParen e) (unParenAll patterns)
    where unParenAll ps = map (unParen *** unParen) ps
unParen e = e



-- | Substitucion de variable por variable en preExpresiones.
-- La precondición es que @v'@ sea fresca para @e@.
rename :: Eq a => a -> a -> PreExpr' a -> PreExpr' a
rename v v' e = substVar v v' <$> e
    where substVar w w' w'' | w == w'' = w'
                            | w /= w'' = w''
                            | otherwise = w''


          
-- | Esta funcion toma una expresión que es un operador aplicado y
-- retorna justamente el operador; sino devuelve nada.
getOperator :: PreExpr' a -> Maybe Operator
getOperator (UnOp op _) = Just op
getOperator (BinOp op _ _) = Just op
getOperator _ = Nothing

-- | Si la expresión es una constante, devuelve justamente esa constante;
-- sino devuelve nada.
getConstant :: PreExpr' a -> Maybe Constant
getConstant (Con c) = Just c
getConstant _ = Nothing

-- | Vista de una expresión como variable o como lo que es.
isVar :: a -> PreExpr' b -> Either a b
isVar _ (Var x) = Right x
isVar a _ = Left a

-- | Operador de recursión sobre @PreExpr'@.
foldE :: (a -> b) -> (Constant -> b) -> (Operator -> b -> b) -> 
        (Operator -> b -> b -> b) -> (Quantifier -> a -> b -> b -> b) -> (b -> b -> b) -> b -> 
        (b -> b -> b -> b) -> (b -> [(b,b)] -> b) ->
        PreExpr' a -> b
foldE f _ _ _ _ _ _ _ _ (Var a) = f a
foldE _ f _ _ _ _ _ _ _ (Con c) = f c
foldE _ _ _ _ _ _ b _ _ (PrExHole _) = b
foldE v c f b q a h i cs (UnOp op e) = f op (foldE v c f b q a h i cs e)
foldE v c f b q a h i cs (BinOp op e e') = b op (foldE v c f b q a h i cs e) (foldE v c f b q a h i cs e')
foldE v c f b q a h i cs (App e e') = a  (foldE v c f b q a h i cs e) (foldE v c f b q a h i cs e')
foldE v c f b qf fa h i cs (Quant q a e e') = qf q a (foldE v c f b qf fa h i cs e) (foldE v c f b qf fa h i cs e')
foldE v c f b qf a h i cs (Paren e) = foldE v c f b qf a h i cs e
foldE v c f b qf a h i cs (If e1 e2 e3) = i (foldE v c f b qf a h i cs e1) (foldE v c f b qf a h i cs e2) (foldE v c f b qf a h i cs e3)
foldE v c f b qf a h i cs (Case e patterns) = cs (foldE v c f b qf a h i cs e) (rec patterns)
    where rec = map (foldE v c f b qf a h i cs *** foldE v c f b qf a h i cs)


-- | Obtiene todas las constantes de una expresión.
getConstants :: PreExpr' a -> [Constant]
getConstants = nub . foldE (const []) 
                           return 
                           (\_ r -> r) 
                           (\_ r r' -> r++r') 
                           (\_ _ r r' -> r ++ r')
                           (++) 
                           [] 
                           (\r r' r'' -> concat [r,r',r''])
                           (\r r' -> concat (r:map (uncurry (++)) r'))

-- | Obtiene todos los operadores de una expresión.
getOperators :: PreExpr' a -> [Operator]
getOperators = nub . foldE (const []) 
                           (const []) 
                           (:) 
                           (\ o r r' -> o:r++r') 
                           (\_ _ r r' -> r ++ r') 
                           (++) 
                           [] 
                           (\r r' r'' -> concat [r,r',r''])
                           (\r r' -> concat (r:map (uncurry (++)) r'))

-- | Obtiene todos los cuantificadores de una expresión.
getQuants :: PreExpr' a -> [Quantifier]
getQuants = nub . foldE (const []) 
                        (const []) 
                        (\_ r -> r) 
                        (\_ r r' -> r++r') 
                        (\q _ r r' -> q:r ++ r')
                        (++) 
                        [] 
                        (\r r' r'' -> concat [r,r',r''])
                        (\r r' -> concat (r:map (uncurry (++)) r'))

-- | Devuelve la lista de variables que son usadas como nombres de función
-- en una expresión.
getCalledVars :: PreExpr' a -> [a]
getCalledVars (App (Var v) e) = v:getCalledVars e
getCalledVars (App e e') = getCalledVars e ++ getCalledVars e'
getCalledVars (UnOp _ e) = getCalledVars e
getCalledVars (BinOp _ e e') = getCalledVars e ++ getCalledVars e'
getCalledVars (Quant _ _ e e') = getCalledVars e ++ getCalledVars e'
getCalledVars (If e1 e2 e3) = getCalledVars e1 ++ getCalledVars e2 ++ getCalledVars e3
getCalledVars (Case e ps) = getCalledVars e ++ concatMap (uncurry ((++) `on` getCalledVars)) ps
getCalledVars (Paren e) = getCalledVars e
getCalledVars _ = []

-- | Dadas asignaciones de tipo para variables, constantes y operadores,
-- actualiza el tipo de todas las subexpresiones de una expresión.
setType :: (Variable -> Type) -> (Constant -> Type) -> (Operator -> Type) -> PreExpr -> PreExpr
setType fv fc fo = foldE (Var . updVar) 
                         (Con . updCon)
                         (UnOp . updOp) 
                         (BinOp . updOp) 
                         Quant 
                         App 
                         (PrExHole $ hole "") 
                         If 
                         Case 
    where updOp op = op {opTy = fo op}
          updVar v = v { varTy = fv v}
          updCon c = c { conTy = fc c}


