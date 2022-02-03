module Language.Syntax where

{- En el lenguaje de programación
   tendremos dos tipos: Booleanos y Enteros.
-}
data Type = BoolT | IntT
    deriving Show

{- Variables con su tipo. -}

type VarName = String
    
data Var = Var String Type
    deriving Show
    
{- Expresiones enteras -}
data IntExpr = ConstI Int              -- Constantes
             | VI VarName              -- Variables enteras
             | Neg IntExpr             -- Negación
             | Plus IntExpr IntExpr    -- Suma
             | Prod IntExpr IntExpr    -- Producto
             | Div IntExpr IntExpr     -- División
             | Mod IntExpr IntExpr     -- Módulo

data BoolExpr = ConstB Bool             -- Constantes
              | VB VarName             -- Variables booleanas
              | And BoolExpr BoolExpr  -- Conjunción
              | Or BoolExpr BoolExpr   -- Disjunción
              | Not BoolExpr           -- Negación
              | Equal IntExpr IntExpr  -- Igual
              | Less IntExpr IntExpr   -- Menor estricto
             

{- 
    Sentencias del lenguaje
-}
data Statement = Skip                      -- No hacer nada
               | AssignB Var BoolExpr      -- Asignación de variable booleana
               | AssignI Var IntExpr       -- Asignación de variable entera
               | Seq Statement Statement   -- Secuencia
               | If [(BoolExpr,Statement)] -- Condicional
               | Do BoolExpr Statement     -- Ciclo
    deriving Show
               
               
-- Instancia Show para las expresiones
instance Show IntExpr where
    show (ConstI i)   = show i
    show (VI v)       = v
    show (Neg e)      = "- " ++ show e
    show (Plus e1 e2) = parens (show e1 ++ " + " ++ show e2)
    show (Prod e1 e2) = parens (show e1 ++ " * " ++ show e2)
    show (Div e1 e2)  = parens (show e1 ++ " / " ++ show e2)
    show (Mod e1 e2)  = parens (show e1 ++ " % " ++ show e2)
    
instance Show BoolExpr where
    show (ConstB b)    = show b
    show (VB v)        = v
    show (And e1 e2)   = parens (show e1 ++ " && " ++ show e2)
    show (Or e1 e2)    = parens (show e1 ++ " || " ++ show e2)
    show (Not e)       = parens ("not " ++ show e)
    show (Equal e1 e2) = parens (show e1 ++ " = " ++ show e2)
    show (Less e1 e2)  = parens (show e1 ++ " < " ++ show e2)

parens :: String -> String
parens s = "( " ++ s ++ " )"
