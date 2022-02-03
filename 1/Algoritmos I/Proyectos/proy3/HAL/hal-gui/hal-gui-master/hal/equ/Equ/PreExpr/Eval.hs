module Equ.PreExpr.Eval where

import Equ.Syntax
import Equ.Theories.AbsName
import Equ.PreExpr.Symbols

import Equ.PreExpr.Internal
import Control.Applicative


-- | Funcion para internalizar numerales.
intToCon :: Int -> PreExpr
intToCon 0 = Con $ natZero
intToCon n = UnOp (natSucc) $ intToCon (n-1)


semNBinOp :: Operator -> Maybe (Int -> Int -> Int)
semNBinOp op = case opName op of
                Sum -> Just (+)
                Prod -> Just (*)
                Div -> Just div
                Dif -> Just (-)
                Mod -> Just mod
                _ -> Nothing

semNUnOp :: Operator -> Maybe (Int -> Int)
semNUnOp op = case opName op of
               Succ -> Just (1+)
               Pred -> Just (\i -> i-1)
               _ -> Nothing

semNConst :: Constant -> Maybe Int
semNConst c = case conName c of 
                Zero -> Just 0
                _ -> Nothing


-- | Versión para pretty-printing.
evalNat :: PreExpr -> Maybe Int
evalNat (Con c) = semNConst c
evalNat (UnOp op e) = semNUnOp op <*> evalNat e
evalNat _ = Nothing


evalN :: PreExpr -> Maybe Int
evalN (Con c) = semNConst c
evalN (UnOp op e) = semNUnOp op <*> evalN e
evalN (BinOp op e e') = semNBinOp op <*> evalN e <*> evalN e'
evalN _ = Nothing


evalExpr :: PreExpr -> PreExpr
evalExpr (Var v) = Var v
evalExpr (Con c) = Con c
evalExpr (PrExHole h) = PrExHole h
evalExpr e@(UnOp op e') = maybe (UnOp op (evalExpr e')) intToCon $ evalN e
evalExpr e@(BinOp op e1 e2) = maybe e' intToCon $ evalN e
    where e' = case (evalN e1, evalN e2) of
                 (Nothing,Nothing) ->  BinOp op (evalExpr e1) (evalExpr e2)
                 (Just m, Nothing) -> BinOp op (intToCon m) (evalExpr e2)
                 (Nothing, Just n) -> BinOp op (evalExpr e1) (intToCon n)
                 (Just m,Just n) -> BinOp op (intToCon m) (intToCon n)
evalExpr (App e e') = App (evalExpr e) (evalExpr e')
evalExpr (Quant q v r t) = Quant q v (evalExpr r) (evalExpr t)
evalExpr (Paren e) = Paren (evalExpr e)
evalExpr (If b t f) = If (evalExpr b) (evalExpr t) (evalExpr f)
evalExpr (Case e cs) = Case e cs