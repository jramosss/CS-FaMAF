{-# LANGUAGE TypeSynonymInstances #-}
module Equ.PreExpr.Show where

import Equ.Syntax
import Equ.PreExpr.Internal
import Equ.PreExpr.Eval
import Equ.Theories.AbsName

showL :: PreExpr -> String -> Maybe String
showL (Con c) xs = if conName c == Empty 
                   then Just $ "[" ++ xs ++ "]"
                   else Nothing
showL (BinOp op e e') xs = if opName op == Append 
                           then showL e' (xs' ++ showExpr e)
                           else Nothing
    where xs' = if null xs then xs else xs ++ ","
showL _ _ = Nothing
                                  

showExpr :: PreExpr -> String
showExpr (Var x) = show x
showExpr (Con k) = show k
showExpr (PrExHole h) = show h
showExpr e@(UnOp op e') = 
    case evalNat e of
        Nothing -> show op ++ " " ++ showWithParentsUn e'
        Just n -> show n
        
    where showWithParentsUn e1 = case e1 of
            (BinOp _ _ _) -> "(" ++ showExpr e1 ++ ")"
            (App _ _) -> "(" ++ showExpr e1 ++ ")"
            (Quant _ _ _ _) -> "(" ++ showExpr e1 ++ ")"
            _ -> showExpr e1
        
showExpr l@(BinOp op e1 e2) = 
    case showL l "" of 
        Nothing -> showWithParentsBin e1 ++ show op ++ showWithParentsBin e2
        Just l' -> l'
        
    where showWithParentsBin e = case e of
           (BinOp op' _ _) -> if opPrec op >= opPrec op'
                             then "(" ++ showExpr e ++ ")"
                             else showExpr e
           _ -> showExpr e
           
showExpr (App e e') = showExpr e ++ "@" ++ showExpr e'
showExpr (Quant q v r t) = "〈" ++ show q ++ show v ++ ":" 
                           ++ showExpr r ++ ":" 
                           ++ showExpr t ++ "〉"
showExpr (Paren e) = "(" ++ showExpr e ++ ")"
showExpr (If c e1 e2) = "if " ++ showExpr c ++ " then " ++ showExpr e1 ++ " else " ++ showExpr e2
showExpr (Case e patterns) = "case " ++ showExpr e ++ " of\n" ++ showPatterns patterns
    where showPatterns = unlines . map showPattern
          showPattern (p,e') = "\t" ++ showExpr p ++ " -> " ++ showExpr e'
