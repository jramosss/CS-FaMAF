-- | Definición de los errores de matching.

module Equ.Matching.Error where
import Equ.PreExpr

-- | Errores de matching.
data MatchError = DoubleMatch Variable PreExpr PreExpr
                | BindingVar Variable
                | InequPreExpr PreExpr PreExpr
                | InequOperator Operator Operator
                | InequQuantifier Quantifier Quantifier
                deriving Eq

-- | Pretty print de errores de matching.
instance Show MatchError where
    show (DoubleMatch v pe pe') = "Variable \"" ++ show v ++ 
                                "\" matched with \"" ++ show pe ++ 
                                "\" fail to match with \"" ++ show pe' ++ "\""
    show (BindingVar v) = "Binding variable \"" ++ show v ++ "\""
    show (InequPreExpr e e')  = "\"" ++ show e ++ 
                                    "\" =/= \"" ++ 
                                    show e' ++ "\""
    show (InequOperator e e')  = "\"" ++ show e ++ 
                                    "\" =/= \"" ++ 
                                    show e' ++ "\""
    show (InequQuantifier e e')  = "\"" ++ show e ++ 
                                    "\" =/= \"" ++ 
                                    show e' ++ "\""
