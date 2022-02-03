module Language.Semantics where

import Language.Syntax
import Language.ListAssoc

-- Asignación de valores para las variables enteras
type StateI = ListAssoc VarName Int
-- Asignación de valores para las variables booleanas
type StateB = ListAssoc VarName Bool

defaultIntValue :: Int
defaultIntValue = 0

defaultBoolValue :: Bool
defaultBoolValue = True

-- Tipo que representa la continuación de un paso de ejecución.
-- Ésta puede ser: Falta ejecutar una sentencia (ToExec), o ya no hay nada por
-- ejecutar (Finish).
data Continuation = ToExec Statement
                  | Finish


-- El estado consta del valor de las variables enteras y las booleanas
type State = (StateI,StateB)

evalIExpr :: IntExpr -> StateI -> Int
evalIExpr (ConstI x) _ = x
evalIExpr (VI x) stateI = case (la_busca stateI x) of
                    Just b -> b
                    Nothing -> defaultIntValue
evalIExpr (Neg x) state = -(evalIExpr x state)
evalIExpr (Plus x y) state = (evalIExpr x state) + (evalIExpr y state)
evalIExpr (Prod x y) state = (evalIExpr x state) * (evalIExpr y state)
evalIExpr (Div x y) state = (evalIExpr x state) `div` (evalIExpr y state)
evalIExpr (Mod x y) state = (evalIExpr x state) `mod` (evalIExpr y state)
-- Para evaluar las expresiones booleanas
-- necesitamos también el estado de variables enteras
-- porque en Equal y Less tenemos subexpresiones enteras.
evalBExpr :: BoolExpr -> State -> Bool
evalBExpr (ConstB x) _ = x
evalBExpr (VB x) state = case (la_busca (snd state) x) of
                         Just b -> b
                         Nothing -> defaultBoolValue
evalBExpr (And x y) state = (evalBExpr x state) && (evalBExpr y state)  
evalBExpr (Or x y) state = (evalBExpr x state ) || (evalBExpr y state) 
evalBExpr (Not x) state = not (evalBExpr x state)
evalBExpr (Equal x y) state = (evalIExpr x (fst state)) == (evalIExpr y (fst state))
evalBExpr (Less x y) state = (evalIExpr x (fst state)) < (evalIExpr y (fst state))

-- Evaluar un paso de ejecución en un programa.
evalStep :: Statement -> State -> (State , Continuation)
evalStep Skip state = (state,Finish)
evalStep (AssignB (Var x BoolT) expry) (stateI,stateB) = ((stateI, (la_agregar x (evalBExpr expry (stateI,stateB)) stateB)), Finish) 

evalStep (AssignI (Var x IntT) expry)  (stateI, stateB) = ((la_agregar x (evalIExpr expry stateI) stateI,stateB), Finish)


evalStep (Seq s1 s2) e1 = case evalStep s1 e1 of
                              (e2,Finish) -> (e2, ToExec s2)
                              (e2, ToExec s1') -> (e2, (ToExec (Seq s1' s2)))
--evalStep (If ((b1,s1):xs)) e = case evalStep b1 s1 of
evalStep (If ((b1,s1):gs)) e = case evalBExpr b1 e of
                                   True -> (e, ToExec s1)
                                   False -> (e, ToExec (If gs))
evalStep (Do b s) e = case evalBExpr b e of
                           False -> (e, Finish)
                           True ->  (evalStep (do s) e)
