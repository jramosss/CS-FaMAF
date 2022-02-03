-- | Test muy simple para comprobar la corrección del zipper.
module TestSuite.Tests.PreExpr (
              prop_toFocuses
            )
    where

import Equ.PreExpr

-- Checkea que (forall e):
--   forall t \in toFocuses e, toExpr t = e
prop_toFocuses :: PreExpr -> Bool
prop_toFocuses pe = prop_toFocusesAux pe (toFocuses pe)

-- Auxiliar para checkear la propiedad de arriba.
prop_toFocusesAux :: PreExpr -> [Focus] -> Bool
prop_toFocusesAux _ [] = True
prop_toFocusesAux pe (f:fs) | toExpr f == pe = prop_toFocusesAux pe fs
                            | otherwise = False
