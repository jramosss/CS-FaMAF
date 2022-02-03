-- | Test correspondientes a las reglas de re-escritura; se
-- verifica que los lados izquierdo y derecho de cada regla
-- se pueda tipar y que ambos tipos sean unificables.
module TestSuite.Tests.Rules where

import Equ.Theories
import Equ.Expr (Expr(..))
import Equ.PreExpr (PreExpr)
import Equ.TypeChecker
import Equ.Rule(Rule(..))
import Data.Text (unpack)
import Test.HUnit (Assertion, assertFailure)
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

-- | Controla que una expresión esté bien tipada.
testTypeable :: PreExpr -> Assertion
testTypeable e = case checkPreExpr e of 
                   Left _ -> assertFailure $ "No se pudo tipar la expresión: " ++ show e
                   Right _ -> return ()

testEqTypes :: PreExpr -> PreExpr -> Assertion
testEqTypes e e' = case checkPreExpr e of
                          Left _ -> err e
                          Right t -> case checkPreExpr e' of
                                           Left _ -> err e'
                                           Right t' -> case unify t t' emptySubst of
                                                        Left _ -> err' t t'
                                                        Right _ -> return ()
    where err er = assertFailure $ "No se pudo tipar la expresión: " ++ show er
          err' t t' = assertFailure $ "Los tipos de lhs con rhs no son unificables: (" 
                                    ++ show t ++"," ++ show t' ++")"

-- TODO: Verificar que los tipos son iguales.
-- | Controla que los dos lados de una regla estén bien tipados.
testRule :: Rule -> Test
testRule rule = testGroup ("Regla: " ++ nameRule) 
                [ testCase "Lado izquierdo: " . testTypeable $ l
                , testCase "Lado derecho: "   . testTypeable $ r
                , testCase "Tipos iguales: "  . testEqTypes l $ r
                ] 
    where nameRule = unpack $ name rule
          (Expr l) = lhs rule
          (Expr r) = rhs rule

-- TODO: Extenderlo a las otras teorías.
-- | Aplica 'testRule' a todas las reglas de una teoría.
testListRules :: Test
testListRules = testGroup "Teoría de Listas" $ map testRule listRules