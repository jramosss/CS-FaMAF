-- | Tests para el parser: los tests consisten en verificar que ciertos
-- strings se parsean correctamente (para estos tests utilizamos HUnit).
module TestSuite.Tests.Parser (testGroupParse) where

import Equ.Parser
import Equ.PreExpr

import TestSuite.Tests.Samples

import Equ.Theories.FOL hiding (true, false)
import Equ.Theories.List

import Test.HUnit (Assertion, assertFailure)
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (unless)

-- | La lista de test del parser. Este es un caso donde es más
-- conveniente HUnit que QuickCheck pues generaríamos un montón
-- de basura no parseable.
testsParser :: [Test]
testsParser = map (\(str, expr) -> testCase (encloseQuotes str) (testParse str expr))
                [ ("p", Var p)
                , ("ys", Var ys)
                , ("S%(0)", sApp0)
                , ("S%(y)", sAppy)
                , ("(x+S%(0))", xPlussApp0)
                , ("x + S%(y) + z", xPlusSyPlusZ)
                , ("S%(y) + S%((x+S%(0))) + z", sAppyPlusSomePlusz)
                , ("[x]", listX)
                , ("[y,w]", listYW)
                , ("[x] ++ [y,w]", listXConcatListYW)
                , ("(#([x] ++ [y,w]))", lengthListXYW)              
                , ("(#([x] ++ [y,w])) + z", lengthListPlusz)
                , ("[0]", listZero)
                , ("[1,2]", listOneTwo)
                , ("[0] ++ [1,2]", listZeroConcatOneTwo)
                , ("(#([0] ++ [1,2])) + 1", lengthListPlusOne)
                , ("x = F%(x)", equ0)
                , ("xs↓1 = ys↓1", equ1)
                , ("〈∃ x : x = F%(x): xs↓1 = ys↓1〉", exist0)
                , ("〈∀ x : x = F%(x): xs↓1 = ys↓1〉", forAll0)
                ]
    where encloseQuotes str = "\""++ str ++"\""

-- TODO: Sobre el control de errores,
-- @ParseError@ no tiene instancia de @Eq@, esto genera un problema para comparar.
-- Por esa razon de momento no testeamos errores informativos de parseo.

-- | Controlamos que el parseo de un string sea el esperado, comparandolo con
-- la preExpresion que le pasamos.
testParse :: String -> PreExpr -> Assertion
testParse s pe = case parseFromString s of
                   Left _ -> assertFailure "Error de parseo."
                   Right pe' -> unless (pe == pe') $
                               assertFailure $ 
                               "\n Resultado esperado: " ++ show pe ++
                               "\n Contra : " ++ show pe'

testGroupParse :: Test
testGroupParse = testGroup "Parser" testsParser
