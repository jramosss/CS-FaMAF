-- | Tests para pruebas. Hay dos aspectos que se controlan: primero
-- con QuickCheck se verifica la corrección de la serialización; en
-- segundo lugar, se utiliza HUnit para verificar que las funciones de
-- manipulación y construcción de pruebas no tienen errores.
module TestSuite.Tests.Proof (
            testGroupProof
            )
    where


import Test.Framework (testGroup, Test)
import Test.HUnit (Assertion, assertFailure)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad (unless)

import qualified Data.Map as M (empty)

import Equ.Expr
import Equ.Rule
import Data.Text
import Equ.Proof
import Equ.Parser
import Equ.PreExpr hiding (goDownR)
import Equ.Theories.FOL
import Equ.Theories.List

-- | Test sobre serializacion; @decode . encode == id@ Notar que este 
-- test, implica verificar la serialización de la mayor parte de las 
-- estructuras.
prop_serialization :: Proof -> Bool
prop_serialization p = let Right p' = (decode . encode) p in p == p'

-- Focus utilizados en Unit-Test.
fFx :: Focus
fFx = toFocus $ parser "F%(x)"
fFy :: Focus 
fFy = toFocus $ parser "F%(y)"
fp :: Focus
fp = toFocus $ parser "p"
fq :: Focus
fq = toFocus $ parser "q"
fy :: Focus
fy = toFocus $ parser "y"
fz :: Focus
fz = toFocus $ parser "z"
fQ0 :: Focus
fQ0 = toFocus $ parser "〈∀ x : R%(x) ∨ S%(x) : T%(x)〉"
fQ1 :: Focus
fQ1 = toFocus $ parser "〈∀ x : R%(x) : T%(x)〉 ∧ 〈∀ x : S%(x) : T%(x)〉"
fequivNeg0 :: Focus
fequivNeg0 = toFocus $ parser "¬(p ≡ q) ≡ ¬p"
fstar0 :: Focus
fstar0 = toFocus $ parser "p ∨ q"
fstar1 :: Focus
fstar1 = toFocus $ parser "p ∨ ¬q ≡ p"
fgr0 :: Focus
fgr0 = toFocus $ parser "p ∧ q"
fgr1 :: Focus
fgr1 = toFocus $ parser "p ≡ q ≡ p ∨ q"
fneu0 :: Focus
fneu0 = toFocus $ parser "True ≡ p"
fdm :: Focus
fdm = toFocus $ parser "¬(p ∨ q)"
fhole1 :: Focus
fhole1 = toFocus $ preExprHole holeInfo1
fhole0 :: Focus
fhole0 = toFocus $ preExprHole holeInfo0

-- Pruebas utilizadas en unit-test.
pFxEqY :: Proof
pFxEqY = Hole M.empty relEq fFx fy 
pFxEqZ :: Proof
pFxEqZ = Hole M.empty relEq fFx fz
pZEqY :: Proof
pZEqY = Hole M.empty relEq fz fy
pYEqZ :: Proof
pYEqZ = Hole M.empty relEq fy fz
pFxImplZ :: Proof
pFxImplZ = Hole M.empty relImpl fFx fz
pFyEqZ :: Proof
pFyEqZ = Hole M.empty relEq fFy fz

-- Información en huecos de preExpr para unit-test.
holeInfo0 :: Text
holeInfo0 = pack ""
holeInfo1:: Text
holeInfo1 = pack "p ∨ ¬q"

-- Axiomas utilizados en unit-test.
axGoldenRule :: Axiom
axGoldenRule = Axiom {
                  axName = pack "Regla Dorada"
                , axExpr = Expr $ parser "p ∧ q ≡ p ≡ q ≡ p ∨ q"
                , axRel = relEquiv
                , axRules = [ goldenRule1, goldenRule2 
                            , goldenRule3, goldenRule4
                            , goldenRule5
                            ]
                }

axNeutralEquiv :: Axiom
axNeutralEquiv = Axiom { axName = pack "Neutro de la equivalencia"
                       , axExpr = Expr $ parser "p ≡ True ≡ p"
                       , axRel = relEquiv
                       , axRules = [neuterEquiv_Rule1, neuterEquiv_Rule2]
                       }

axEquivNeg :: Axiom
axEquivNeg = Axiom { axName = pack "Negacion y Equivalencia: ¬(p ≡ q) ≡ ¬p ≡ q"
                   , axExpr = Expr $ parser "¬(p ≡ q) ≡ ¬p ≡ q"
                   , axRel = relEquiv
                   , axRules = [equivNeg_Rule1, equivNeg_Rule2]
                   }

{- Prueba de que p ⇒ p ≡ True, comenzamos con una prueba de principio y final;

    newP = newProof (≡) (p ⇒ p) (True)

nos construimos una prueba usando la regla de implicacion;

    pft = proofFromTruth (p ⇒ p) (p ∨ p ≡ p) (≡) (Ax p ⇒ q ≡ p ∨ q ≡ q)

luego añadimos un paso usando regla de implicacion;

    newStepP = addStep (toProofFocus newP) pft
    
hacemos un focus de la prueba con un nuevo paso y nos focalizamos en el hueco
por completar;

    hf = goDownR (toProofFocus newStepP)
    
completamos el hueco en la prueba usando (Ax (p ∨ p ≡ p) ≡ True);
    
    fp = fillHole hfs axImpl1
    
para finalizar nos posicionamos en el TOP de la prueba;

    finalP = toProof fp
    
Luego podemos verificar que efectivamente esta es la prueba por transitividad,
    finalP' = Trans (ctx) (p ⇒ p) (p ∨ p ≡ p) (True) pft pft' 
    donde pft' = (proofFromTruth (p ∨ p ≡ p) (True) (≡) (Ax (p ∨ p ≡ p) ≡ True))

NOTA IMPORTANTE; Pensar en generalizar proofFromTruth para que se pueda llevar
    el contexto de un hueco.
-}

-- Expresiones a utilizar para generar la prueba.
fpip0 :: Focus
fpip0 = toFocus $ parser "p ⇒ p"
fpip1 :: Focus
fpip1 = toFocus $ parser "p ∨ p ≡ p"
ftrue :: Focus
ftrue = toFocus $ parser "True"

-- Comienzo una prueba de que, p ⇒ p ≡ True
newP :: Proof
newP = newProof Nothing relEquiv fpip0 ftrue

-- Generamos una prueba simple de que, (p ⇒ p) ≡ (p ∨ p ≡ p)
pft :: Proof
Right pft = proofFromTruth fpip0 fpip1 relEquiv axImpl id

-- Añadimos un paso a la prueba, usando lo probado anteriormente.
newStepP :: Proof
Right newStepP = addStep (toProofFocus newP) pft

-- Navegamos la prueba hasta ubicarnos en el hueco por completar.
hf :: ProofFocus
Just hf = goDownR (toProofFocus newStepP)

-- Completamos el hueco de la prueba con (Ax (p ∨ p ≡ p) ≡ True)
finalPF :: ProofFocus
Right finalPF = fillHole hf axIdemPotOr

-- Volvemos al Top de la prueba.
finalP :: Proof
finalP = toProof finalPF

axImpl :: Axiom
axImpl = Axiom { axName = pack "Regla implicacion"
               , axExpr = Expr $ parser "p ⇒ q ≡ p ∨ q ≡ q"
               , axRel = relEquiv
               , axRules = [implRule1, implRule2]
               }

axIdemPotOr :: Axiom
axIdemPotOr = Axiom { axName = pack "Idempotencia del ∨"
                    , axExpr = Expr $ parser "(p ∨ p ≡ p) ≡ True"
                    , axRel = relEquiv
                    , axRules = [idempotOr_Rule1]
                    }

-- Prueba simple (p ⇒ p) ≡ (p ∨ p ≡ p)
ppip01 :: Proof
ppip01 = Simple M.empty relEquiv fpip0 fpip1 (Ax axImpl)

-- Prueba simple (p ∨ p ≡ p) ≡ True
ppip12 :: Proof
ppip12 = Simple M.empty relEquiv fpip1 ftrue (Ax axIdemPotOr)

-- Prueba trans (p ⇒ p) ≡ (p ∨ p ≡ p) ≡ True
pPImplP :: Proof
pPImplP = Trans M.empty relEquiv fpip0 fpip1 ftrue ppip01 ppip12

-- Comenzar una prueba simple con principio y final, agregar un paso usando
-- una prueba simple y llenar el hueco con otra prueba simple.
testCaseProof0 :: Assertion
testCaseProof0 = unless (finalP == pPImplP) $ 
                 assertFailure $ 
                 "FinalP: " ++ show finalP ++
                 "PImplP: " ++ show pPImplP

-- Pruebas usadas en unit-test.
pGr0 :: Proof
pGr0 = Simple M.empty relEquiv fgr0 fgr1 (Ax axGoldenRule)

pNeu0 :: Proof
pNeu0 = Simple M.empty relEquiv fp fneu0 (Ax axNeutralEquiv)

pEquivNeg :: Proof
pEquivNeg = Simple M.empty relEquiv fequivNeg0 fq (Ax axEquivNeg)

{- Prueba usando regla dorada.
    ((p)∧(q),Top)
Equiv {Ax "Regla Dorada": ((((p)∧(q))≡(p))≡(q))≡((p)∨(q))}
    (((p)≡(q))≡((p)∨(q)),Top)
-}
testCaseProofFromAxiom0 :: Assertion
testCaseProofFromAxiom0 = testCaseProofFromTruth fgr0 fgr1 relEquiv 
                                                 axGoldenRule (Right pGr0)

{- Prueba usando neutro de la equivalencia.
    (p,Top)
Equiv {Ax "Neutro de la equivalencia": ((p)≡(True))≡(p)}
    ((True)≡(p),Top)
-}
testCaseProofFromAxiom1 :: Assertion
testCaseProofFromAxiom1 = testCaseProofFromTruth fp fneu0 relEquiv 
                                                 axNeutralEquiv (Right pNeu0)


{- Prueba usando la definición de concatenar (Caso inductivo).
    ((x ▹ xs) ++ ys,Top)
Equiv {Ax "Definición de Concatenar (++) CI": (x ▹ xs) ++ ys = x ▹ (xs ++ ys)}
    (x ▹ (xs ++ ys),Top)
-}
testCaseProofFromAxiom2 :: Assertion
testCaseProofFromAxiom2 = testCaseProofFromTruth fequivNeg0 fq relEquiv
                                                axEquivNeg (Right pEquivNeg)

-- Verificar casos de tests para pruebas con axiomas y teoremas.
testCaseProofFromTruth :: (Truth t) => Focus -> Focus -> Relation -> t
                          -> PM Proof -> Assertion
testCaseProofFromTruth f f' r t res = let p = proofFromTruth f f' r t id
                                    in unless (p == res) $
                                        assertFailure $ 
                                        "\n Resultado esperado: " ++ show res ++
                                        "\n Contra: " ++ show p

{- Comenzar una prueba con la siguiente forma;
    (F (x),Top)
Eq  {?}
    (y, Top)
-}
testCaseNewProof0 :: Assertion
testCaseNewProof0 = testCaseNewProof relEq fFx fy 
                        (Hole M.empty relEq fFx fy)

{- Comenzar una prueba con la siguiente forma;
    〈∀ x : R@x ∨ S@x : T@x〉
Equiv  {?}
    〈∀ x : R@x : T@x〉 ∧ 〈∀ x : S@x : T@x〉
-}
testCaseNewProof1 :: Assertion
testCaseNewProof1 = testCaseNewProof relEquiv fQ0 fQ1 
                        (Hole M.empty relEquiv fQ0 fQ1)

{- Comenzar una prueba con la siguiente forma;
    p ∨ q
Equiv  {?}
    p ∨ ¬q ≡ p
-}
testCaseNewProof2 :: Assertion
testCaseNewProof2 = testCaseNewProof relEquiv fstar0 fstar1 
                        (Hole M.empty relEquiv fstar0 fstar1)

-- Verificar casos de tests para el comienzo de pruebas sin huecos de preExpr.
testCaseNewProof :: Relation -> Focus -> Focus -> Proof -> Assertion
testCaseNewProof r f f' res = let p = newProof Nothing r f f'
                              in unless (p == res) $
                                  assertFailure $ 
                                  "\n Resultado esperado: " ++ show res ++
                                  "\n Contra: " ++ show p

{- Comenzar una prueba con la siguiente forma;
    p ∨ q
Equiv  {?}
    ?{p ∨ ¬q}
-}
testCasenewProofWE0 :: Assertion
testCasenewProofWE0 = testCasenewProofWE relEquiv fstar0 holeInfo1
                            (Hole M.empty relEquiv fstar0 fhole1)

{- Comenzar una prueba con la siguiente forma;
    ¬(p ∨ q)
Impli  {?}
    ?
-}
testCasenewProofWE1 :: Assertion
testCasenewProofWE1 = testCasenewProofWE relImpl fdm holeInfo0
                            (Hole M.empty relImpl fdm fhole0)

-- Verificar casos de tests para el comienzo de una prueba sin final, es decir,
-- una prueba en la que tenemos un hueco de preExpr como expresion final de
-- la prueba.
testCasenewProofWE :: Relation -> Focus -> HoleInfo -> Proof -> Assertion
testCasenewProofWE r f hi res = let p = newProofWithoutEnd r f hi
                                     in unless (p == res) $
                                         assertFailure $ 
                                         "\n Resultado esperado: " ++ show res ++
                                         "\n Contra: " ++ show p

{- Agregamos un paso en el cual las prueba coinciden en su primer focus.

pFxEqY:
    (F (x),Top)
Eq  {?}
    (y, Top)

pFxEqZ:
    (F (x),Top)
Eq  {...}
    (z, Top)

Res:
    (F (x),Top)
Eq {...}
    (z,Top)
Eq {?}
    (y,Top)
-}
testCaseAddStep0 :: Assertion
testCaseAddStep0 = testCaseAddStep pFxEqY pFxEqZ 
                   (Right $ Trans M.empty relEq fFx fz fy pFxEqZ pZEqY)

{- Agregamos un paso en el cual las prueba coinciden en su ultimo y primer focus.
pFxEqY:
    (F (x),Top)
Eq  {?}
    (y, Top)

pYEqZ:
    (y,Top)
Eq  {...}
    (z, Top)

Res:
    (F (x),Top)
Eq {...}
    (y,Top)
Eq {?}
    (z,Top)
-}
testCaseAddStep1 :: Assertion
testCaseAddStep1 = testCaseAddStep pFxEqY pYEqZ 
                   (Right $ Trans M.empty relEq fFx fy fz pFxEqY pYEqZ)


{- Intentanmos agregar un paso en el que no coinciden las relaciones, luego
    entonces deberiamos devolver ClashRel.
pFxEqY:
    (F (x),Top)
Eq  {?}
    (y, Top)

pFxImplZ:
    (F (x),Top)
Impl  {...}
    (z, Top)

Res: ClashRelation eq impl
-}
testCaseAddStep2 :: Assertion
testCaseAddStep2 = testCaseAddStep pFxEqY pFxImplZ 
                   (Left $ ProofError (ClashRel relEq relImpl) id)

{- Intentamos agregar un paso en el que no coinciden los focus de las pruebas,
    luego entonces deberiamos devolver ClashAddStep.
pFxEqY:
    (F (x),Top)
Eq  {?}
    (y, Top)

pFyEqZ:
    (F (y),Top)
Impl  {...}
    (z, Top)

Res: ClashAddStep pFxEqY pFyEqZ
-}
testCaseAddStep3 :: Assertion
testCaseAddStep3 = testCaseAddStep pFxEqY pFyEqZ 
                   (Left $ ProofError (ClashAddStep pFxEqY pFyEqZ) id)

-- Verifica los casos de test para, dada una prueba, agregar un paso, es decir
-- otra prueba y generar una nueva prueba por transitividad.
testCaseAddStep :: Proof -> Proof -> PM Proof -> Assertion
testCaseAddStep pf p res = let p' = addStep (toProofFocus pf) p
                           in unless (p' == res) $
                                assertFailure $ 
                                "\n Resultado esperado: " ++ show res ++
                                "\n Contra : " ++ show p

-- Conjunto de tests para pruebas.
testGroupProof :: Test
testGroupProof = testGroup "Proof" 
                 [ testGroup "*New proof"
                    [ testCase ("newProof Eq F(x) y => \n" ++
                                    "\t\t(F (x),Top) \n" ++
                                "\tEq  {?}\n" ++
                                    "\t\t(y, Top)")
                        testCaseNewProof0
                    , testCase ("newProof \" Partición de rango \" => \n" ++
                                "\t\t〈∀ x : R@x ∨ S@x : T@x〉\n" ++
                                "\tEquiv  {?}\n" ++
                                "\t\t〈∀ x : R@x : T@x〉 ∧ 〈∀ x : S@x : T@x〉")
                        testCaseNewProof1
                    , testCase ("newProof \" Teorema estrella \" => \n" ++
                                "\t\tp ∨ q\n" ++
                                "\tEquiv  {?}\n" ++
                                "\t\tp ∨ ¬q ≡ p")
                        testCaseNewProof2
                    ]
                 ,
                   testGroup "*New proof without end"
                    [ testCase ("newProofWithoutEnd \" Teorema estrella \" " ++
                                    "=> \n\t\tp ∨ q \n" ++
                                "\tEquiv  {?}\n" ++
                                    "\t\t?{p ∨ ¬q}")
                        testCasenewProofWE0
                    , testCase ("newProofWithoutEnd \" De morgan \" " ++
                                    "=> \n\t\t¬(p ∨ q) \n" ++
                                "\tEquiv  {?}\n" ++
                                    "\t\t?{}")
                        testCasenewProofWE1
                    ]
                 , testGroup "*Add step"
                    [ testCase ("addStep [(F (x),Top) Eq{?} (y, Top)]" ++
                                      " [(F (x),Top) Eq{...} (z, Top)] =>" ++
                                      "\n\t\t(F (x),Top)" ++
                                      "\n\tEq {...}" ++
                                      "\n\t\t(z,Top)" ++
                                      "\n\tEq {?}" ++
                                      "\n\t\t(y,Top)"
                                      )
                        testCaseAddStep0
                    , testCase ("addStep [(F (x),Top) Eq{?} (y, Top)]" ++
                                      " [(y,Top) Eq{...} (z, Top)] =>" ++
                                      "\n\t\t(F (x),Top)" ++
                                      "\n\tEq {...}" ++
                                      "\n\t\t(y,Top)" ++
                                      "\n\tEq {?}" ++
                                      "\n\t\t(z,Top)"
                                      )
                        testCaseAddStep1
                    , testCase ("addStep [(F (x),Top) Eq{?} (y, Top)]" ++
                                      " [(F (x),Top) Impl{...} (z, Top)] =>" ++
                                      "\n\t ClashRel Eq Impl"
                                      )
                        testCaseAddStep2
                    , testCase ("addStep [(F (x),Top) Eq{?} (y, Top)]" ++
                                      " [(F (y),Top) Eq{...} (z, Top)] =>" ++
                                      "\n\t ClashAddStep" ++
                                      " [(F (x),Top) Eq{?} (y, Top)]" ++
                                      " [(F (y),Top) Eq{...} (z, Top)]"
                                      )
                        testCaseAddStep3
                    ]
                 , testGroup "*Proof from truth"
                    [ testCase ("proofFromTruth [p ∧ q] [p ≡ q ≡ p ∨ q]" ++
                                      " Equiv goldenRule =>" ++
                                      "\n\t\t(p ∧ q, Top) " ++
                                      "\n\t Equiv {Ax \"Regla Dorada\":" ++
                                      " p ∧ q ≡ p ≡ q ≡ p ∨ q}" ++
                                      "\n\t\t(p ≡ q ≡ p ∨ q, Top)"
                                      )
                        testCaseProofFromAxiom0
                    , testCase ("proofFromTruth [p] [p ≡ True]" ++
                            " Equiv neutralEquiv =>" ++
                            "\n\t\t(p, Top) " ++
                            "\n\t Equiv {Ax \"Neutro de la equivalencia\":" ++
                            " p ≡ True ≡ p}" ++
                            "\n\t\t(True ≡ p, Top)"
                               )
                        testCaseProofFromAxiom1
                    , testCase ("proofFromTruth [¬(p ≡ q) ≡ ¬p] [q]" ++
                        " Equiv equivNeg =>" ++
                        "\n\t\t(¬(p ≡ q) ≡ ¬p, Top) " ++
                        "\n\t Equiv {Ax \"Negacion y Equivalencia\":"++
                        " ¬(p ≡ q) ≡ ¬p ≡ q}" ++
                        "\n\t\t(q, Top)"
                            )
                        testCaseProofFromAxiom2
                    ]
                , testGroup "*Test's combinando funciones de prueba."
                    [ testCase ("Comenzar una prueba con principio y final, " ++
                                "agregar un paso usando una prueba simple " ++
                                "por axioma y \n\t luego completar el hueco " ++
                                "restante usando una prueba simple.") 
                        testCaseProof0
                    ]
                 , testGroup "Serialización"
                    [testProperty "decode . encode == id" 
                        prop_serialization
                    ]
                 ]
