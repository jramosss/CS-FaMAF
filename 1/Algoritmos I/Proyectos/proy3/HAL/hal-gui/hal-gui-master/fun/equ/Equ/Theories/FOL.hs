-- | El m&#243;dulo de expresiones de f&#243;rmulas de primer orden.
{-# Language OverloadedStrings #-}
module Equ.Theories.FOL 
    ( -- * Constructores y operadores.
      folTrue , folFalse , folForall , folExist , folEqual
    , folEquiv, folDiscrep, folAnd, folOr, folNeg, folImpl, folConseq
    -- ** Listas de constructores y operadores
    , theoryConstantsList
    , theoryOperatorsList
    , theoryQuantifiersList
    -- ** Lista de axiomas de la teoria
    , assocEquivAx
    , theoryAxiomList
    -- * Versión tipada de operadores.
    , true, false, equal, equiv, discrep
    , neg, and, or, impl, conseq, forAll, exist
    -- ** Cuantificador \"Existe\"
    -- *** Definicion
    )
    where

import Prelude hiding (and,or) 
import Equ.Syntax
import Equ.Types
import Equ.Expr
import Equ.PreExpr
import Equ.Rule
import Equ.Proof.Proof
import Equ.Proof.Condition
import Equ.Theories.AbsName
import Equ.Theories.Common

import Data.Text(Text)

-- CONSTANTES
-- CUANTIFICADORES
-- | Cuantificador &#8704;
folForall :: Quantifier
folForall = Quantifier { quantRepr = "∀"
                       , quantName = Forall
                       , quantTy = tyVar "A" :-> tyBool
                       }

-- | Cuantificador &#8707;
folExist :: Quantifier
folExist = Quantifier { quantRepr = "∃"
                      , quantName = Exist
                      , quantTy = tyVar "A" :-> tyBool
                      }

-- OPERACIONES

-- Tipo de las operaciones logicas
folUnOpType,folBinOpType :: Type
folUnOpType = tyBool :-> tyBool
folBinOpType = tyBool :-> tyBool :-> tyBool

                    
-- | Discrepancia /&#8801;
folDiscrep :: Operator
folDiscrep = Operator { opRepr = "/≡"
                      , opName = Discrep
                      , opTy = folBinOpType
                      , opAssoc = ALeft
                      , opNotationTy = NInfix
                      , opPrec = 1
                      , opGlyphs = ["/="]
                      }

-- LOS OPERADORES AND Y OR SE ENCUENTRAN DEFINIDOS EN COMMON
                      
     
-- | Negacion &#172;
folNeg :: Operator
folNeg = Operator { opRepr = "¬"
                  , opName = Neg
                  , opTy = folUnOpType
                  , opAssoc = None
                  , opNotationTy = NPrefix
                  , opPrec = 4
                  , opGlyphs = []
                  }

-- | Implicaci&#243;n &#8658;
folImpl :: Operator
folImpl = Operator { opRepr = "⇒"
                   , opName = Implic
                   , opTy = folBinOpType
                   , opAssoc = ARight
                   , opNotationTy = NInfix
                   , opPrec = 2
                   , opGlyphs = ["=>"]
                   }

-- | Consecuencia &#8656;
folConseq :: Operator
folConseq = Operator { opRepr = "⇐"
                     , opName = Conseq
                     , opTy = folBinOpType
                     , opAssoc = ALeft
                     , opNotationTy = NInfix
                     , opPrec = 2
                     , opGlyphs = ["<="]
                     }

-- | Constantes de FOL.
theoryConstantsList :: [Constant]
theoryConstantsList = [folTrue,folFalse]

-- | Operadores de FOL.
theoryOperatorsList :: [Operator]
theoryOperatorsList = [ folEqual,folEquiv
                      , lessOper, lessOrEqOper 
                      , folDiscrep,folAnd,folOr
                      , folImpl,folConseq,folNeg
                      ]

-- | Cuantificadores de FOL.
theoryQuantifiersList :: [Quantifier]
theoryQuantifiersList = [folForall,folExist]

theoryAxiomList :: [(Text,Expr,Condition)]
theoryAxiomList = [ conmEquivAxiom
                  , trueLNeutralEquiv
                  , trueRNeutralEquiv
                  , negEquivAxiom
                  , falseDefAxiom
                  , discrepDefAxiom
                  , assocOrAxiom
                  , commOrAxiom
                  , idempotOrAxiom
                  , distEqOrAxiom
                  , excludThirdAxiom
                  , goldenRuleAxiom
                  , defImplAxiom
                  , trueLNeutralAnd
                  , trueRNeutralAnd
                  , leftWeak
                  , rightWeak
                  -- CUANTIFICADORES
                  , emptyRangeForAll
                  , unitRangeForAll
                  , partRangeForAll
                  , termRuleForAll
                  , constTermForAll
                  , distLeftOrForAll
                  , distRightOrForAll
                  , nestedRuleForAll
                  , changeVarForAll
                  , interRangeTermForallAxiom
                  , distAndForAll
                  , interQuantForAll
                  , existDef
                  , magicAxiomEqual
                  , magicAxiomEquiv
                  , conmutEqual
                  , reflexEqual
                  , reindAxiom
                  , termSepForAll1
                  , termSepForAllLast
                  ]
 

-- A continuacion definimos constructores de expresiones, para su facil manejo

-- | Constructores de Operaciones l&#243;gicas


-- | Discrepancia
discrep :: Expr -> Expr -> Expr
discrep (Expr p) (Expr q) = Expr $ BinOp folDiscrep p q

-- | Negacion
neg :: Expr -> Expr
neg (Expr p) = Expr $ UnOp folNeg p

-- | Implicacion
impl :: Expr -> Expr -> Expr
impl (Expr p) (Expr q) = Expr $ BinOp folImpl p q

-- | Consecuencia
conseq :: Expr -> Expr -> Expr
conseq (Expr p) (Expr q) = Expr $ BinOp folConseq p q

-- Constructor de para todo:

forAll :: Variable -> Expr -> Expr -> Expr
forAll v (Expr r) (Expr t) = Expr $ Quant folForall v r t

-- Constructor del existe:
exist :: Variable -> Expr -> Expr -> Expr
exist v (Expr r) (Expr t) = Expr $ Quant folExist v r t

-- AXIOMAS DEL CALCULO PROPOSICIONAL
-- Los axiomas del calculo proposicional son Expresiones dentro de Eq

-- Variables a usar en las reglas:
varP,varQ,varR,varT :: Expr
varP= Expr $ Var $ var "p" tyBool
varQ= Expr $ Var $ var "q" tyBool
varR= Expr $ Var $ var "r" tyBool
varT= Expr $ Var $ var "t" tyBool


-- Variable para usar en las cuantificaciones en las reglas.
-- VER: Qué tipo le ponemos a la variable cuantificada????
varX :: Variable
varX= var "x" (tyVar "A")
varI :: Variable
varI= var "i" (tyVar "A")
varY :: Variable
varY= var "y" (tyVar "A")

-- Expresion para igualar con la variable cuantificada
varN :: Expr
varN = Expr $ Var $ var "n" (tyVar "A")
varM :: Expr
varM = Expr $ Var $ var "m" (tyVar "A")

-- ============
-- EQUIVALENCIA
-- ============

-- Ascociatividad: ((p &#8801; q) &#8801; r) &#8801; (p &#8801; (q &#8801; r))
-- VER CUANTAS SON LAS REGLAS QUE HAY QUE HACER PARA ESTE AXIOMA.
-- Aca hay solo dos opciones, el equivalente del medio es siempre el de "relacion".
-- Las dos formas posibles son conmutar ambos miembros.

-- assocEquivAx :: (Text,Expr)
-- assocEquivAx = ("Asociatividad de la Equivalencia", 
--                 associativityEquiv equiv varP varQ varR)


-- | Asociatividad de la equivalencia; este axioma no puede
-- generarse de la misma manera que los demás.
assocEquivAx :: Axiom
assocEquivAx = Axiom {  axName = "Asociatividad de la Equivalencia"
                      , axExpr = expr
                      , axRel = relEquiv
                      , axRules = map mkr [(lhs',rhs'),(rhs',lhs'),(expr,true),(true,expr)]
                      , axCondition = noCondition
                     }
    where lhs' = (varP `equiv` varR) `equiv` varQ
          rhs' = varP `equiv` (varR `equiv` varQ)
          mkr (l,r) = mkrule l r relEquiv
          expr = associativityEquiv equiv varP varQ varR

-- Axioma Conmutatividad de la equivalencia:
conmEquivAxiom :: (Text,Expr,Condition)
conmEquivAxiom = ("Conmutatividad de la Equivalencia", 
                  symmetryEquiv equiv varP varQ,
                  noCondition)

trueLNeutralEquiv :: (Text,Expr,Condition)
trueLNeutralEquiv = ("Neutro de la equivalencia a izquierda", 
                     leftNeutralEquiv equiv true varP,
                     noCondition)

trueRNeutralEquiv :: (Text,Expr,Condition)
trueRNeutralEquiv = ("Neutro de la equivalencia a derecha", 
                     rightNeutralEquiv equiv true varP,
                     noCondition)


-- =========
-- NEGACION
-- =========

negEquivAxiom :: (Text,Expr,Condition)
negEquivAxiom = ("Negación y Equivalencia", 
                 neg (varP `equiv` varQ) `equiv` ((neg varP) `equiv` varQ),
                 noCondition)

{- | Definicion de false:
@
    False &#8801; &#172;True
@
-}
falseDefAxiom :: (Text,Expr,Condition)
falseDefAxiom = ("Definición de False",false `equiv` neg true,noCondition)
                  

-- ============
-- DISCREPANCIA
-- ============

{- | Definicion de discrepancia:
@
    (p /&#8801; q) &#8801; &#172;(p &#8801; q)
@
-}
discrepDefAxiom :: (Text,Expr,Condition)
discrepDefAxiom = ("Definición de discrepancia",  
                   (discrep varP varQ) `equiv` (neg (equiv varP varQ)),
                   noCondition)
                    
-- ===========
-- DISYUNCION
-- ===========

{- | Regla asociatividad:
@
    (p &#8744; q) &#8744; r &#8801; p &#8744; (q &#8744; r)
@
-}
assocOrAxiom :: (Text,Expr,Condition)
assocOrAxiom = ("Asociatividad del ∨", 
                associativityEquiv or varP varR varQ,
                noCondition)
                    
{- | Regla conmutatividad:
@
    p &#8744; q &#8801; q &#8744; p
@
-}
                  
commOrAxiom :: (Text,Expr,Condition)
commOrAxiom = ("Conmutatividad del ∨", symmetryEquiv or varP varQ,noCondition)

{- | Regla idempotencia:
@
    p &#8744; p &#8801; p
@
-}

idempotOrAxiom :: (Text,Expr,Condition)
idempotOrAxiom = ("Idempotencia del ∨", varP `or` varP `equiv` varP,noCondition)

                      
distEqOrAxiom :: (Text,Expr,Condition)
distEqOrAxiom = ("Distributividad de ∨ con ≡"
                , equiv (or varP (equiv varQ varR)) (equiv (or varP varQ) (or varP varR))
                , noCondition)


{- | Tercero Excluido:
@
    p &#8744; &#172;p
@
-}
excludThirdAxiom :: (Text,Expr,Condition)
excludThirdAxiom = ("Tercero excluido", equiv (or varP (neg varP)) true,noCondition)

-- ===========
-- CONJUNCION
-- ===========

goldenRuleAxiom :: (Text,Expr,Condition)
goldenRuleAxiom = ( "Regla Dorada"
                  , ((varP `and` varQ)  `equiv` varP) `equiv` (varQ `equiv` (varP `or` varQ))
                  , noCondition)


trueLNeutralAnd :: (Text,Expr,Condition)
trueLNeutralAnd = ( "Neutro a izquierda de ∧", leftNeutral and true varP, noCondition)

trueRNeutralAnd :: (Text,Expr,Condition)
trueRNeutralAnd = ( "Neutro a derecha de ∧", rightNeutral and true varP, noCondition)
                   
-- ===========
-- IMPLICACION
-- ===========

-- ------------------------------
-- Definicion de &#8658;: p &#8658; q &#8801; p &#8744; q &#8801; q
-- ------------------------------

defImplAxiom :: (Text,Expr,Condition)
defImplAxiom = ( "Definición del Implica"
               , (impl varP varQ) `equiv` ((or (neg varP) varQ) `equiv` varQ)
               , noCondition
               )

leftWeak :: (Text,Expr,Condition)
leftWeak = ( "Debilitamiento a izquierda", ((varP `and` varQ) `impl` varP) , noCondition)

rightWeak :: (Text,Expr,Condition)
rightWeak = ( "Debilitamiento a derecha", (((varP `and` varQ) `impl` varQ)) `equiv` true , noCondition)
                 
-- ===========
-- CONSECUENCIA
-- ===========
-- TODO!!

-- AXIOMAS PARA LOS CUANTIFICADORES

-- ===========
-- PARA TODO
-- ===========
          
-- Definicion de Axiomas generales:

emptyRangeForAll :: (Text,Expr,Condition)
emptyRangeForAll = 
    ( "Rango Vacío Para Todo"
    , emptyRange forAll equiv varX varP true
    , noCondition
    )
    
unitRangeForAll :: (Text,Expr,Condition)
unitRangeForAll =
    ( "Rango Unitario Para Todo"
    , unitRange forAll equiv varI varN varP varQ
    , GenConditions [ReplacedExpr peVarQ peVarP varI peVarN]
    )
    where Expr peVarP = varP
          Expr peVarN = varN
          Expr peVarQ = varQ
    
partRangeForAll :: (Text,Expr,Condition)
partRangeForAll =
    ( "Partición de Rango Para Todo"
    , partRange forAll equiv and varX varP varQ varR
    , noCondition
    )
    
termRuleForAll :: (Text,Expr,Condition)
termRuleForAll =
    ( "Regla del Término Para Todo"
    , termRule forAll equiv and varX varR varP varQ
    , noCondition
    )
    
constTermForAll :: (Text,Expr,Condition)
constTermForAll =
    ( "Regla del Término Constante Para Todo"
    , constTermRule forAll equiv varX varR varP
    , GenConditions [VarNotInExpr varX peVarP,NotEmptyRange peVarR]
    )
    where Expr peVarP = varP
          Expr peVarR = varR
    
distLeftOrForAll :: (Text,Expr,Condition)
distLeftOrForAll =
    ( "Distributividad a izquierda del o y Para Todo"
    , distLeftQuant forAll equiv or varX varR varP varQ
    , GenConditions [VarNotInExpr varX peVarP]
    )
    where Expr peVarP = varP
    
distRightOrForAll :: (Text,Expr,Condition)
distRightOrForAll =
    ( "Distributividad a derecha del o y Para Todo"
    , distRightQuant forAll equiv or varX varR varP varQ
    , GenConditions [VarNotInExpr varX peVarP]
    )
    where Expr peVarP = varP
    
nestedRuleForAll :: (Text,Expr,Condition)
nestedRuleForAll =
    ( "Regla de Anidado Para Todo"
    , nestedRule forAll equiv varX varY varR varP varQ
    , GenConditions [VarNotInExpr varY peVarR]
    )
    where Expr peVarR = varR
    
changeVarForAll :: (Text,Expr,Condition)
changeVarForAll =
    ( "Regla de Cambio de Variable Para Todo"
    , changeVar forAll equiv varX varY varR varP varQ varT
    , GenConditions [ReplacedExpr peVarQ peVarR varX (Var varY),
                     ReplacedExpr peVarT peVarP varX (Var varY),
                     VarNotInExpr varY peVarR,
                     VarNotInExpr varY peVarP]
    )
    where Expr peVarR = varR
          Expr peVarP = varP
          Expr peVarQ = varQ
          Expr peVarT = varT
    
-- | Reindizado
reindAxiom :: (Text,Expr,Condition)
reindAxiom =
    ( "Reindizado Para Todo"
    , reindex forAll equiv varNatCuant varNat1 varNat2 varP varQ
    , GenConditions [ReplacedExpr peVarQ peVarP varNatCuant succVarCuant]
    )
    
    where varNatCuant = var "i" (TyAtom ATyNat)
          varNat1 = Expr $ Var $ var "x" (TyAtom ATyNat)
          varNat2 = Expr $ Var $ var "y" (TyAtom ATyNat)
          Expr peVarP = varP
          Expr peVarQ = varQ
          Expr succVarCuant = successor $ Expr $ Var varNatCuant
    
-- Axiomas particulares del Para Todo
    
interRangeTermForallAxiom :: (Text,Expr,Condition)
interRangeTermForallAxiom = 
    ( "Intercambio entre rango y término"
    , (forAll varX varP varQ) `equiv` (forAll varX true (impl varP varQ))
    , noCondition
    )
    
distAndForAll :: (Text,Expr,Condition)
distAndForAll =
    ( "Distributividad de y con Para todo"
    , (and (forAll varX true varP) (forAll varX true varQ)) `equiv` 
          (forAll varX true (and varP varQ))
    , noCondition
    )
    
interQuantForAll :: (Text,Expr,Condition)
interQuantForAll =
    ( "Intercambio de cuantificadores Para Todo"
    , (forAll varX true (forAll varY true varP)) `equiv`
      (forAll varY true (forAll varX true varP))
    , noCondition
    )
    
termSepForAll1 :: (Text,Expr,Condition)
termSepForAll1 =
    ( "Separación del primer término Para Todo"
    , termSep1 forAll equiv and varI m n varP varQ varR
    , GenConditions [ReplacedExpr peVarQ peVarP varI peM,
                     ReplacedExpr peVarR peVarP varI succI]
    )
    where m = Expr $ Var $ var "m" (TyAtom ATyNat)
          n = Expr $ Var $ var "n" (TyAtom ATyNat)
          Expr succI = successor $ Expr $ Var $ varI
          Expr peVarQ = varQ
          Expr peVarP = varP
          Expr peVarR = varR
          Expr peM = m
    
termSepForAllLast :: (Text,Expr,Condition)
termSepForAllLast =
    ( "Separación del último término Para Todo"
    , termSepLast forAll equiv and varI m n varP varQ
    , GenConditions [ReplacedExpr peVarQ peVarP varI peN]
    )
    where m = Expr $ Var $ var "m" (TyAtom ATyNat)
          n = Expr $ Var $ var "n" (TyAtom ATyNat)
          Expr peVarQ = varQ
          Expr peVarP = varP
          Expr peN = n

-- ===========
-- EXISTE
-- ===========

-- | Definición de Existe

existDef :: (Text,Expr,Condition)
existDef =
    ( "Definición de Existe"
    , (exist varX varR varP) `equiv` (neg (forAll varX varR $ neg varP))
    , noCondition
    )
    
    
magicAxiomEquiv :: (Text,Expr,Condition)
magicAxiomEquiv =
    ( "Harry Potter Equivalencia"
    , varP `equiv` varQ
    , noCondition
    )
    
magicAxiomEqual :: (Text,Expr,Condition)
magicAxiomEqual =
    ( "Harry Potter Igualdad"
    , varP `equal` varQ
    , noCondition
    )
    
    
-- Igualdad

-- | Conmutatividad
conmutEqual :: (Text,Expr,Condition)
conmutEqual =
    ( "Conmutatividad de ="
    , (equal varN varM) `equiv` (equal varM varN)
    , noCondition
    )
    
-- | Reflexividad 
reflexEqual :: (Text,Expr,Condition)
reflexEqual =
    ( "Reflexividad de ="
    , (varN `equal` varN) `equiv` true
    , noCondition
    )

-- {- | Definicion de Existe:
-- @
--     <&#8707;x : r.x : f.x> &#8801; &#172; <&#8704;x : r.x : &#172;f.x>
-- @
-- -}
-- existRule :: Rule
-- existRule = Rule { lhs = exist varX range term
--                  , rhs = neg $ forAll varX range (neg term)
--                  , rel = relEquiv
--                  , name = ""
--                  , desc = ""
--                  }
--     where varX = var "x" $ tyVar "A"
--           range = Expr $ Var $ var "r" $ tyBool
--           term = Expr $ Var $ var "t" $ tyBool



