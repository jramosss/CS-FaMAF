{-# Language GADTs, TypeSynonymInstances,OverloadedStrings,FlexibleInstances #-}

{-| Este m&#243;dulo define la noci&#243;n de una prueba. -}
module Equ.Proof.Proof (
                  -- * Axiomas y teoremas
                 Basic(..)
                 , Axiom(..)
                 , Theorem(..)
                 , Truth(..)
                 , Hypothesis(..)
                 , EvalStep(..)
                 , Name
                 -- * Pruebas
                 -- $proofs
                 , Proof
                 , Proof'(..)
                 , Ctx 
                 -- * Ejemplos
                 -- $samples
                 , isHole
                 -- Proyecciones
                 , getCtx, getStart, getEnd, getRel, getBasic
                 , updateStart, updateEnd, updateRel, updateMiddle, updateBasic
                 , updThmExp, updThmPrf
                 , encode, decode
                 , setCtx, beginCtx, freshName, addCtx, addCtxJust
                 , addHypothesis
                 , addHypothesisProof
                 , getHypothesis
                 , exprIsHypothesis 
                 , addHypothesis'
                 , printProof
                 , conditionFunction
                 , getGenConditions
                 , instanciateInCtx
                 , printPf
                 ) where

import Equ.Expr
import Equ.PreExpr
import Equ.Rule
import Equ.TypeChecker (checkPreExpr)
import Equ.Types
import Equ.Proof.Condition
import Equ.Matching(VariableRename)

import qualified Equ.Theories.Common as C (equal,folFalse)
 
import Data.Text (Text, unpack,pack)
import qualified Data.Text as T
-- import Data.List (intersperse,map)
import qualified Data.Set as Set

import qualified Data.Map as M ( Map, findMax, null, fromList
                               , insert, lookup, empty, elems, singleton
                               , map, union)

import Data.Monoid
import Data.Maybe
import Data.Serialize(Serialize, get, getWord8, put, putWord8, encode)

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Test.QuickCheck

type Name = Text

-- | Comienza un contexto en base a una preExpresion.
beginCtx :: Ctx
beginCtx = M.empty

-- | Retorna un nombre fresco sobre un contexto.
freshName :: Ctx -> Name
freshName c = if M.null c then "a" else T.concat $ maxName:["a"]
    where maxName :: Name
          maxName = (fst . M.findMax) c 

getHypothesis :: Name -> Ctx -> Maybe Hypothesis
getHypothesis = M.lookup

exprIsHypothesis :: Expr -> Ctx -> Bool
exprIsHypothesis e = (e `elem`) . map hypExpr . M.elems

-- instance Arbitrary Name where
--     arbitrary = Index <$> arbitrary

-- | La clase Truth representa una verdad en una teoría. En principio
-- pensamos en Axiomas y Teoremas.
class Truth t where
    truthName  :: t -> Text
    truthExpr  :: t -> Expr
    truthRel   :: t -> Relation
    truthRules :: t -> [Rule]
    truthConditions :: t -> Condition
    truthBasic :: t -> Basic
        
-- | Un axioma es una expresi&#243;n que puede ser interpretada como varias
-- reglas de re-escritura.
data Axiom = Axiom {
      axName  :: !Text 
    , axExpr  :: !Expr
    , axRel   :: !Relation
    , axRules :: [Rule]
    -- Condicion para aplicar un axioma. Es un predicado al que le pasamos como
    -- parametro la substitucion que se realiza al aplicar un axioma, y verifica
    -- que las expresiones cumplan alguna propiedad.
    , axCondition :: Condition
    }
    deriving Eq


instance Show Axiom where
    show  = unpack . axName
    
instance Arbitrary Axiom where
   arbitrary = Axiom <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Serialize Axiom where
   put (Axiom n e r lru _) = put n >> put e >> put r >> put lru
   get = Axiom <$> get <*> get <*> get <*> get <*> get

-- | Instancia de Truth para el tipo Axiom.
instance Truth Axiom where
    truthName  = axName
    truthExpr  = axExpr
    truthRel   = axRel
    truthRules = axRules
    truthBasic = Ax
    truthConditions = axCondition

-- |  Un   teorema  tambi&#233;n  permite,  como   un  axioma,  re-escribir
-- expresiones; a  diferencia de un  axioma debe tener una  prueba que
-- demuestre su validez.
data Theorem = Theorem {
      thName  :: Text
    , thExpr  :: Expr
    , thRel   :: Relation
    , thProof :: Proof
    , thRules :: [Rule]
    , thCondition :: Condition
    }
    deriving Eq

updThmExp :: PreExpr -> Theorem -> Theorem
updThmExp e t = t { thExpr = Expr e }

updThmPrf :: Proof -> Theorem -> Theorem
updThmPrf p t = t {thProof = p }

data EvalStep = EvConst 
              | EvFun 
              | EvVar 
              | EvUnary Operator
              | EvBinary Operator
              | IfTrue
              | IfFalse
              | EvApp
              | EvCase
                deriving Eq

instance Show EvalStep where
    show EvConst = "Constante"
    show EvFun = "Función"
    show EvVar = "Variable"
    show (EvUnary op) = "Definición de operador " ++ show op ++ " "
    show (EvBinary op) = "Definición de operador " ++ show op ++ " "
    show IfTrue = "Guarda verdadera"
    show IfFalse = "Guarda falsa"
    show EvApp = "Aplicación de función"
    show EvCase = "Definición por casos"

instance Truth EvalStep where
    truthName = pack . show
    truthExpr = const . Expr . PrExHole $ hole ""
    truthRel = const relEval
    truthRules = const []
    truthBasic = Evaluation
    truthConditions = const (GenConditions [])

instance Serialize EvalStep where
    put EvConst = putWord8 0
    put EvFun = putWord8 1
    put EvVar = putWord8 2
    put (EvUnary o) = putWord8 3 >> put o
    put (EvBinary o)= putWord8 4 >> put o
    put IfTrue = putWord8 5
    put IfFalse = putWord8 6
    put EvApp = putWord8 7
    put EvCase = putWord8 8
    
    get = getWord8 >>= \tag_ ->
          case tag_ of
            0 -> return EvConst
            1 -> return EvFun
            2 -> return EvVar
            3 -> EvUnary <$> get
            4 -> EvBinary <$> get
            5 -> return IfTrue
            6 -> return IfFalse
            7 -> return EvApp
            8 -> return EvCase
            _ -> fail $ "SerializeErr Basic " ++ show tag_


instance Show Theorem where
    show th = (unpack . thName) th ++ ": " ++ (show . thExpr) th

instance Arbitrary Theorem where
    arbitrary = Theorem <$> arbitrary <*> arbitrary <*> 
                            arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Serialize Theorem where
    put (Theorem n e r p lru conds) = put n >> put e >> put r >> put p >> put lru
                                      >> put conds
    get = Theorem <$> get <*> get <*> get <*> get <*> get <*> get

-- | Instancia de Truth para el tipo theorem.
instance Truth Theorem where
    truthName  = thName
    truthExpr  = thExpr
    truthRel   = thRel
    truthRules = thRules
    truthBasic = Theo
    truthConditions _ = GenConditions []

data Hypothesis = Hypothesis {
     hypName :: Text
   , hypExpr :: Expr
   , hypRel  :: Relation
   , hypRule :: [Rule]
   , hypCondition :: Condition
}

instance Eq Hypothesis where
    h1 == h2 = hypExpr h1 == hypExpr h2 && hypRel h1 == hypRel h2 && hypRule h1 == hypRule h2

instance Show Hypothesis where
    show hyp = show (hypName hyp) ++ ":" ++ show (hypExpr hyp)

instance Truth Hypothesis where
    truthName = hypName
    truthExpr = hypExpr
    truthRel  = hypRel
    truthRules = hypRule
    truthBasic = Hyp . hypName
    truthConditions = hypCondition

instance Arbitrary Hypothesis where
    arbitrary = Hypothesis <$> arbitrary <*> arbitrary <*> arbitrary <*> 
                               arbitrary <*> arbitrary
                            
instance Serialize Hypothesis where
    put (Hypothesis t e r ru _) = put t >> put e >> put r >> put ru
    get = Hypothesis <$> get <*> get <*> get <*> get <*> get
    
-- | El contexto lleva las hip&#243;tesis actuales; en nuestro caso hay
-- tres formas de agregar hip&#243;tesis al contexto: en una prueba por
-- casos hay nuevas igualdades; en una prueba por inducci&#243;n hay
-- hip&#243;tesis inductivas y en una prueba que usa el metateorema de la
-- deducci&#243;n asumimos el antecedente de una implicaci&#243;n.

type Ctx = M.Map Name Hypothesis


instance Arbitrary Ctx where
    arbitrary = M.fromList <$> arbitrary

-- | Las pruebas elementales son aplicar un axioma (en un foco), 
-- usar un teorema ya probado, o usar una hip&#243;tesis.
data Basic where
    Ax  :: Axiom -> Basic        -- Un axioma de cierta teor&#237;a.
    Theo :: Theorem -> Basic     -- Un teorema ya probado.
    Hyp :: Name -> Basic   -- Una hip&#243;tesis que aparece en el contexto.
    Evaluate :: Basic           -- Evaluacion interna para aritmética
    Evaluation ::  EvalStep -> Basic
           deriving Eq
    
instance Truth Basic where
    truthName (Ax a) = axName a
    truthName (Theo t) = thName t
    truthName (Hyp h) = h
    truthName Evaluate = "Evaluar"
    truthName (Evaluation ev) = pack $ show ev

    truthExpr (Ax a) = axExpr a
    truthExpr (Theo t) = thExpr t
    truthExpr (Hyp _) = undefined
    truthExpr Evaluate = C.equal (varNat "x") (varNat "x")
    truthExpr (Evaluation _) = error ""

    truthRel (Ax a) = axRel a
    truthRel (Theo t) = thRel t
    truthRel (Hyp _) = undefined
    truthRel Evaluate = relEq
    truthRel (Evaluation _) = relEval

    truthRules (Ax a) = axRules a
    truthRules (Theo t) = thRules t
    truthRules (Hyp _) = undefined
    truthRules Evaluate = []
    truthRules (Evaluation _) = []
    
    truthConditions (Ax a) = axCondition a
    truthConditions (Theo t) = thCondition t
    truthConditions (Hyp _) = undefined
    truthConditions Evaluate = GenConditions []
    truthConditions (Evaluation _) = GenConditions []

    truthBasic b = b

instance Show Basic where
    show = unpack . truthName
    
-- 
instance Arbitrary Basic where
    arbitrary = 
        oneof [ Ax <$> arbitrary
              , Theo <$> arbitrary
              , Hyp <$> arbitrary
              ]
-- 
instance Serialize Basic where
    put (Ax a) = putWord8 0 >> put a
    put (Theo t) = putWord8 1 >> put t
    put (Hyp h) = putWord8 2 >> put h
    put Evaluate = putWord8 3
    put (Evaluation e) = putWord8 4 >> put e


    get = getWord8 >>= \tag_ ->
          case tag_ of
            0 -> Ax <$> get
            1 -> Theo <$> get
            2 -> Hyp <$> get
            3 -> return Evaluate
            4 -> Evaluation <$> get
            _ -> fail $ "SerializeErr Basic " ++ show tag_

{- $proofs

[@Simple@] 
  La propiedad importante es que en los pasos simples
  tenemos alguna regla asociada al axioma o al teorema usado que hace
  coincidir (salvo variables ligadas?) a la primera con la segunda
  expresi&#243;n:

Si 

@
proofAx :: Proof
proofAx = Simple ctx foc foc' $ Ax ax

proofThm :: Proof
proofThm = Simple ctx foc foc' $ Theo thm
@

entonces @ rewriteInFocus foc ax == foc' @ y @ rewriteInFocus foc thm == foc' @.

Si la prueba es simple y fue usando una hip&#243;tesis, digamos

@
proofHyp :: Proof
proofHyp = Simple ctx foc foc' $ Hyp n
@ 

entonces @length ctx > n@ y @rewriteInFocus foc (ctx!n) == foc'@.

[@Transitividad@] 

Una prueba por transitividad 

@
theoTrans :: Proof
theoTrans = Trans ctx rel fe fe1 fe2 prf prf' 
@

es una prueba de @fe rel fe2@ usando pruebas intermedias @prf@ de @fe
rel fe1@ y @prf'@ de @fe1 rel fe2@. 

[Casos]

En una prueba por casos

@
theoCases :: Proof
theoCases = Cases ctx rel fe1 fe2 c [(f1,p1),..,(fn,pn)] guardsProof
@

hacemos an&#225;lisis por casos en @c@, esto es posible dependiendo
del tipo de @c@; por ejemplo si @type c == TyConst ATyNat@, entonces
podemos introducir los casos @c == 0@, @c==1@, @c > 1@. La lista de
focos y pruebas consiste en tantas pruebas como casos se hayan
considerando; cada par @(fi,pi)@ representa una prueba de @fe1 rel
fe2@ a&#241;adiendo la hip&#243;tesis extra @fi@ en @ctx@.
guardsProof es una prueba del o de las guardas. Debe ser:
(f1 v .... v fn) equivalente True

[Inducci&#243;n]

La prueba por inducci&#243;n es similar a la prueba por casos, excepto
que en los casos inductivos podemos usar la hip&#243;tesis
inductiva. Para entender este tipo de pruebas utilicemos el siguiente
ejemplo de inducci&#243;n en los naturales:

@&#9001; &#8704; n : : &#9001; &#8721; i : 0 &#8804; i &#8804; n : i &#9002; = n * (n + 1) / 2 &#9002;@

para probarlo utilizamos inducci&#243;n en @n@ (notemos que impl&#237;citamente
estamos utilizando el meta-teorema que dice que para probar una
cuantificaci&#243;n universal podemos elegir un elemento arbitrario del
dominio cuantificado y probar la matriz de la f&#243;rmula); ahora queremos
probar 

@&#9001; &#8721; i : 0 &#8804; i &#8804; n : i &#9002; = n * (n + 1) / 2@ 

donde @n@ es ahora una variable libre. Usar inducci&#243;n en @n@ entonces implica
construir una prueba de 

@&#9001; &#8721; i : 0 &#8804; i &#8804; 0 : i &#9002; = 0 * (0 + 1) / 2@

y otra prueba de 

@&#9001; &#8721; i : 0 &#8804; i &#8804; (k+1) : i &#9002; = (k+1) * ((k+1) + 1) / 2@

con la hip&#243;tesis inductiva como una hip&#243;tesis adicional en el contexto:

@
&#9001; &#8721; i : 0 &#8804; i &#8804; k : i &#9002; = k * (k+1) / 2 .
@

El constructor @Ind@ permite pruebas por inducci&#243;n en varias
sub-expresiones @e1,...,en@; la segunda lista es la lista de pruebas
correspondiente a todos los casos de acuerdo a los tipos de las
expresiones sobre las que hacemos inducci&#243;n. Inicialmente podemos
pensar que s&#243;lo haremos inducci&#243;n en los naturales y que @[e1,...,en]@
tiene un s&#243;lo elemento: la variable sobre la que hacemos inducci&#243;n;
por lo tanto @[(fs1,p1),...,(fsm,pm)]@ ser&#237;a una lista con dos elementos
@[(n=0,proofBaseCase),(n=k+1,proofInd)]@.

@
theoInd :: Proof
theoInd = Ind ctx rel fe1 fe2 [e1,...,en] [(fs1,p1),...,(fsm,pm)]
@

[Meta-teorema de la deducci&#243;n]

El meta-teorema de la deducci&#243;n est&#225; representado por el constructor
@Deduc@ y permite construir una prueba de @p &#8658; q@ a partir de una
prueba de @q@ agregando @p@ a las hip&#243;tesis; por ejemplo, si
@proofQ :: Proof@ con @lhs proofQ == q@, @rhs proofQ == True@,
@p `in` ctx proofQ@ entonces el siguiente t&#233;rmino es una prueba
de @p &#8658; q@:

@
theoDeduc :: Proof
theoDeduc = Deduc ctx relImpl p q prf
@

[Sub-pruebas in-situ]

En general las pruebas (sean estos pasos elementales o m&#225;s
complicados) implican la prueba de una relaci&#243;n entre dos expresiones
pero donde se reescribe una sub-expresi&#243;n; esa reescritura es una
prueba sencilla necesariamente. @Focus@ permite que reescribamos una
sub-expresi&#243;n sin usar un teorema previamente demostrado sino con una
prueba in-situ. Por ejemplo, en la siguiente expresi&#243;n, @prf@ es una
prueba de @e == e'@.

@
theoFocus :: Proof
theoFocus = Focus ctx rel (e,p) (e',p') prf 
@

-}

data Proof' ctxTy relTy proofTy exprTy where
    Reflex :: Proof' ctxTy relTy proofTy exprTy
    Hole   :: ctxTy -> relTy -> exprTy -> exprTy -> Proof' ctxTy relTy proofTy exprTy
    Simple :: ctxTy -> relTy -> exprTy -> exprTy -> proofTy ->
              Proof' ctxTy relTy proofTy exprTy
    Trans  :: ctxTy -> relTy -> exprTy -> exprTy -> exprTy -> 
              Proof' ctxTy relTy proofTy exprTy -> Proof' ctxTy relTy proofTy exprTy ->
              Proof' ctxTy relTy proofTy exprTy
    -- En la prueba por casos incluimos una subprueba que demuestra el "o"
    -- de las guardas.
    Cases  :: ctxTy -> relTy -> exprTy -> exprTy -> exprTy -> 
              [(exprTy,Proof' ctxTy relTy proofTy exprTy)] -> 
              Maybe (Proof' ctxTy relTy proofTy exprTy) ->
              Proof' ctxTy relTy proofTy exprTy
    -- Haremos inducción en una sola VARIABLE. Para no modificar tanto, asumimos
    -- que la expresion donde se hace inducción es de tipo "Var a".
    Ind    :: ctxTy -> relTy -> exprTy -> exprTy -> exprTy -> 
              [(exprTy,Proof' ctxTy relTy proofTy exprTy)] -> 
              Proof' ctxTy relTy proofTy exprTy
    Deduc  :: ctxTy -> exprTy -> exprTy -> 
              Proof' ctxTy relTy proofTy exprTy -> 
              Proof' ctxTy relTy proofTy exprTy
    Focus  :: ctxTy -> relTy -> exprTy -> exprTy -> 
              Proof' ctxTy relTy proofTy exprTy ->
              Proof' ctxTy relTy proofTy exprTy

instance (Serialize ctxTy, Serialize relTy, Serialize proofTy, Serialize exprTy) => 
         Serialize (Proof' ctxTy relTy proofTy exprTy) where
    put Reflex = putWord8 0
    put (Hole ctxTy rel exprTy exprTy') = 
        putWord8 1 >> put ctxTy >> put rel >> put exprTy >> put exprTy'
    put (Simple ctxTy rel exprTy exprTy' proofTy) = 
        putWord8 2 >> put ctxTy >> put rel >> 
                      put exprTy >> put exprTy' >> 
                      put proofTy
    put (Trans ctxTy rel exprTy exprTy' exprTy'' proofTy proofTy') = 
                putWord8 3 >> put ctxTy >> put rel >> 
                              put exprTy >> put exprTy' >> put exprTy'' >>
                              put proofTy >> put proofTy'
    put (Cases ctxTy rel exprTy exprTy' exprTy'' lfproofTy proofTy) = 
                putWord8 4 >> put ctxTy >> put rel >> 
                              put exprTy >> put exprTy' >> put exprTy'' >>
                              put lfproofTy >> put proofTy
    put (Ind ctxTy rel exprTy exprTy' lf llfproofTy) = 
                putWord8 5 >> put ctxTy >> put rel >> 
                              put exprTy >> put exprTy' >>
                              put lf >> put llfproofTy
    put (Deduc ctxTy exprTy exprTy' proofTy) = 
                putWord8 6 >> put ctxTy >> put exprTy >> put exprTy' >> 
                              put proofTy
    put (Focus ctxTy rel exprTy exprTy' proofTy) = 
                putWord8 7 >> put ctxTy >> put rel >> 
                              put exprTy >> put exprTy' >> put proofTy
    
    get = getWord8 >>= \tag_ ->
        case tag_ of
        0 -> return Reflex 
        1 -> Hole <$> get <*> get <*> get <*> get
        2 -> Simple <$> get <*> get <*> get <*> get <*> get
        3 -> Trans <$> get <*> get <*> get <*> get <*> get <*> get <*> get
        4 -> Cases <$> get <*> get <*> get <*> get <*> get <*> get <*> get
        5 -> Ind  <$> get <*> get <*> get <*> get <*> get <*> get
        6 -> Deduc <$> get <*> get <*> get <*> get
        7 -> Focus <$> get <*> get <*> get <*> get <*> get
        _ -> fail $ "SerializeErr Proof' " ++ show tag_

type Proof = Proof' Ctx Relation Basic Focus


addCtxJust :: Ctx -> Proof -> Proof
addCtxJust c pr = fromJust (addCtx c pr)

-- Agregamos contexto a una prueba
addCtx :: Ctx -> Proof ->  Maybe Proof
addCtx c proof = getCtx proof >>= \ctx ->
                 addCtx' (c `M.union` ctx) proof

    where addCtx' ctx (Ind _ r f f' lf lfp) = Just (Ind ctx r f f' lf (addCtxInSubProofs ctx lfp))
          addCtx' ctx (Trans _ r f f' f'' p p') = Just (Trans ctx r f f' f'' (fromMaybe p
                                                                                  (addCtx ctx p)) 
                                                                           (fromMaybe p'
                                                                                  (addCtx ctx p')))
          addCtx' ctx (Cases _ r f f' f'' lfp p') = Just (Cases ctx r f f' f'' (addCtxInSubProofs ctx lfp) 
                                                                             (p' >>= addCtx ctx))
          addCtx' ctx p = setCtx ctx p
    
          addCtxInSubProofs ctx = map (second (fromJust . addCtx ctx))

-- Igualdad sintáctica entre pruebas. Podríamos definir también una igualdad en donde
-- si tenemos pruebas iguales salvo renombres de variables cuantificadas, tambien de True.
instance Eq Proof where
    Reflex == Reflex = True
    Hole ctx1 rel1 f1 f2 == Hole ctx2 rel2 f3 f4 = 
        ctx1==ctx2 && rel1==rel2 && f1==f3 && f2==f4
    Simple ctx1 rel1 f1 f2 basic1 == Simple ctx2 rel2 f3 f4 basic2 =
        ctx1==ctx2 && rel1==rel2 && f1==f3 && f2==f4 && basic1==basic2
    Trans ctx rel f1 f2 f3 p1 p2 == Trans ctx' rel' f1' f2' f3' p1' p2' = 
        ctx==ctx' && rel==rel' && f1==f1' && f2==f2' && f3==f3' && 
        p1==p1' && p2==p2'
    Cases ctx rel f1 f2 f cases orProof == Cases ctx' rel' f1' f2' f' cases' orProof' =
        ctx==ctx' && rel==rel' && f1==f1' && f2==f2' && f==f' &&
        cases==cases' && orProof==orProof'
    Ind ctx rel f1 f2 f patterns == Ind ctx' rel' f1' f2' f' patterns' =
        ctx==ctx' && rel==rel' && f1==f1' && f2==f2' && f==f' &&
        patterns==patterns'
    Deduc ctx f1 f2 p == Deduc ctx' f1' f2' p' =
        ctx==ctx' && f1==f1' && f2==f2' && 
        p==p'
    Focus ctx rel f1 f2 p == Focus ctx' rel' f1' f2' p' = 
        ctx==ctx' && rel==rel' && f1==f1' && f2==f2' &&
        p==p'
    _==_ = False
    
    


{-    
instance Eq Proof where
    Reflex == Reflex = True
    Reflex == _ = False
    _ == Reflex = False
    p1 == p2 = (fromJust $ getStart p1) == (fromJust $ getStart p2) &&
               (fromJust $ getEnd p1) == (fromJust $ getEnd p2)-}

               

printPf :: Proof -> [String]
printPf Reflex = [""]
printPf (Hole _ r f f') = [ showExpr' (toExpr f)
                          , show r
                          ,  showExpr' (toExpr f')]
printPf (Simple c r f f' b) = [ "Contexto: " ++ show c
                              , showExpr' (toExpr f)
                              , show r ++ " { " ++ show b ++ " }"
                              , showExpr' (toExpr f')]
printPf (Trans _ _ _ _ _ p p') = init (printPf p) ++ printPf p'
printPf (Focus _ _ f f' p) = [ showExpr' (toExpr f)
                             , last . init $ printPf p
                             , showExpr' (toExpr f')
                             ]
printPf (Deduc _ f f' p) = [ showExpr' (toExpr f), show relEquiv ]
                           ++ printPf p ++
                           [ showExpr' (toExpr f')]
printPf (Ind c r f f' f'' lfp) = [ "Induction "
                                , show c 
                                , show r ++ "(" ++ show f ++ ", " ++ show f' ++ ")" 
                                , "by induction on " ++ show f''
                                , " { " ++ show lfp ++ " } "
                                ]
printPf (Cases c r f f' f'' lfp mprf) = [ "Cases "
                                , show c 
                                , show r ++ "(" ++ show f ++ ", " ++ show f' ++ ")" 
                                , "by induction on " ++ show f''
                                , " { " ++ show lfp ++ " } "
                                , show mprf
                                ]
                                
printProof :: Proof -> String
printProof = (++"\n") . unlines . printPf
                                    
-- TODO: Completar esta funcion cuando se termine el parser de pruebas.
instance Show Proof where
    show = printProof


instance Arbitrary Proof where
    arbitrary = sized proof
        where
            proof :: Int -> Gen Proof
            proof 0 = 
                oneof [ return Reflex
                      , Hole <$> arbitrary <*> arbitrary <*> 
                                 arbitrary <*> arbitrary
                      , Simple <$> arbitrary <*> arbitrary <*> 
                                   arbitrary <*> arbitrary <*> arbitrary
                      ]
            proof n | n > 0 = 
                oneof [ return Reflex
                      , Hole <$> arbitrary <*> arbitrary <*> 
                                 arbitrary <*> arbitrary
                      , Simple <$> arbitrary <*> arbitrary <*> 
                                   arbitrary <*> arbitrary <*> arbitrary
                      , Trans <$> arbitrary <*> arbitrary <*> arbitrary <*> 
                                  arbitrary <*> arbitrary <*> 
                                  subProof <*> subProof
                      , Cases <$> arbitrary <*> arbitrary <*> arbitrary <*> 
                                  arbitrary <*> arbitrary <*> listPairFocusProof
                                  <*> arbitrary
                      , Ind <$> arbitrary <*> arbitrary <*> arbitrary <*> 
                                arbitrary <*> arbitrary <*> 
                                listPPFocusProof
                      , Deduc <$> arbitrary <*> arbitrary <*> 
                                  arbitrary <*> subProof
                      , Focus <$> arbitrary <*> arbitrary <*> 
                                  arbitrary <*> arbitrary <*> subProof
                    ]
                where   
                    -- Disminuimos el largo (visto como un arbol) de la prueba.
                    subProof :: Gen Proof
                    subProof = proof (n `div` 10)
                    pairFocusProof :: Gen (Focus, Proof)
                    pairFocusProof = (,) <$> arbitrary <*> subProof
                    listPairFocusProof :: Gen [(Focus, Proof)]
                    listPairFocusProof = vectorOf 2 pairFocusProof
                    listPPFocusProof :: Gen [(Focus, Proof)]
                    listPPFocusProof = vectorOf 2 pairFocusProof
    

instance Monoid (Proof' ctxTy relTy proofTy exprTy) where
    mempty = Reflex
    mappend Reflex p = p
    mappend p Reflex = p
    mappend p1 p2 = Trans (fromJust $ getCtx p1) (fromJust $ getRel p1) 
                          (fromJust $ getStart p1) (fromJust $ getStart p2) 
                          (fromJust $ getEnd p2) p1 p2

isHole :: Proof' ctxTy relTy proofTy exprTy -> Bool
isHole (Hole _ _ _ _) = True
isHole _ = False

getCtx :: Proof' ctxTy relTy proofTy exprTy -> Maybe ctxTy
getCtx Reflex = Nothing
getCtx (Hole c _ _ _) = Just c
getCtx (Simple c _ _ _ _) = Just c
getCtx (Trans c _ _ _ _ _ _) = Just c
getCtx (Cases c _ _ _ _ _ _) = Just c
getCtx (Ind c _ _ _ _ _) = Just c
getCtx (Deduc c _ _ _) = Just c
getCtx (Focus c _ _ _ _) = Just c

-- Esta función me hace pensar si no hara falta lo mismo para los demas
-- componentes de las pruebas.
-- | Cambiamos el contexto de una prueba.
setCtx :: ctxTy -> Proof' ctxTy relTy proofTy exprTy ->  Maybe (Proof' ctxTy relTy proofTy exprTy)
setCtx _ Reflex = Nothing
setCtx c (Hole _ r f f') = Just (Hole c r f f')
setCtx c (Simple _ r f f' b) = Just (Simple c r f f' b)
setCtx c (Trans _ r f f' f'' p p') = Just (Trans c r f f' f'' (setCtx' c p) (setCtx' c p'))
setCtx c (Cases _ r f f' f'' lfp p') = Just (Cases c r f f' f'' (setCtxInList c lfp) (setMCtx' c p'))
setCtx c (Ind _ r f f' lf lfp) = Just (Ind c r f f' lf (setCtxInList c lfp))
setCtx c (Deduc _ f f' p) = Just (Deduc c f f' (setCtx' c p)) --VER BIEN ESTE CASO
setCtx c (Focus _ r f f' p) = Just (Focus c r f f' (setCtx' c p)) --ESTE TAMBIEN

setCtx' :: ctxTy -> Proof' ctxTy relTy proofTy exprTy ->  Proof' ctxTy relTy proofTy exprTy
setCtx' c p = fromMaybe p (setCtx c p)
                       
setCtxInList :: ctxTy -> [(exprTy,Proof' ctxTy relTy proofTy exprTy)] ->  
                         [(exprTy,Proof' ctxTy relTy proofTy exprTy)]
setCtxInList c = map (second (setCtx' c)) 

setMCtx' :: ctxTy -> Maybe (Proof' ctxTy relTy proofTy exprTy) ->  
                     Maybe (Proof' ctxTy relTy proofTy exprTy)
setMCtx' c = fmap (setCtx' c)

                     
getStart :: Proof' ctxTy relTy proofTy exprTy -> Maybe exprTy
getStart Reflex = Nothing
getStart (Hole _ _ f _) = Just f
getStart (Simple _ _ f _ _) = Just f
getStart (Trans _ _ f _ _ _ _) = Just f
getStart (Cases _ _ f _ _ _ _) = Just f
getStart (Ind _ _ f _ _ _) = Just f
getStart (Deduc _ f _ _) = Just f
getStart (Focus _ _ f _ _) = Just f

getEnd :: Proof' ctxTy relTy proofTy exprTy -> Maybe exprTy
getEnd Reflex = Nothing
getEnd (Hole _ _ _ f) = Just f
getEnd (Simple _ _ _ f _) = Just f
getEnd (Trans _ _ _ _ f _ _) = Just f
getEnd (Cases _ _ _ f _ _ _) = Just f
getEnd (Ind _ _ _ f _ _) = Just f
getEnd (Deduc _ _ f _) = Just f
getEnd (Focus _ _ _ f _) = Just f

-- | Devuelve la relación para la cual es una prueba.
-- NOTA: para Deduc devolvemos Nothing ahora para que compile. En el caso concreto de "Proof" deberia devolver "relImpl", pero
-- como tenemos el tipo general Proof' no sabemos qué devolver.
getRel :: Proof' ctxTy relTy proofTy exprTy -> Maybe relTy
getRel Reflex = Nothing
getRel (Hole _ r _ _) = Just r
getRel (Simple _ r _ _ _) = Just r
getRel (Trans _ r _ _ _ _ _) = Just r
getRel (Cases _ r _ _ _ _ _) = Just r
getRel (Ind _ r _ _ _ _) = Just r
getRel (Deduc _ _ _ _) = Nothing
getRel (Focus _ r _ _ _) = Just r

getBasic:: Proof' ctxTy relTy proofTy exprTy -> Maybe proofTy
getBasic (Simple _ _ _ _ b) = Just b
getBasic _ = Nothing

updateStart :: Proof' ctxTy relTy proofTy exprTy -> exprTy -> Proof' ctxTy relTy proofTy exprTy
updateStart Reflex _ = Reflex
updateStart (Hole c r _ f2) f = Hole c r f f2
updateStart (Simple c r _ f2 b) f = Simple c r f f2 b
updateStart (Trans c r _ fm f2 p p') f = Trans c r f fm f2 (updateStart p f) p'
updateStart (Cases c r _ f2 fc list p') f = Cases c r f f2 fc list p'
updateStart (Ind c r _ f2 l1 l2) f = Ind c r f f2 l1 l2
updateStart (Deduc c _ f2 p) f = Deduc c f f2 p
updateStart (Focus c r _ f2 p) f = Focus c r f f2 p

updateEnd :: Proof' ctxTy relTy proofTy exprTy -> exprTy -> Proof' ctxTy relTy proofTy exprTy
updateEnd Reflex _ = Reflex
updateEnd (Hole c r f1 _) f = Hole c r f1 f
updateEnd (Simple c r f1 _ b) f = Simple c r f1 f b
updateEnd (Trans c r f1 fm _ p p') f = Trans c r f1 fm f p (updateEnd p' f)
updateEnd (Cases c r f1 _ fc list p') f = Cases c r f1 f fc list p'
updateEnd (Ind c r f1 _ l1 l2) f = Ind c r f1 f l1 l2
updateEnd (Deduc c f1 _ p) f = Deduc c f1 f p
updateEnd (Focus c r f1 _ p) f = Focus c r f1 f p

updateMiddle :: Proof' ctxTy relTy proofTy exprTy -> exprTy -> Proof' ctxTy relTy proofTy exprTy
updateMiddle (Trans c r f1 _ f2 p p') f = Trans c r f1 f f2 (updateEnd p f) (updateStart p' f)
updateMiddle _ _ = undefined

updateRel :: Proof' ctxTy relTy proofTy exprTy -> relTy -> Proof' ctxTy relTy proofTy exprTy
updateRel Reflex _ = Reflex
updateRel (Hole c _ f1 f2) r = Hole c r f1 f2
updateRel (Simple c _ f1 f2 b) r = Simple c r f1 f2 b
updateRel (Trans c _ f1 fm f2 p p') r = Trans c r f1 fm f2 p p'
updateRel (Cases c _ f1 f2 fc list p') r = Cases c r f1 f2 fc list p'
updateRel (Ind c _ f1 f2 l1 l2) r = Ind c r f1 f2 l1 l2
updateRel (Deduc c f1 f2 p) _ = Deduc c f1 f2 p
updateRel (Focus c _ f1 f2 p) r = Focus c r f1 f2 p

updateBasic :: Proof' ctxTy relTy proofTy exprTy -> proofTy -> Proof' ctxTy relTy proofTy exprTy
updateBasic (Simple c r f1 f2 _) b = Simple c r f1 f2 b
updateBasic p _ = p

{- $samples

[Axiomas]

-}
-- neutralEquiv :: Axiom
-- neutralEquiv = Axiom { axName = pack "Neutro de la equivalencia"
--                      , axExpr = Expr $ parser "(p ≡ True) ≡ p"
--                      , axRel = relEquiv
--                      , axRules = [neuterEquiv_Rule1]
--                      }
-- {-
-- 
-- [Pruebas]
-- 
-- @
-- incomplete :: Proof
-- incomplete = Hole M.empty Equivalence (Top,equiv True True) (Top,True)
-- @
-- 
-- -}
-- 
-- trivial :: Proof
-- trivial = Simple empty relEquiv (toFocus reflTrue) (toFocus true') $
--           Ax neutralEquiv
--     where (Expr true') = true
--           (Expr reflTrue) = true `equiv` true
-- 
-- trivialBack :: Proof
-- trivialBack = Simple empty relEquiv (toFocus true') (toFocus reflTrue) $
--               Ax neutralEquiv
--     where (Expr true') = true
--           (Expr reflTrue) = true `equiv` true
-- 
-- trivial' :: Proof
-- trivial' = Simple empty relEquiv foc foc $ Ax neutralEquiv
--     where (Expr true') = true
--           foc = toFocus true'
-- 
-- 
-- -- | Ejemplo, muy pavo, de prueba usando transitividad.
-- transEx :: Proof
-- transEx = Trans empty relEquiv foc (toFocus true') foc trivial trivialBack
--     where (Expr reflTrue) = true `equiv` true
--           (Expr true') = true
--           foc = toFocus reflTrue
-- 
-- 
-- -- | Ejemplo, muy pavo, de prueba usando transitividad con un hueco.
-- transExHole :: Proof
-- transExHole = Trans empty relEquiv foc foc' foc trivial step
--     where (Expr reflTrue) = true `equiv` true
--           (Expr true') = true
--           foc = toFocus reflTrue
--           foc' = toFocus true'
--           hole = Hole empty relEquiv foc' foc'
--           step = Trans empty relEquiv foc' foc' foc hole trivialBack


-- | Dada una expresión; chequeamos que el tipo sea bool y la agregamos
-- al contexto como una hipótesis. Esto genera dos reglas: @e ≡ True@ y
-- @True ≡ e@. Si su tipo no es bool se devuelve el mismo contexto.
addHypothesis :: PreExpr -> Relation -> [PreExpr] -> Ctx -> (Ctx,Maybe Name)
addHypothesis expr rel exprs c = case checkPreExpr expr of
                                   Left _ -> (c, Nothing)
                                   Right ty -> if and . map (either (const False) (==ty) . checkPreExpr) $ exprs 
                                              then  (M.insert n hyp c,Just n)
                                              else  (c, Nothing)
    where n = freshName c
          hyp = Hypothesis {
                  hypName = ""
                , hypExpr = Expr expr
                , hypRel  = rel
                , hypRule = map (rule . Expr) exprs
                , hypCondition = GenConditions []
                }
          rule ex' = mkrule (Expr expr) ex' rel


-- | Agrega una hipotesis al contexto de una prueba
addHypothesisProof :: PreExpr -> Relation -> [PreExpr] -> Proof -> Maybe Proof
addHypothesisProof e r es pf = getCtx pf >>=
                               return . addHypothesis e r es >>= \(ctx',_) ->
                               setCtx ctx' pf


addHypothesis' :: Hypothesis -> Ctx -> Ctx
addHypothesis' hyp ctx = M.insert (hypName hyp) hyp ctx
                          
varNat :: Name -> Expr                       
varNat s = Expr $ Var $ var s (TyAtom ATyNat)

instanciateInCtx :: Ctx -> Variable -> Focus -> Ctx
instanciateInCtx ctx v p = M.map (replaceHypothesis v p) ctx            

replaceHypothesis :: Variable -> Focus -> Hypothesis -> Hypothesis
replaceHypothesis v p' hyp =
              let (Expr peHyp) = hypExpr hyp in
                  hyp { hypExpr = Expr $ applySubst peHyp (M.singleton v (toExpr p')) }


conditionFunction :: GCondition -> (ExprSubst,VariableRename) -> PreExpr -> Bool
conditionFunction (VarNotInExpr v p) (subst,_) _ = not $ Set.member v (freeVars $ applySubst p subst)
    
conditionFunction (InductiveHypothesis pattern) (subst,_) _ =
    -- En la hipótesis inductiva, solo podemos validar la reecritura si
    -- la expresión por la q se quiere reemplazar la variable inductiva
    -- es la misma. Ejemplo: si la HI es "x es par", solo podemos aplicarla
    -- a la expresión "x es par" y no a "(x+1) es par". Por eso pedimos que 
    -- la substitución de reescritura asigne x -> x.
    case pattern of
         Var v -> applySubst (Var v) subst == Var v
         _ -> False
conditionFunction (NotEmptyRange ptn) (subst,_) _ = applySubst ptn subst /= Con C.folFalse 
                          
{- q es igual a p donde reemplazamos x por n.
   Esta condicion se utiliza en cuantificadores. x es la variable cuantificada.
   Al chequear el reemplazo en expresiones, vemos el renombre que hizo el matching
   a la variable cuantificada. -}
conditionFunction (ReplacedExpr q p x n) (subst,rnm) _ =
            maybe False (\v -> 
            applySubst q subst == applySubst (applySubst p subst) 
                                        (M.singleton v (applySubst (n' rnm) subst))
            ) $ M.lookup x rnm
        where n' = applySubst n . M.map Var
            
getGenConditions :: Condition -> [GCondition]
getGenConditions (GenConditions lc) = lc
getGenConditions _ = error "getGenConditions: Condición especial!"
   
