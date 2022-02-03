{-# Language GADTs,OverloadedStrings #-}

{-| Este m&#243;dulo define la noci&#243;n de una prueba. -}
module Equ.Proof ( newProof
                 , newProofWithCases
                 , newProofWithStart 
                 , newProofWithoutEnd
                 , proofFromTruth
                 , holeProof, emptyProof, updateStart, updateEnd, updateRel, updateBasic
                 , validateProof, toHoleProof, validateProofFocus, validateListedProof
                 , validateStepProof
                 , simpleProof, addEmptyStep
                 , possibleExpr
                 , beginCtx
                 , getExprProof
                 , Truth (..)
                  -- * Axiomas y teoremas
                 , Axiom(..)
                 , Theorem(..)
                 , Basic(..)
                 , Hypothesis
                 , EvalStep(..)
                 -- * Pruebas
                 -- $proofs
                 , Proof
                 , Proof'(..)
                 , ProofFocusAnnots
                 , ProofAnnotation
                 , emptyProofAnnots
                 , addEmptyStepAnnots
                 --, Basic(..)
                 -- * Ejemplos
                 -- $samples
                 , module Equ.Proof.Zipper
                 , module Equ.Proof.Monad
                 , module Equ.Proof.Error
                 , module Equ.Rewrite
                 , module Equ.Proof.ListedProof
                 , module Equ.Proof.Condition
                 -- * Funciones auxiliares
                 , addHypothesis
                 , addHypotheses
                 , addHypothesis'
                 , getHypothesis
                 , exprIsHypothesis 
                 , addBoolHypothesis
                 , Name
                 , Ctx
                 , printProof
                 , isBoolean
                 ) where

import Equ.Proof.Annot
import Equ.Proof.Proof hiding (getCtx,getStart,getEnd,getRel,setCtx)
import qualified Equ.Proof.Proof as P(getStart,getEnd,getRel)
import Equ.Proof.Zipper
import Equ.Proof.Monad
import Equ.Proof.Error
import Equ.Proof.ListedProof
import Equ.Proof.Condition
import Equ.Theories.Common hiding (and)
import Equ.TypeChecker(typeCheckPreExpr)
import Equ.Syntax hiding (Hole)

import qualified Equ.PreExpr as PE
import Equ.Expr
import Equ.PreExpr.Eval (evalExpr)
import Equ.Rule hiding (rel)
import Equ.Rewrite
import Equ.Matching

import Equ.TypeChecker (checkPreExpr)
import Equ.IndType
import Equ.IndTypes
import Equ.Theories

import Equ.Proof.Induction(createIndHypothesis)

import Data.Monoid(mappend)

import Data.Maybe
import Data.Either (partitionEithers,rights)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Function (on)
import Control.Arrow((&&&),(***))
import Control.Applicative ((<$>))


-- | Funciones auxiliares que podrían ir a su propio módulo.
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- | Determina si dos expresiones son iguales al evaluar
-- todas las expresiones aritméticas.
checkEval :: PE.Focus -> PE.Focus -> Bool
checkEval = (==) `on` (evalExpr . PE.toExpr)


-- Funcion para checkear igualdad, con la variante importante que en caso de
-- no cumplirse devolvemos un resultado por default.
checkEqWithDefault :: Eq a => ProofError -> a -> a -> PM ()
checkEqWithDefault def a b | a /= b = Left def
                           | otherwise = return ()

whenEqWithDefault :: Eq a => ProofError -> a -> a -> PM a
whenEqWithDefault def a b = whenPM (==a) def b >> return b

checkSimpleStepFromRule :: Truth t => PE.Focus -> PE.Focus -> Relation -> t -> 
                           Rule -> (ProofFocus -> ProofFocus) -> PM ()
checkSimpleStepFromRule f1 f2 _ t rule move = do
      let   fs1 = PE.toFocuses e1
            fs2 = PE.toFocuses $ PE.toExpr f2
            Expr leftE = lhs rule
            Expr rightE = rhs rule
            fs1' = filter (isRight . match leftE . fst) fs1
            fs2' = filter (isRight . match rightE . fst) fs2
            es = concatMap (flip map fs2') (map (pegar (truthRel t)) fs1')
            (_,res) = partitionEithers $ map lastMatch es
            res' = filter ((==PE.toExpr f2) . PE.toExpr . replaceExpr) res
            res'' = filter checkConditions res'
      whenPM' (not $ null res'') err
        
    where
        e1 = PE.toExpr f1
        err :: ProofError
        err = ProofError (BasicNotApplicable $ truthBasic t) move
        pegar :: Relation -> PE.Focus -> PE.Focus -> (PE.PreExpr,PE.Focus)
        pegar rel f1'@(e1',_) (e2,_) = (mkPreExprFromRel rel e1' e2,f1')
        replaceExpr :: (PE.PreExpr,PE.Focus,(PE.ExprSubst,VariableRename)) -> PE.Focus
        replaceExpr (PE.BinOp _ _ f2',focus,_) = PE.replace focus f2'
        replaceExpr _ = error "La expresion pegada no puede ser distinta de BinOp"
        lastMatch :: (PE.PreExpr,PE.Focus) -> Either (MatchMErr,PE.Log) (PE.PreExpr,PE.Focus,(PE.ExprSubst,VariableRename))
        lastMatch (expr,f) = match (ruleExpr rule) expr >>= \subst ->
                             return (expr,f,subst)
        checkConditions :: (PE.PreExpr,PE.Focus,(PE.ExprSubst,VariableRename)) -> Bool
        checkConditions (_,_,subst) = all (\check -> check subst e1) condPreds
            where condPreds = map conditionFunction (getGenConditions $ truthConditions t)

        

{- 
Funciones para construir y manipular pruebas.
Este kit de funciones deber&#237;a proveer todas las herramientas
necesarias para desarrollar pruebas en equ.
-}
proofFromRule :: Truth t => PE.Focus -> PE.Focus -> Relation -> t ->
                            Rule -> (ProofFocus -> ProofFocus) -> PM Proof
proofFromRule f1 f2 rel t r fMove = 
                        checkSimpleStepFromRule f1 f2 rel t r fMove >>
                        (return $ Simple beginCtx rel f1 f2 $ truthBasic t)

proofFromRuleWithHyp :: (Truth t, Truth t') => Ctx -> PE.Focus -> PE.Focus -> Relation -> t -> t' ->
                            Rule -> (ProofFocus -> ProofFocus) -> PM Proof
proofFromRuleWithHyp ctx f1 f2 rel t b r fMove = 
                        checkSimpleStepFromRule f1 f2 rel b r fMove >>
                        (return $ Simple ctx rel f1 f2 $ truthBasic t)

-- | Dados dos focuses f1 y f2, una relacion rel y un axioma o
-- teorema, intenta crear una prueba para f1 rel f2, utilizando el
-- paso simple de aplicar el axioma o teorema.
proofFromTruth :: Ctx -> PE.Focus -> PE.Focus -> Relation -> Basic -> 
                  (ProofFocus -> ProofFocus) -> PM Proof
proofFromTruth ctx f f' r basic fMove = 
    case basic of
        Evaluate -> 
                if checkEval f f' 
                then Right $ Simple beginCtx r f f' Evaluate
                else Left $ ProofError (BasicNotApplicable Evaluate) fMove
        Hyp n -> maybe (Left $ ProofError (BasicNotApplicable $ Hyp n) fMove) 
                       (\b -> proofFromTruth' (flip (simplesWithHyp b) fMove) b)
                       $ Map.lookup n ctx
        _ -> proofFromTruth' (flip simples fMove) basic

    where simples = proofFromRule f f' r basic
          simplesWithHyp = proofFromRuleWithHyp ctx f f' r basic

    
proofFromTruth' :: Truth t => (Rule -> Either a b) -> t -> Either a b
proofFromTruth' fValidateRule basic =
    case partitionEithers $ map fValidateRule (truthRules basic) of
        ([],[]) -> Left undefined -- TODO: FIX THIS CASE!
        (_, p:_) -> Right p
        (er, []) -> Left $ head er
    
validateProofFocus :: ProofFocus -> PM Proof
validateProofFocus = validateProof . fst

-- | Valida una prueba completa vista como lista
validateListedProof :: ListedProof -> PM Proof
validateListedProof lProof = validateProof (toProof $ pFocus lProof)

-- | Valida el paso de una prueba vista como lista.
validateStepProof :: ListedProof -> PM Proof
validateStepProof lProof = let p = (pList lProof)!!(selIndex lProof) 
                           in validateProof p
                          
validateProof :: Proof -> PM Proof
validateProof p =  validateProof' p goTop'
                          
validateProof' :: Proof -> (ProofFocus -> ProofFocus) -> PM Proof
validateProof' (Hole _ _ _ _) moveFocus = Left $ ProofError HoleProof moveFocus
validateProof' (Simple ctx rel f1 f2 b) moveFocus = proofFromTruth ctx f1 f2 rel b moveFocus
validateProof' proof@(Trans _ _ f1 f f2 p1 p2) moveFocus = 
    getStart p1 >>= whenEqWithDefault err f1 >>
    getEnd p1 >>= whenEqWithDefault err f >>
    getStart p2 >>= whenEqWithDefault err f >>
    getEnd p2 >>= whenEqWithDefault err f2 >>
    validateProof' p1 (goDownL' . moveFocus) >>= \p1' ->
    validateProof' p2 (goDownR' . moveFocus) >>= \p2' ->
    return (mappend p1' p2')
    
    where err :: ProofError
          err = ProofError (TransInconsistent proof) moveFocus
    
validateProof' proof@(Deduc _ p q prf) mvFocus = 
    getEnd prf >>= whenEqWithDefault err q >>
    case (addHypothesisProof (PE.toExpr p) relEquiv [true'] prf) of
      Nothing -> Left err
      Just prf' -> validateProof' prf' mvFocus >> return proof
    where err = ProofError DeducInvalidEnd mvFocus
          Expr true' = true
                    
          
-- TODO: ver que las variables de un pattern sean frescas respecto a
-- las expresiones de la prueba, donde reemplazamos la variable
-- inductiva por una constante.
validateProof' proof@(Ind _ rel f1 f2 e ps) _ =
    either (const $  Left errProof) (return . PE.toFocus) 
        (typeCheckPreExpr (PE.toExpr f1)) >>= \typedF1 ->
    -- Primero verificamos que e sea una variable:
    PE.isVar errIndVar (PE.toExpr e) >>= \x ->
    -- Luego vemos que la variable esté en la expresión f1
    whenPM' (Set.member x (PE.freeVars (PE.toExpr typedF1))) errIndVar >>
    -- Chequeamos los casos de inducción:
    checkPatterns x proof ps >>
    return proof
    
    where errIndVar = errProofPlace $ InductionError VarIndNotInExpr
          checkPatterns :: Variable -> Proof -> [(PE.Focus,Proof)] -> PM ()
          checkPatterns x pr ps' = getIndType' x >>= \it ->
                                  -- chequeamos las variables de cada pattern
                                  mapM_ (checkVarsPattern x pr) (map fst basePat) >>
                                  mapM_ (checkVarsPattern x pr) (map fst indPat) >>
                                  checkExpr x (PE.toExpr f2) >>
                                  checkAllConstants it constPat >>
                                  checkAllBaseConst it basePat >>
                                  checkAllIndConst it indPat
             where indPatterns@(constPat,basePat,indPat) = splitByConst tyVarInd $ map (PE.toExpr *** id) ps'
                   tyVarInd = varTy x
                   -- chequeamos que los casos del case sean los mismos que en la prueba inductiva.

                   checkCase :: [(PE.PreExpr,PE.PreExpr)] -> PM ()
                   checkCase css = cheqEqCases indPatterns casePatterns >>
                                   checkSubProofCase x pr (joinPatterns constPat constCase) >>
                                   checkSubProofCase x pr (joinPatterns basePat baseCase) >>
                                   checkSubProofIndCase x pr (joinPatterns indPat indCase)
                       where casePatterns@(constCase,baseCase,indCase) = splitByConst tyVarInd css


                  -- Si la expresion final es un case, la prueba es diferente:
                   -- cada subprueba se debe corresponder con cada case.

                   checkExpr :: Variable -> PE.PreExpr -> PM ()
                   checkExpr v (PE.Case c cases) = checkCaseVar v c >> checkCase cases
                   checkExpr v _ = checkSubProof v pr constPat >>
                                   checkSubProof v pr basePat >>
                                   checkSubProofInd v pr indPat

          -- chequeamos que la variable inductiva  sea donde se aplica el Cases
          checkCaseVar :: Variable -> PE.PreExpr -> PM ()
          checkCaseVar v (PE.Var v') = whenPM' (v'==v) errCase
          checkCaseVar _ _ = Left errCase
          
          errCase = errProofPlace $ InductionError VarExprCases



          -- Esta funcion chequea las pruebas de cada tipo de patrón base
          -- (no aplica hipótesis inductiva).
          checkSubProof x pr = mapM_ go
              where go (expr,p) = 
                            -- Chequeamos que la prueba p demuestra lo mismo que pr, pero
                            -- aplicando la substitucion correspondiente
                            sameProofWithSubst p pr (Map.singleton x expr) >>
                            sameCtxs pr p >>
                            -- Finalmente validamos la prueba p y continuamos chequeando.
                            validateProof' p id
                             
          
          checkSubProofInd x pr cs = Map.elems <$> getCtx proof >>= \hypsProof ->
                                     mapM_ (go hypsProof) cs
              where go hprf (expr,p) = 
                        let hyps = createIndHypothesis rel f1 f2 (PE.toFocus expr) x "hypind" in
                        -- Chequeamos que cada hipótesis del contexto de la subprueba
                        -- esté en el contexto de la prueba inductiva, o sea hipótesis
                        -- inductiva.
                        getCtx p >>= \ctx' ->
                        return (setCtx' (replaceVacuousHyp hyps ctx') p) >>= \p' ->
                        Map.elems <$> getCtx p' >>= \hypsSubProof -> 
                        allElems (hprf ++ hyps) hypsSubProof errSubProof >>
                        -- Ahora vemos que la subprueba prueba lo mismo que la prueba inductiva,
                        -- pero reemplazando la variable por el pattern correspondiente.
                        sameProofWithSubst p' pr (Map.singleton x expr) >>
                        validateProof' p' id
                    errSubProof = errProofPlace $ InductionError SubProofHypothesis

          
          -- | Toma la lista de patterns de la prueba y de la expresion case y devuelve
          --   una sola lista con el pattern, la prueba y la expresion del case.
          --   Asume que las dos listas tienen la misma longitud y los patterns de ambas
          --   listas son iguales.
          joinPatterns :: [(PE.PreExpr,a)] -> [(PE.PreExpr,b)] -> [(PE.PreExpr,a,b)]
          joinPatterns = zipWith (\a b -> (fst a,snd a, snd b))
          
          checkSubProofCase :: Variable -> Proof -> [(PE.PreExpr,Proof,PE.PreExpr)] -> PM ()
          checkSubProofCase v pr cs = mapM_ go cs 
              where go (pattern,subp,expr) = 
                        correctSubProofCase v pattern pr subp expr >>
                        getCtx subp >>= \ctxSub ->
                        getCtx pr >>= \ctxPr ->
                        whenPM' (ctxSub == ctxPr) (errProofPlace ContextsNotEqual) >>
                        validateProof' subp id
                
                        
          checkSubProofIndCase :: Variable -> Proof -> [(PE.PreExpr,Proof,PE.PreExpr)] -> PM ()
          checkSubProofIndCase v pr cs = 
              Map.elems <$> getCtx proof >>= \hypsProof ->
              mapM_ (go hypsProof) cs
              where errSubPrfHyp = errProofPlace $ InductionError SubProofHypothesis
                    go hprf (pattern,subp,expr) = 
                        let hyps = createIndHypothesis rel f1 f2 (PE.toFocus pattern) v "hypind" in
                        -- Chequeamos que cada hipótesis del contexto de la subprueba
                        -- esté en el contexto de la prueba inductiva, o sea hipótesis
                        -- inductiva.
                        Map.elems <$> getCtx subp >>= \hypsSubProof -> 
                        allElems (hprf ++ hyps) hypsSubProof errSubPrfHyp >>
                        correctSubProofCase v pattern pr subp expr >>
                        validateProof' subp id
              
          correctSubProofCase :: Variable -> PE.PreExpr -> Proof -> Proof -> 
                                 PE.PreExpr -> PM ()
          correctSubProofCase v pattern pr subp expr =
                getStart pr >>= \stPr -> 
                getStart subp >>= \stSubp ->
                getEnd subp >>= \endSubp -> 
                (return $ PE.applySubst (PE.toExpr stPr) (Map.singleton v pattern)) >>= \stReplaced ->
                getRel pr >>= \relPr -> getRel subp >>= \relSubp ->
                whenPM' (PE.toExpr stSubp==stReplaced && 
                         PE.toExpr endSubp==expr && relSubp==relPr)
                        (errProofPlace $ InductionError IncorrectSubProof)
              
              
-- | Validación de una prueba por casos. La prueba de exahustividad de las guardas
--   es opcional. Ver como podemos dar un mensaje Warning en ese caso.
validateProof' proof@(Cases _ _ _ _ _ cases mGuardsProof) _ = 
    -- Primero chequeamos la prueba que dice que el "o" de las guardas vale:
       maybe (return ()) checkGuardsProof mGuardsProof >>
    -- Ahora debemos chequear cada prueba de casos. Cada subprueba debe tener
    -- la misma relacion, expresion inicial y final que la prueba general, solo
    -- que agrega al contexto la hipótesis correspondiente al caso, por ejemplo
    -- si el caso es "e==0", luego se agrega "e==0 ≡ True" como hipótesis.
       foldl (>>) (return ()) (map (checkSubProofCases proof) cases) >>
       return proof

    
    
    -- Chequeamos la prueba que demuestra que el "o" de todas las guardas es True.
    -- Para eso, esta prueba debe tener el mismo contexto que la prueba general.
    -- debe tener como relación a la equivalencia, la expresión inicial debe ser
    -- el o de todas las guardas (VER QUE PASA SI ES LA MISMA EXPRESION PERO CON CONMUTATIVIDAD
    -- Y ASOCIATIVIDAD DISTINTOS) y la expresión final debe ser True.
    where checkGuardsProof guardsProof=
            getRel guardsProof >>= \relGP -> getStart guardsProof >>= \stGP -> 
            getEnd guardsProof >>= \endGP -> return (map fst cases) >>= \cs -> 
            orCasesExpr cs >>= \orCE -> 
            sameCtxs guardsProof proof >>
            whenPM' (relGP==relEquiv) errProof >> 
            whenPM' (stGP==orCE) errProof >>
            whenPM' (endGP==(PE.toFocus $ PE.Con folTrue)) errProof >>
            validateProof' guardsProof id >>
            return ()
            
          orCasesExpr :: [PE.Focus] -> PM PE.Focus  
          orCasesExpr [] = Left $ ProofError (CasesError EmptyCases) id -- La lista de casos no puede ser vacia
          orCasesExpr fs = Right $ PE.toFocus $ orCasesExpr' (map PE.toExpr fs)

          orCasesExpr' [] = error "Impossible!"
          orCasesExpr' [e'] = e'
          orCasesExpr' (e':es) = PE.BinOp folOr e' (orCasesExpr' es)
          
          checkSubProofCases :: Proof -> (PE.Focus,Proof) -> PM ()
          checkSubProofCases proof' (c,cProof) =
              sameProof proof' cProof >>
              getCtx cProof >>= \cpCtx ->
              getCtx proof' >>= \ctx ->
              return (freshName cpCtx) >>= \name' -> 
              return (createHypothesis name' (Expr $ PE.toExpr c) noCondition) >>= \hypCase -> 
              (return $ Map.elems ctx) >>= \hypsProof ->
              (return $ Map.elems cpCtx) >>= \hypsCasesProof ->
              allElems (hypCase:hypsProof) hypsCasesProof errHypCase >>
              validateProof' cProof id >> 
              return ()
           where errHypCase = errProofPlace $ CasesError HypothesisCases
              
validateProof' _ _ = error "ValidateProof!"

-- | Chequea que los contextos de dos pruebas sean iguales.
sameCtxs :: Proof -> Proof -> PM ()
sameCtxs = liftA2' (checkEqWithDefault (errProofPlace ContextsNotEqual)) `on` getCtx

-- | Esta función verifica si dos pruebas prueban lo mismo, aunque no sean la misma prueba.
--   Esto es, que tengan las mismas expresiones inicial y final, y la misma relacion. Podrían
--   tener diferentes contextos.
--   Si se verifica, retorna Right (), caso contrario Left error.
sameProof :: Proof -> Proof -> PM ()
sameProof p1 p2 =
    getStart p1 >>= \p1st -> getEnd p1 >>= \p1end -> 
    getRel p1 >>= \rel1 -> getStart p2 >>= \p2st -> 
    getEnd p2 >>= \p2end ->
    getRel p2 >>= \rel2 ->
    whenPM' (p1st==p2st && p1end==p2end && rel1==rel2) (errProofPlace (InductionError IncorrectSubProof))


-- | Chequea que dos pruebas prueban lo mismo, aplicando en las expresiones de
--   la segunda la substitución dada.
sameProofWithSubst :: Proof -> Proof -> PE.ExprSubst -> PM ()
sameProofWithSubst p1 p2 subst = 
        getStart p2 >>= \p2start -> 
        getEnd p2 >>= \p2end ->
        return (updateStart p2 (PE.toFocus $ PE.applySubst (PE.toExpr p2start) subst)) >>= \p2' ->
        return (updateEnd p2' (PE.toFocus $ PE.applySubst (PE.toExpr p2end) subst)) >>= \p2'' ->
        sameProof p1 p2''
                

-- | Devuelve las posibles expresiones de aplicar una 
possibleExpr :: PE.PreExpr -> Basic -> [(PE.PreExpr, Maybe PE.Focus)]
possibleExpr p basic = 
            case basic of
                Evaluate -> filter (flip ((/=) . fst) p) [(evalExpr p,Nothing)]
                _ -> map ((PE.toExpr &&& Just) . fst) . typed $ concatMap (rewriteAllFocuses p) (truthRules basic)
    where typed = filter (isRight . checkPreExpr . PE.toExpr . fst) . rights
{- | Comenzamos una prueba dados dos focus y una relaci&#243;n entre ellas, de 
        la cual no tenemos una prueba.
    {POS: El contexto de la prueba es vacio.}
    Dadas rel, f y f' tenemos una prueba del estilo;
    
@
    f
rel {?}
    f'
@
-}
newProof :: Maybe Ctx -> Relation -> PE.Focus -> PE.Focus -> Proof
newProof = Hole . maybe beginCtx id


-- | Comenzamos una prueba dada la expresion inicial y la relacion.
newProofWithStart :: Relation -> PE.Focus -> Proof
newProofWithStart rel f = Hole beginCtx rel f PE.emptyExpr

{- | Comenzamos una prueba dada una relación. No tenemos ni las expresiones
     ni la prueba.
    {POS: El contexto de la prueba es vacio.}
    Dada rel tenemos una prueba del estilo;
    
@
    Hole
rel {?}
    Hole
@
-}
holeProof :: Maybe Ctx -> Relation -> Proof
holeProof c r = newProof c r PE.emptyExpr PE.emptyExpr

-- | ProofFocus vacio
emptyProof :: Maybe Ctx -> Relation -> ProofFocus
emptyProof c r = toProofFocus $ holeProof c r

{- | Comenzamos una prueba dado unfocus y una relacion.
    {POS: El contexto de la prueba es vacio.}
    Dadas rel y f tenemos una prueba del estilo;
    
@
    f
rel {?}
    ?{}
@
-}
newProofWithoutEnd :: Relation -> PE.Focus -> PE.HoleInfo -> Proof
newProofWithoutEnd r f hi = Hole beginCtx r f h
    where h = PE.toFocus $ PE.preExprHole hi


{- | Comenzamos una prueba por casos. -}
newProofWithCases :: Relation -> PE.Focus -> PE.Focus -> PE.Focus -> [PE.Focus] -> Maybe Proof -> Proof
newProofWithCases r f f' c _ orGuardsProof = Cases ctx r f f' c lp orGuardsProof
    where
        ctx :: Ctx
        ctx = beginCtx
        lp :: [(PE.Focus,Proof)]
        lp = []


-- | Función para convertir una prueba en un Hole
toHoleProof :: ProofFocus -> ProofFocus
toHoleProof (Simple ctx r f f' _,path) = (Hole ctx r f f',path)
toHoleProof (Trans ctx r f _ f'' _ _,path) = (Hole ctx r f f'',path)
toHoleProof pf = pf

{- Funciones para pasar de una prueba vacía a una prueba con más contenido.
   Todas las funciones no validan la prueba, son solo para manipulacion -}

{- | Convierte una prueba vacía en un Simple o transforma una prueba simple en otra.
     Si la prueba no es vacía o no es simple, entonces se comporta como la identidad
     -}
simpleProof :: ProofFocus -> Basic -> ProofFocus
simpleProof (Hole ctx r f1 f2,path) b =  (Simple ctx r f1 f2 b,path)
simpleProof (Simple ctx r f1 f2 _,path) b = (Simple ctx r f1 f2 b,path)
simpleProof p _ = p



{- | Pasa de una prueba vacia a una prueba transitiva vacia. Si la prueba no es vacía
     o no es Simple, entonces se comporta como la identidad
     -}
addEmptyStep :: ProofFocus -> ProofFocus
addEmptyStep (Hole ctx r f1 f2,path) = 
    (Trans ctx r f1 PE.emptyExpr f2 (Hole ctx r f1 PE.emptyExpr) (Hole ctx r PE.emptyExpr f2),path)
-- Si le pasamos una prueba simple, la considera un hueco
addEmptyStep (Simple ctx r f1 f2 _,path) = 
    (Trans ctx r f1 PE.emptyExpr f2 (Hole ctx r f1 PE.emptyExpr) (Hole ctx r PE.emptyExpr f2),path)
addEmptyStep p = p



{-
5. Definir funciones que comprueban que un elemento de tipo Proof
es realmente una prueba.

Entiendo que algo de tipo Proof sera una prueba cuando se cumpla lo 
siguiente; Sea P de tipo Proof
* P no es Hole, al igual que ninguna de sus ramas.
* Todas las expresiones de la prueba estan bien tipadas. Acá la duda es,
 deberíamos hacer checkPreExpr de cada expresión que se encuentre en la 
 prueba y ver que todas las expresiones sean "tipables" (?) 
 O hay que hacer un poquito mas y lo que deberíamos hacer es, ademas de
 hacer checkPreExpr como antes, ver que el conjunto de tipos que nos quedo
 sean unificables de a pares (?)

Como PRE importante considero que cada vez que se agregaba un paso en una 
en un prueba se comprobaba que las expresiones "matchearan" como
correspondían y ademas que al ingresar un axioma o teorema este se aplico 
correctamente.
-}

addBoolHypothesis :: PE.PreExpr -> Ctx -> (Ctx,Maybe Name)
addBoolHypothesis e = addHypothesis e relEquiv [true']
    where Expr true' = true
   
-- | Dada una prueba, retorna la expresión que representa a toda la prueba.
getExprProof :: PM Proof -> Expr
getExprProof = either (const holeExpr) buildExpr
    where buildExpr = maybe holeExpr Expr . mexpr
          mexpr p = P.getRel p >>= \r ->
                   P.getStart p >>= \e ->
                   P.getEnd p >>= \s ->
                   return (PE.BinOp (relToOp r) (PE.toExpr e) (PE.toExpr s))

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation xs ys = Set.fromList xs == Set.fromList ys

liftA2' :: (a -> b -> PM c) -> PM a -> PM b -> PM c
liftA2' f ma mb = ma >>= \a -> mb >>= f a

allElems :: (Show a,Eq a) => [a] -> [a] -> ProofError -> PM ()
allElems inList l _ = whenPM' (all (`elem` inList) l) (error $ show l ++ "\n" ++ show inList)


-- | Todos los constructores (0-arios, unarios y binarios) usados en
-- una prueba correspondan al tipo inductivo.
checkConstructor :: (Ord a,Show a) => PErrorInduction -> 
                   (PE.PreExpr -> Maybe a) -> (IndType -> [a]) -> 
                   IndType -> [(PE.PreExpr,Proof)] -> PM ()
checkConstructor err get els it ps' = 
                          validateEls >>= \elsInProof ->
                          whenPM' (isPermutation elsInProof (els it)) 
                                  (errProofPlace $ InductionError err)
    where l = map (get . fst) ps'
          validateEls = if Nothing `elem` l
                        then Left (errProofPlace $ InductionError err)
                        else return $ catMaybes l

checkAllConstants :: IndType -> [(PE.PreExpr,Proof)] -> PM ()
checkAllConstants = checkConstructor ConstantPatternError PE.getConstant constants
checkAllBaseConst :: IndType -> [(PE.PreExpr,Proof)] -> PM ()
checkAllBaseConst = checkConstructor OperatorPatternError PE.getOperator baseConstructors
checkAllIndConst :: IndType -> [(PE.PreExpr,Proof)] -> PM ()
checkAllIndConst = checkConstructor OperatorPatternError PE.getOperator indConstructors

-- | Un pattern no contiene variables que estén en las expresiones
-- originales (salvo que sea la misma variable inductiva).
-- TODO: Explicar el sentido.
checkVarsPattern :: Variable -> Proof ->  PE.PreExpr -> PM ()
checkVarsPattern v p pattern = 
    getStart p >>= \fs -> getEnd p >>= \fe ->
    return (Set.map varName $ PE.freeVars $ PE.toExpr fs) >>= 
    \varNames1 ->
    return (Set.map varName $ PE.freeVars $ PE.toExpr fe) >>=
    \varNames2 -> 
    return (Set.delete (varName v) $ Set.union varNames1 varNames2) >>= \varNames ->
    return (Set.map varName $ PE.freeVars pattern) >>= 
    \varNamesPattern ->
    whenPM' (Set.notMember True $ Set.map (flip Set.member varNames) varNamesPattern)
            (errProofPlace (InductionError VariablePatternError))

-- | TODO: documentar!
cheqEqCases :: ([(PE.PreExpr,a)],[(PE.PreExpr,a)],[(PE.PreExpr,a)]) ->
               ([(PE.PreExpr,b)],[(PE.PreExpr,b)],[(PE.PreExpr,b)]) ->
               PM ()
cheqEqCases (l1,l2,l3) (l1',l2',l3') =  whenPM' (map fst l1 == map fst l1' && 
                                                 map fst l2 == map fst l2' && 
                                                 map fst l3 == map fst l3') 
                                                (errProofPlace $ InductionError ExprCases)

-- | TODO : documentar!
getIndType' :: Variable -> PM IndType
getIndType' = maybe (Left errIndType) return . getIndType . varTy
    where errIndType = errProofPlace $ InductionError TypeNotInductive
