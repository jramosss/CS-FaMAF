{-# Language Rank2Types,FlexibleInstances,TypeSynonymInstances #-}
module Equ.Proof.ListedProof
    (ListedProof'(..),ListedProof,createListedProof
    , addStepOnPosition
    , updateSelExpr
    , updateRelLP
    , updateBasicLP
    , moveToPosition
    , moveToNextPosition
    , moveToPrevPosition
    , getSelExpr
    , getSelBasic
    , getRelLP
    , getStartExpr
    , getBasicAt
    , updateExprAt
    , updateFirstExpr
    , resetStep
    , listedToProof
    , runActionLP
    , isLastSelected
    ) where

import Equ.Proof.Proof
import Equ.Proof.Zipper
import Equ.Rule(Relation)

import qualified Equ.PreExpr as PE

import Data.Maybe(fromJust)
import Control.Monad

{- Un ListedProof nos sirve para ver una prueba transitiva como una lista de pasos
simples.
Para utilizar este tipo, lo creamos a partir de una prueba transitiva, y luego
podemos ir reemplazando pasos simples o huecos por más transitividades, es decir, 
podemos reemplazar el elemento i-ésimo de la lista, en dos nuevos elementos (que corresponden
a la prueba izquierda y derecha de la nueva transitividad que queda en ese lugar).
-}

data ListedProof' ctxTy relTy proofTy exprTy= ListedProof' {
                    pList  :: [Proof' ctxTy relTy proofTy exprTy]
                  , pFocus :: ProofFocus' ctxTy relTy proofTy exprTy
                  , selIndex :: Int -- indice del elemento seleccionado
}

createListedProof :: ProofFocus' ctxTy relTy proofTy exprTy -> 
                     Maybe (ListedProof' ctxTy relTy proofTy exprTy)
createListedProof pf = let pf' = goTop' pf in
                           case (fst pf') of
                                (Simple _ _ _ _ _) -> Just $ lSimple pf'
                                (Hole _ _ _ _) -> Just $ lSimple pf'
                                (Trans _ _ _ _ _ _ _) -> Just $ lTrans pf'
                                _ -> Nothing
                                
    where lSimple pfocus = ListedProof' {
                            pList = [fst pfocus]
                          , pFocus = pfocus
                          , selIndex = 0
                          }
          lTrans pfocus = ListedProof' {
                            pList = createListedProof' [fst focusOnLeft] focusOnLeft
                          , pFocus = focusOnLeft
                          , selIndex = 0
                          }
            where focusOnLeft = goLeftLeaf pfocus
                            

createListedProof' :: [Proof' ctxTy relTy proofTy exprTy] ->
                      ProofFocus' ctxTy relTy proofTy exprTy -> 
                      [Proof' ctxTy relTy proofTy exprTy]
createListedProof' ps pf = let mpf = goNextStep' pf in
                               case mpf of
                                    Nothing -> ps
                                    Just pf' -> createListedProof' (ps++[fst pf']) pf'
   
{-| Transforma una prueba simple o hueco, que tenemos en la lista, en dos nuevos huecos y 
actualiza el proofFocus, creando la nueva transitividad correspondiente.
La función fNewProofs toma el paso de la prueba que queremos transformar y devuelve
una nueva expresion y dos nuevos pasos, así se puede construir la transitividad.
Dejamos enfocado la parte derecha de esa transitividad.
Las funciones fUpIndexExpr y fUpIndexBasic toman las expresiones y el basic de un paso
de prueba respectivamente,y debe actualizar
algun componente relacionado con el indice del paso en la lista, aumentándolo en uno (se aplica a los pasos
que se veran afectados por la inserción del nuevo paso).-}
addStepOnPosition :: Int -> 
                     (Proof' ctxTy relTy proofTy exprTy -> 
                     (exprTy,Proof' ctxTy relTy proofTy exprTy,Proof' ctxTy relTy proofTy exprTy)) ->
                     (exprTy -> Int -> exprTy) -> (proofTy -> Int -> proofTy) ->
                     ListedProof' ctxTy relTy proofTy exprTy -> 
                     (ListedProof' ctxTy relTy proofTy exprTy)
addStepOnPosition ind fNewProofs fUpIndexExpr fUpIndexBasic lProof = 
                                if ind < 0 || ind >= length (pList lProof)
                                then lProof
                                else updateStepIndexes fUpIndexExpr fUpIndexBasic
                                            (ind+1) $
                                            ListedProof' {
                                                pList = take ind (pList lProof) ++ 
                                                newSteps nPFocus ++ drop (ind+1) (pList lProof)
                                            , pFocus = newFocus nPFocus
                                            , selIndex = ind + 1
                                            }
                                  
    where nPFocus = (moveToPos ind $ pFocus lProof)
          
          newSteps pf = [pf',pf'']
              where (_,pf',pf'') = fNewProofs $ fst pf

          newFocus (p,path) = goDownR' (Trans ctx rel expr1 newExpr expr2 p1 p2,path)
            where   ctx = fromJust $ getCtx p
                    rel = fromJust $ getRel p
                    expr1 = fromJust $ getStart p
                    expr2 = fromJust $ getEnd p
                    (newExpr,p1,p2) = fNewProofs p
   
updateStepIndexes :: (exprTy -> Int -> exprTy) -> (proofTy -> Int -> proofTy) ->
                     Int ->  ListedProof' ctxTy relTy proofTy exprTy -> 
                     ListedProof' ctxTy relTy proofTy exprTy
updateStepIndexes fUpExpr fUpBasic ind lProof = 
    let oldList = take ind (pList lProof) in
        ListedProof' {
            pList = oldList ++ listUpdated ind (drop ind $ pList lProof)
          , pFocus = moveToPos ind $ updateFocus (pFocus lProof) ind
          , selIndex = ind
        }
              
              
    where listUpdated _ [] = []
          listUpdated i (x:xs) = (upStep x i) : (listUpdated (i+1) xs)
          
          updateFocus pf i = let (p,path) = moveToPos i pf in
                                 let p' = upStep p i in
                                     case goNextStep' (p',path) of
                                          Nothing -> (p',path)
                                          Just pf' -> updateFocus pf' (i+1)
            
          upStep prf i = 
              case prf of
                (Hole ctx r e1 e2) -> (Hole ctx r (fUpExpr e1 i) (fUpExpr e2 i))
                (Simple ctx r e1 e2 b) -> (Simple ctx r (fUpExpr e1 i) (fUpExpr e2 i) (fUpBasic b i))
                prf' -> prf'
          
          
-- | Ejecuta acciones monádicas sobre cada elemento de la prueba listeada, a partir
-- de un índice. La acción monádica devuelve la expresion de la derecha y el basic
-- correspondientes al paso en el cual se ejecuta la accion.
-- El uso que le damos a esto es para restablecer los manejadores de eventos luego
-- de haber cambiado el índice de cada paso que quedó a la derecha del agregado.
-- Al cambiar los manejadores tenemos que actualizar los cids de la expresion derecha y
-- del basic correspondiente.
runActionLP :: Monad m => ListedProof' ctxTy relTy proofTy exprTy -> Int ->
                     (Proof' ctxTy relTy proofTy exprTy -> m (exprTy,proofTy)) -> 
                     m (ListedProof' ctxTy relTy proofTy exprTy)
runActionLP lPrf idx act = liftM (moveToPosition idx) $ runActionLP' lPrf idx act
    where runActionLP' lProof ind action = 
            let lProof' = moveToPosition ind lProof
                selProof = (pList lProof')!!ind
            in action selProof >>= \(e,p) ->
               return (updateSelExpr e lProof') >>= \lProof'' ->
               return (updateBasicLP lProof'' p) >>= \lp ->
               if ind < length (pList lProof') - 1
               then runActionLP' lp (ind+1) action
               else return lp
           

-- -- Reemplaza la expresión izquierda del primer paso.
updateFirstExpr :: exprTy -> ListedProof' ctxTy relTy proofTy exprTy ->
                  ListedProof' ctxTy relTy proofTy exprTy
updateFirstExpr expr lProof = let ind = selIndex lProof in
                            ListedProof' {
                                pList = newList
                              , pFocus = nPFocus
                              , selIndex = ind
                            }
                            
    where newList = updateStart (head (pList lProof)) expr:tail (pList lProof)        
          nPFocus = let pf = pFocus lProof 
                    in fromJust $ updateStartFocus (goTop' pf) expr
                                    
-- Reemplaza la expresión derecha de un paso de la prueba. Deja enfocado el paso.
updateSelExpr :: exprTy -> ListedProof' ctxTy relTy proofTy exprTy ->
                 ListedProof' ctxTy relTy proofTy exprTy
updateSelExpr expr lProof = let ind = selIndex lProof in
                            ListedProof' {
                                pList = newList ind
                              , pFocus = nPFocus
                              , selIndex = ind
                            }
                            
    where newList i = if i < length (pList lProof) - 1
                        then take i (pList lProof) ++
                             [updateEnd ((pList lProof)!!i) expr,
                              updateStart ((pList lProof)!!(i+1)) expr] ++
                             drop (i+2) (pList lProof)
                        -- si no, estamos enfocados en el ultimo paso.
                        else take i (pList lProof) ++
                             [updateEnd ((pList lProof)!!i) expr]
        
        
          nPFocus = let up1 = updateEndFocus (goFirstLeft $ pFocus lProof) expr in
                        case goRight (fromJust up1) of
                             Nothing -> goEnd (fromJust up1)
                             Just pf' -> goEnd $ goDownL' $ fromJust $ updateMiddleFocus (goUp' $ fromJust $ updateStartFocus pf' expr) expr

{- | Devuelve la expresión seleccionada dentro de la prueba, ésta es siempre la de la
derecha del paso que tenemos enfocado. -}
getSelExpr :: ListedProof' ctxTy relTy proofTy exprTy -> exprTy
getSelExpr lProof = fromJust $ getEnd ((pList lProof)!!(selIndex lProof))

{- | Devuelve la expresión izquierda del paso de prueba seleccionado. -}
getStartExpr :: ListedProof' ctxTy relTy proofTy exprTy -> exprTy
getStartExpr lProof = fromJust $ getStart ((pList lProof)!!(selIndex lProof))

getSelBasic :: ListedProof' ctxTy relTy proofTy exprTy -> Maybe proofTy
getSelBasic lProof = getBasic ((pList lProof)!!(selIndex lProof))

getBasicAt :: Int -> ListedProof' ctxTy relTy proofTy exprTy -> exprTy 
getBasicAt i = getSelExpr . moveToPosition i

updateExprAt :: Int -> exprTy -> ListedProof' ctxTy relTy proofTy exprTy ->
               ListedProof' ctxTy relTy proofTy exprTy
updateExprAt i e p =  (moveToPosition oldIdx . updateSelExpr e . moveToPosition i) p
    where oldIdx = selIndex p

-- | Transforma el paso enfocado en un Hole.
resetStep :: ListedProof' ctxTy relTy proofTy exprTy ->
             ListedProof' ctxTy relTy proofTy exprTy
resetStep lProof = case pFocus lProof of
                        (Simple c r f f' _,p) -> lProof { pList = newPList ind
                                                        , pFocus = (Hole c r f f',p)
                                                        }
                        pf -> lProof { pList = newPList ind
                                     , pFocus = pf
                                     }
                                
    where ind = selIndex lProof 
          newPList i = take i (pList lProof) 
                       ++ [resetStep' $ (pList lProof)!!i] 
                       ++ drop (i+1) (pList lProof)
          resetStep' p = case p of
                           (Simple ctx r f f' _) -> Hole ctx r f f'
                           p' -> p'
          
listedToProof :: ListedProof' ctxTy relTy proofTy exprTy -> 
                 Proof' ctxTy relTy proofTy exprTy
listedToProof lProof = toProof $ pFocus lProof


getRelLP :: ListedProof' ctxTy relTy proofTy exprTy -> relTy
getRelLP lProof = fromJust $ getRel ((pList lProof)!!(selIndex lProof))

updateRelLP :: ListedProof' ctxTy relTy proofTy exprTy -> relTy ->
               ListedProof' ctxTy relTy proofTy exprTy
updateRelLP lProof rel = let ind = selIndex lProof in
                         lProof {
                            pList = take ind (pList lProof) ++
                             [updateRel ((pList lProof)!!ind) rel] ++
                             drop (ind+1) (pList lProof)
                          , pFocus = (updateRel (fst $ pFocus lProof) rel,snd (pFocus lProof))
                         }
                         
updateBasicLP :: ListedProof' ctxTy relTy proofTy exprTy -> proofTy ->
                 ListedProof' ctxTy relTy proofTy exprTy
updateBasicLP lProof basic = case (pList lProof)!!idx of
                                  (Hole c r e1 e2) -> lproofRet c r e1 e2 idx
                                  (Simple c r e1 e2 _) -> lproofRet c r e1 e2 idx
                                  _ -> lProof
                                  
    where idx = selIndex lProof
          lproofRet c r e1 e2 ind = 
            lProof {
                  pList = take ind (pList lProof) 
                          ++ [(Simple c r e1 e2 basic)] 
                          ++ drop (ind+1) (pList lProof)
                , pFocus = (Simple c r e1 e2 basic,snd (pFocus lProof))
                }
                 
                
moveToPosition :: Int -> ListedProof' ctxTy relTy proofTy exprTy ->
                  ListedProof' ctxTy relTy proofTy exprTy
moveToPosition i lProof = if i < 0 || i >= length (pList lProof)
                          then lProof
                          else ListedProof' {
                                    pList = pList lProof
                                  , pFocus = moveToPos i (pFocus lProof)
                                  , selIndex = i
                             }
                             
moveToNextPosition :: ListedProof' ctxTy relTy proofTy exprTy ->
                      ListedProof' ctxTy relTy proofTy exprTy
moveToNextPosition lProof = moveToPosition (selIndex lProof + 1) lProof
                               
moveToPrevPosition :: ListedProof' ctxTy relTy proofTy exprTy ->
                      ListedProof' ctxTy relTy proofTy exprTy
moveToPrevPosition lProof = moveToPosition (selIndex lProof - 1) lProof
                    
isLastSelected :: ListedProof' ctxTy relTy proofTy exprTy -> Bool
isLastSelected lProof = selIndex lProof == (length (pList lProof)) - 1
                    
                    
-- Mueve un proofFocus hasta la hoja indicada por el indice. 
-- NOTA: Si el indice es mayor a la cantidad de hojas devuelve la ultima.
moveToPos :: Int -> ProofFocus' ctxTy relTy proofTy exprTy -> 
             ProofFocus' ctxTy relTy proofTy exprTy
moveToPos 0 pf = goLeftLeaf $ goTop' pf
moveToPos n pf = goNextStep (moveToPos (n-1) pf)
                      
                      
                                    
type ListedProof = ListedProof' Ctx Relation Basic PE.Focus
                                    
instance Show ListedProof where
    show lProof = show (pList lProof) ++ " | Index: " ++ show (selIndex lProof)

