{-# Language TypeSynonymInstances #-}

module Equ.Proof.Zipper 
    ( ProofFocus' , ProofFocus
    , ProofPath'(..), ProofPath
    , toProof, toProofFocus
    , replace
    , goDown, goUp, goLeft, goRight, goDownR, goDownL, goTop
    -- las siguientes funcionas navegan el zipper y siempre devuelven algo
    , goDown', goUp', goLeft', goRight', goDownR', goDownL', goTop', goEnd
    , moveToEnd, goFirstLeft, goLeftLeaf, goNextStep, goFirstRight, goRightLeaf
    , goPrevStep, goNextStep', goPrevStep'
    , updateStartFocus, updateEndFocus
    , updateMiddleFocus, getStartFocus, getEndFocus, getBasicFocus
    ) where

import Equ.Proof.Proof
import Data.Monoid
import Data.Maybe(fromJust)
import qualified Equ.PreExpr as PE (Focus)
import Equ.Rule(Relation)

import Control.Applicative ((<$>))

-- | Definición de los posibles lugares en los que podemos estar
-- enfocándonos.
data ProofPath' ctxTy relTy proofTy exprTy = 
                 Top
               | TransL (ProofPath' ctxTy relTy proofTy exprTy) (Proof' ctxTy relTy proofTy exprTy)
               | TransR (Proof' ctxTy relTy proofTy exprTy) (ProofPath' ctxTy relTy proofTy exprTy)

instance Show (ProofPath' ctxTy relTy proofTy exprTy) where
    show Top = "Top"
    show (TransL path _) = "TransL | " ++ show path
    show (TransR _ path) = "TransR | " ++ show path
               
            
type ProofFocus' ctxTy relTy proofTy exprTy = (Proof' ctxTy relTy proofTy exprTy
                                            ,  ProofPath' ctxTy relTy proofTy exprTy)

type ProofPath = ProofPath' Ctx Relation Basic PE.Focus
type ProofFocus = ProofFocus' Ctx Relation Basic PE.Focus

toProof :: ProofFocus' ctxTy relTy proofTy exprTy -> Proof' ctxTy relTy proofTy exprTy
toProof (p, Top) = p
toProof (p, TransL path pr) = toProof (mappend p pr,path)
toProof (p, TransR pl path) = toProof (mappend pl p,path)

toProofFocus :: Proof' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
toProofFocus p = (p,Top)

-- | Reemplaza la expresión enfocada por una nueva expresión.
replace :: ProofFocus' ctxTy relTy proofTy exprTy -> Proof' ctxTy relTy proofTy exprTy -> 
           ProofFocus' ctxTy relTy proofTy exprTy
replace (_,p) p' = (p',p)

-- | Bajar un nivel en el focus, yendo por izquierda.
goDownL :: ProofFocus' ctxTy relTy proofTy exprTy -> 
           Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
goDownL = goDown

goDownL' :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goDownL' = goDown'

-- | Bajar un nivel en el focus, yendo por derecha.
goDownR :: ProofFocus' ctxTy relTy proofTy exprTy -> 
           Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
goDownR f = goDown f >>= goRight

goDownR' :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goDownR' = goRight' . goDown'

-- Navegación dentro de un Zipper.
-- | Bajar un nivel en el focus.
goDown :: ProofFocus' ctxTy relTy proofTy exprTy -> 
          Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
goDown (Trans _ _ _ _ _ pl pr,path) = Just (pl,TransL path pr)
goDown (_,_)= Nothing

goDown' :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goDown' (Trans _ _ _ _ _ pl pr,path) = (pl,TransL path pr)
goDown' pf= pf

-- | Subir un nivel en el focus.
goUp :: ProofFocus' ctxTy relTy proofTy exprTy -> 
        Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
goUp (_, Top) = Nothing
goUp (p, TransL path pr) = Just (mappend p pr,path)
goUp (p, TransR pl path) = Just (mappend pl p,path)

goUp' :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goUp' (p, TransL path pr) = (mappend p pr,path)
goUp' (p, TransR pl path) = (mappend pl p,path)
goUp' pf = pf

-- | Sube hasta el tope.
goTop :: ProofFocus' ctxTy relTy proofTy exprTy -> 
         Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
goTop (p,Top) = Just (p,Top)
goTop pf = goTop $ fromJust $ goUp pf

goTop' :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goTop' (p,Top) = (p,Top)
goTop' pf = goTop' $ goUp' pf

-- | Se mueve a la derecha todo lo q pueda.
goEnd :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goEnd pf = maybe pf goEnd $ goDownR pf

-- | Ir a la izquierda en un focus, sin cambiar de nivel.
goLeft :: ProofFocus' ctxTy relTy proofTy exprTy -> 
          Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
goLeft (p, TransR pl path) = Just (pl,TransL path p)
goLeft (_, _) = Nothing

goLeft' :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goLeft' (p, TransR pl path) = (pl,TransL path p)
goLeft' pf = pf

{- | Se Mueve arriba desde la derecha, lo mas que puede, hasta llegar a un nodo
     que esta en la parte izquierda: 
-}
goFirstLeft :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goFirstLeft pf = maybe (maybe pf goFirstLeft (goUp pf)) 
                       (const pf) 
                       (goRight pf)
               
-- | Simétrica a goFirstLeft
goFirstRight :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goFirstRight pf = maybe (maybe pf goFirstRight (goUp pf)) 
                        (const pf) 
                        (goLeft pf)
                      
-- | Baja a la hoja de mas a la izquierda
goLeftLeaf :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goLeftLeaf pf = maybe pf goLeftLeaf $ goDownL pf
               
-- | Baja a la hoja de mas a la derecha
goRightLeaf :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goRightLeaf pf = maybe pf goRightLeaf $ goDownR pf

-- | Ir a la derecha en un focus, sin cambiar de nivel.
goRight :: ProofFocus' ctxTy relTy proofTy exprTy -> 
           Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
goRight (p, TransL path pr) = Just (pr,TransR p path)
goRight (_, _) = Nothing

goRight' :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goRight' (p, TransL path pr) = (pr,TransR p path)
goRight' pf = pf

{- | La siguiente funcion mueve el focus hasta la siguiente hoja de la prueba.
     En una transitividad, vista como lista: [p1,...,pn]. goNextStep pi = pi+1.
     NOTA: Tal como está ahora, si estamos en la ultima hoja, vuelva a la primera.
     -}
goNextStep :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goNextStep pf = maybe pf goLeftLeaf $ goRight (goFirstLeft pf)

goPrevStep :: ProofFocus' ctxTy relTy proofTy exprTy -> ProofFocus' ctxTy relTy proofTy exprTy
goPrevStep pf = maybe pf goRightLeaf $ goLeft (goFirstRight pf)

goNextStep' :: ProofFocus' ctxTy relTy proofTy exprTy -> Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
goNextStep' pf = goLeftLeaf <$> goRight (goFirstLeft pf)

goPrevStep' :: ProofFocus' ctxTy relTy proofTy exprTy -> Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
goPrevStep' pf = let pf' = goFirstRight pf in
                    case goLeft pf' of
                         Nothing -> Nothing -- Estamos en la primera hoja de la izquierda
                         Just pf'' -> Just $ goRightLeaf pf''
                         

moveToEnd :: ProofFocus' ctxTy relTy proofTy exprTy -> 
             Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
moveToEnd = Just . goEnd . goTop'


-- Funciones que actualizan elementos de la prueba enfocada
updateStartFocus :: ProofFocus' ctxTy relTy proofTy exprTy -> exprTy -> 
                   Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
updateStartFocus (p,path) f = Just (updateStart p f,path)

updateEndFocus :: ProofFocus' ctxTy relTy proofTy exprTy -> exprTy -> 
                  Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
updateEndFocus (p,path) f = Just (updateEnd p f,path)

updateMiddleFocus :: ProofFocus' ctxTy relTy proofTy exprTy -> exprTy -> 
                     Maybe (ProofFocus' ctxTy relTy proofTy exprTy)
updateMiddleFocus (p,path) f = Just (updateMiddle p f,path)

getStartFocus :: ProofFocus' ctxTy relTy proofTy exprTy -> Maybe exprTy
getStartFocus (p,path) = getStart p

getEndFocus :: ProofFocus' ctxTy relTy proofTy exprTy -> Maybe exprTy
getEndFocus (p,path) = getEnd p

getBasicFocus :: ProofFocus' ctxTy relTy proofTy exprTy -> Maybe proofTy
getBasicFocus (p,path) = getBasic p
