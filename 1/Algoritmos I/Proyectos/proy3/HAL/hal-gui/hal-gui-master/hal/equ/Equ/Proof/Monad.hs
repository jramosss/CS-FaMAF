module Equ.Proof.Monad where

import Equ.Proof.Error

import Equ.Proof.Proof hiding (getCtx, getStart, getEnd, getRel, setCtx)
import qualified Equ.Proof.Proof as P ( getCtx, getStart
                                      , getEnd, getRel
                                      , setCtx, freshName) 

import Equ.Rewrite(RM)
import Equ.PreExpr
import Equ.Rule
import Equ.Proof.Zipper(goTop')

import qualified Data.Map as M (fromList, toList)

type PM a = Either ProofError a

whenPM :: (a -> Bool) -> ProofError -> a -> PM a
whenPM p e a | p a      = return a
             | otherwise = Left e
             
whenPM' :: Bool -> ProofError -> PM ()
whenPM' b e | b = return ()
            | otherwise = Left e

-- liftRw :: RM a -> PM a
-- liftRw (Left err) = Left $ [Rewrite err]
-- liftRw (Right a) = return a

-- Lifting de proyecciones de Proof a la mónada de Proof.
getCtx :: Proof -> PM Ctx
getCtx = maybe (Left $ ProofError ReflexHasNoCtx goTop') return . P.getCtx

setCtx :: Ctx -> Proof -> PM Proof
setCtx c = maybe (Left $ ProofError ReflexHasNoStart goTop') return . (P.setCtx c)

getStart :: Proof -> PM Focus
getStart = maybe (Left $ ProofError ReflexHasNoStart goTop') return . P.getStart

getEnd :: Proof -> PM Focus
getEnd = maybe (Left $ ProofError ReflexHasNoEnd goTop') return . P.getEnd

getRel :: Proof -> PM Relation
getRel = maybe (Left $ ProofError ReflexHasNoRel goTop') return . P.getRel
