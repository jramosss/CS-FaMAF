-- | Módulo de la sintáxis extendida del lenguaje imperativo simple con anotaciones (LISA).
-- En esta versión guardamos ademas la información sobre los números de lineas
-- de los comandos.
{-# Language GADTs #-}
module HGUI.ExtendedLang where

import Prelude hiding (snd)

import Hal.Lang

import qualified Equ.Expr as Equ ( Expr(..) )
import qualified Equ.PreExpr as PreEqu ( PreExpr'(Con) )
import qualified Equ.Theories.FOL as TheoEqu ( folTrue )


import Text.Parsec.Pos

data CommPos = CommPos { begin :: SourcePos
                       , end   :: SourcePos
                       }
    deriving (Eq,Show)


true :: Equ.Expr
true = Equ.Expr $ PreEqu.Con TheoEqu.folTrue

makeCommPos :: SourcePos -> SourcePos -> CommPos
makeCommPos = CommPos

initPos :: CommPos
initPos = CommPos (initialPos "") (initialPos "")

takeBeginLComm :: ExtComm -> Int
takeBeginLComm (ExtSkip   pos)       = sourceLine $ begin pos
takeBeginLComm (ExtAbort  pos)       = sourceLine $ begin pos
takeBeginLComm (ExtPre    pos _)     = sourceLine $ begin pos
takeBeginLComm (ExtAssert pos _)     = sourceLine $ begin pos
takeBeginLComm (ExtIf     pos _)     = sourceLine $ begin pos
takeBeginLComm (ExtIAssig pos _ _)   = sourceLine $ begin pos
takeBeginLComm (ExtBAssig pos _ _)   = sourceLine $ begin pos
takeBeginLComm (ExtDo     pos _ _ _) = sourceLine $ begin pos
takeBeginLComm (ExtEval  (toE:_))    = sourceLine $ begin (extEvalPos toE)
takeBeginLComm (ExtSeq c _)          = takeBeginLComm c
takeBeginLComm _          = error "takeBeginLComm: Imposible"

takeBeginCComm :: ExtComm -> Int
takeBeginCComm (ExtSkip   pos)         = sourceColumn $ begin pos
takeBeginCComm (ExtAbort  pos)         = sourceColumn $ begin pos
takeBeginCComm (ExtPre    pos _)       = sourceColumn $ begin pos
takeBeginCComm (ExtAssert pos _)       = sourceColumn $ begin pos
takeBeginCComm (ExtIf     pos _)       = sourceColumn $ begin pos
takeBeginCComm (ExtIAssig pos _ _)     = sourceColumn $ begin pos
takeBeginCComm (ExtBAssig pos _ _)     = sourceColumn $ begin pos
takeBeginCComm (ExtDo     pos _ _ _)   = sourceColumn $ begin pos
takeBeginCComm (ExtEval   (toE:_))     = sourceColumn $ begin (extEvalPos toE)
takeBeginCComm (ExtSeq c _)            = takeBeginCComm c
takeBeginCComm _          = error "takeBeginCComm: Imposible"

takeEndCComm :: ExtComm -> Int
takeEndCComm (ExtSkip   pos)         = sourceColumn $ end pos
takeEndCComm (ExtAbort  pos)         = sourceColumn $ end pos
takeEndCComm (ExtPre    pos _)       = sourceColumn $ end pos
takeEndCComm (ExtAssert pos _)       = sourceColumn $ end pos
takeEndCComm (ExtIf     pos _)       = sourceColumn $ end pos
takeEndCComm (ExtIAssig pos _ _)     = sourceColumn $ end pos
takeEndCComm (ExtBAssig pos _ _)     = sourceColumn $ end pos
takeEndCComm (ExtDo     pos _ _ _)   = sourceColumn $ end pos
takeEndCComm (ExtEval   (toE:_)) = sourceColumn $ end (extEvalPos toE)
takeEndCComm (ExtSeq c _)            = takeEndCComm c
takeEndCComm _          = error "takeEndCComm: Imposible"

extEvalPos :: ExpToEval -> CommPos
extEvalPos (Left (pos,_)) = pos
extEvalPos (Right (pos,_)) = pos

takePos :: ExtComm -> CommPos
takePos (ExtSkip   pos)       = pos
takePos (ExtAbort  pos)       = pos
takePos (ExtPre    pos _)     = pos
takePos (ExtAssert pos _)     = pos
takePos (ExtIf     pos _)     = pos
takePos (ExtIAssig pos _ _)   = pos
takePos (ExtBAssig pos _ _)   = pos
takePos (ExtDo     pos _ _ _) = pos
takePos (ExtEval   (toE:_))   = extEvalPos toE
takePos (ExtSeq c _)          = takePos c
takePos _ = error "Imposible"

getCommLines :: ExtComm -> [Int]
getCommLines (ExtSeq c c')        = getCommLines c ++ getCommLines c'
getCommLines cif@(ExtIf _ cs)     = takeBeginLComm cif :
                                    (concat $ map (getCommLines . trd) cs)
getCommLines cdo@(ExtDo _ _ _ c)  = takeBeginLComm cdo : getCommLines c
getCommLines c                    = [takeBeginLComm c]

fst :: (CommPos,BExp,ExtComm) -> CommPos
fst (cp,_,_) = cp

snd :: (CommPos,BExp,ExtComm) -> BExp
snd (_,b,_) = b

trd :: (CommPos,BExp,ExtComm) -> ExtComm
trd (_,_,c) = c

-- Los terminos que representan los comandos con la información extra
-- sobre en que linea se encuentran.
data ExtComm where
    ExtSkip   :: CommPos -> ExtComm
    ExtAbort  :: CommPos -> ExtComm
    
    ExtPre    :: CommPos -> FormFun -> ExtComm
    ExtAssert :: CommPos -> FormFun -> ExtComm
    
    ExtIf     :: CommPos -> [(CommPos,BExp,ExtComm)] -> ExtComm
    
    ExtIAssig :: CommPos -> Identifier -> Exp -> ExtComm
    ExtBAssig :: CommPos -> Identifier -> BExp -> ExtComm
    
    ExtDo     :: CommPos -> FormFun -> BExp -> ExtComm -> ExtComm
    ExtSeq    :: ExtComm -> ExtComm -> ExtComm
    ExtEval   :: ExpsToEval -> ExtComm
    deriving Show

type ExpToEval  = Either (CommPos,Exp) (CommPos,BExp)
type ExpsToEval = [ExpToEval]

data ExtProgram where
    ExtProg     :: LIdentifier -> ExtComm    -> ExtProgram
    ExtEvalProg :: LIdentifier -> ExpsToEval -> ExtProgram
    deriving Show
