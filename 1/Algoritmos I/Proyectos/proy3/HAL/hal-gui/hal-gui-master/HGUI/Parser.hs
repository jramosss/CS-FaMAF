-- | Parser del Lenguaje extendido de LISA.
module HGUI.Parser where

import Language.Haskell.TH.Ppr ( bytesToString )
import qualified Data.ByteString as B
import qualified Data.Map as M
import Control.Applicative ( (<$>) )

-- Imports Parsec
import Text.Parsec 

-- Imports de Equ
import qualified Equ.Parser as PEqu

-- Imports de Hal
import Hal.Parser

-- Imports de Hal-Gui
import HGUI.ExtendedLang hiding (snd)

-- *** Comandos.
-- |Comando simples.
extSingle :: String -> (CommPos -> ExtComm) -> ParserH ExtComm
extSingle s ec = try $ do
              st <- getParserState
              let initp = statePos st
              _ <- sym s
              st' <- getParserState
              let endp = statePos st'

              _ <- semip
              return $ ec $ makeCommPos initp endp

-- | Skip
extSkip :: ParserH ExtComm
extSkip = extSingle "skip" ExtSkip

-- | Abort
extAbort :: ParserH ExtComm
extAbort = extSingle "abort" ExtAbort

-- |Asignación.
extAssignInt :: ParserH ExtComm
extAssignInt = try $ do
               st <- getParserState
               let initp = statePos st
               
               acc <- pintvar  
               oper ":="
               iexp <- intexp 
               
               st' <- getParserState
               let endp = statePos st'

               _ <- semip
               return $ ExtIAssig (makeCommPos initp endp) acc iexp

extAssignBool :: ParserH ExtComm
extAssignBool = try $ do
                st <- getParserState
                let initp = statePos st
                
                acc <- pboolvar
                oper ":="
                bexp <- boolexp 
                
                st' <- getParserState
                let endp = statePos st'
                
                _ <- semip
                return $ ExtBAssig (makeCommPos initp endp) acc bexp

-- |Condicional.      
extIfthen :: ParserH ExtComm
extIfthen = try $ do
            keyword "if" 

            inib <- statePos <$> getParserState
            b <- (boolexp <?> "Expresión booleana")
            endb <- statePos <$> getParserState

            keyword "->"
            c <- extComm

            cs <- many (try $ keyword "|" >>
                              statePos <$> getParserState >>= \inibi ->
                              boolexp >>= \bi ->
                              statePos <$> getParserState >>= \endbi ->
                              keyword "->" >>
                              extComm >>= \ci ->
                              return (makeCommPos inibi endbi, bi,ci)
                       )
            
            keyword "fi"
            
            return $ ExtIf (makeCommPos inib endb) 
                           ((makeCommPos inib endb,b,c):cs)

-- | Assert
extAssert :: ParserH ExtComm
extAssert = try $ do
            st <- getParserState
            let initp = statePos st
            
            f <- formfun
            
            st' <- getParserState
            let endp = statePos st'
            return $ ExtAssert (makeCommPos initp endp) f

-- | Do - While
extWhile :: ParserH ExtComm
extWhile = try $ do
           whites
           keyword "do"
           
           inib <- statePos <$> getParserState
           b <- boolexp
           endb <- statePos <$> getParserState
           
           keyword "->"
           c <- extComm
           keyword "od"
           
           return (ExtDo (makeCommPos inib endb) true b c)

-- | Comandos del lenguaje LISA
extComms :: ParserH ExtComm
extComms = choice $ map try
           [ extSkip
           , extAbort
           , extAssignInt
           , extAssignBool
           , extAssert
           , extIfthen
           , extWhile
           ]

extComm :: ParserH ExtComm
extComm = try $ whites >> many1 extComms >>= return . foldl1 ExtSeq

intExpToEval :: ParserH ExpToEval
intExpToEval = do
            keyword "evaluar" 

            inib <- statePos <$> getParserState
            i <- (intexp <?> "Expresión booleana")
            endb <- statePos <$> getParserState

            _ <- semip

            return $ Left ((makeCommPos inib endb), i)

boolExpToEval :: ParserH ExpToEval
boolExpToEval = do
            keyword "evaluar" 

            inib <- statePos <$> getParserState
            b <- (boolexp <?> "Expresión booleana")
            endb <- statePos <$> getParserState

            _ <- semip

            return $ Right ((makeCommPos inib endb), b)

expsToEval :: ParserH ExpsToEval
expsToEval = many1 $ choice $ map try $ [intExpToEval, boolExpToEval]

-- | Un programa consta de declaraciones de variables, una precondición,
--   un comando y una postcondición 
extProgram :: ParserH ExtProgram
extProgram = varinputs >>
             vardefs >>
             extComm >>= \c ->
             eof >>
             M.elems . pvars . stateUser <$> getParserState >>= \vars ->
             return $ ExtProg vars c

extEvalProgram :: ParserH ExtProgram
extEvalProgram = varinputs >>
                 vardefs >>
                 changePosEval <$> expsToEval >>= \toEval ->
                 eof >>
                 M.elems . pvars . stateUser <$> getParserState >>= \vars ->
                 return $ ExtEvalProg vars toEval

changePosEval :: ExpsToEval -> ExpsToEval
changePosEval [] = error "Imposible"
changePosEval (e:es) = snd (foldl f (pe,[e]) es)
    where pe :: CommPos
          pe = case e of
                    Left (p,_)  -> p
                    Right (p,_) -> p
          f :: (CommPos,ExpsToEval) -> ExpToEval -> (CommPos,ExpsToEval)
          f (p,es') (Left (p',e'))  = (p',es'++[Left (p,e')])
          f (p,es') (Right (p',e')) = (p',es'++[Right (p,e')])
              
                 
parseProgram :: ParserH ExtProgram
parseProgram = choice $ map try [extProgram,extEvalProgram]

-- | Función principal de parseo desde String
parseExtPrgFromString :: B.ByteString -> Either ParseError ExtProgram
parseExtPrgFromString = runParser parseProgram initSt "" .
                                               bytesToString . B.unpack
    where initSt = PHalState { lvars = M.empty
                             , pvars = M.empty
                             , equPState = PEqu.initPExprState PEqu.UnusedParen
                             }

parseExtPrgFromFile :: FilePath -> IO ExtProgram
parseExtPrgFromFile f = 
    B.readFile f >>= return . parseExtPrgFromString >>=
    either (error . show) return

parseExtPrgFromFile' :: FilePath -> IO (Either ParseError ExtProgram)
parseExtPrgFromFile' f = B.readFile f >>= return . parseExtPrgFromString 
