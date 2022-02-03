module Hal.Interpreter.Parser where

import qualified Data.Map as M
import Data.Text(pack,unpack)

import Control.Monad.Identity

-- Imports Parsec
import Text.Parsec
import Text.Parsec.Token

-- Imports Hal
import Hal.Parser
import Hal.Interpreter.Interpreter

-- Imports Equ
import qualified Equ.Parser as PEqu

parseIC :: String -> ICommand -> ParserH ICommand
parseIC s c = try $ sym s >> return c

parseLoad :: ParserH ICommand
parseLoad = try $ sym "load" >> program >>= \prg -> return (Load prg)

parseLoadFromFile :: ParserH ICommand 
parseLoadFromFile = try $ 
                    sym "loadfile" >> 
                    many (alphaNum <|> char '_' 
                                   <|> char '/'
                                   <|> char '.') >>= \str ->
                    return (LoadFile str)

parseRestart :: ParserH ICommand
parseRestart = parseIC "restart" Restart

parseStep :: ParserH ICommand
parseStep = parseIC "step" Step

parseView :: ParserH ICommand
parseView = parseIC "view" View

parseExit :: ParserH ICommand
parseExit = parseIC "exit" Exit

parseICommand :: ParserH ICommand
parseICommand =  parseLoad
             <|> parseLoadFromFile
             <|> parseRestart
             <|> parseStep
             <|> parseView
             <|> parseExit

parseICFromString :: String -> Either ParseError ICommand
parseICFromString = runParser parseICommand initSt "" 
    where initSt = PHalState { lvars = M.empty
                             , pvars = M.empty
                             , equPState = PEqu.initPExprState PEqu.UnusedParen
                             }
