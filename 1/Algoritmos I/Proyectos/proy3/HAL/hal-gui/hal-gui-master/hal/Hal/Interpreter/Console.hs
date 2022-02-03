module Hal.Interpreter.Console where

-- Imports Monad
import System.IO
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT,get,put,execStateT)

-- Imports de hal
import Hal.Lang
import Hal.Parser
import Hal.Interpreter.Parser
import Hal.Interpreter.Interpreter

runConsole :: InterpreterState ()
runConsole = liftIO (putStr lineMessage >> hFlush stdout) >> 
             liftIO getLine >>= \str ->
             case parseICFromString str of
                Left er -> liftIO (putStrLn $ show er) >> runConsole
                Right ic -> case ic of
                                Exit -> return ()
                                LoadFile str -> loadfile str
                                Step -> evalInterpreter ic >>= \v ->
                                        maybe (return ()) (liftIO . putStrLn) v >>
                                        evalInterpreter View >>= \v ->
                                        maybe (return ()) (liftIO . putStrLn) v >>
                                        runConsole
                                _ -> evalInterpreter ic >>= 
                                     maybe (return ()) (liftIO . putStrLn) >>
                                     runConsole

loadfile :: String -> InterpreterState ()
loadfile str =  liftIO (parseFromFile' str) >>= \eprg ->
                case eprg of
                   Left er   -> liftIO (putStrLn $ show er) >> runConsole
                   Right prg -> liftIO (putStrLn $ loadPrgMessage prg) >>
                                evalInterpreter (Load prg) >> runConsole
                                
lineMessage :: String
lineMessage = "Eval hal> "

loadPrgMessage :: Program -> String
loadPrgMessage prg = unlines [ "Carga exitosa"
                             , ""
                             , show prg
                             ]

prettyHal :: String
prettyHal = unlines [ "     _   _   ______  _"
                    , "    / / / / /____ / / /"
                    , "   / /_/ / ____/ / / /"
                    , "  / __  / / __  / / /"
                    , " / / / / / /_/ / / /"
                    , "/_/ /_/ /_____/ /_/"
                    ]

initialMessage :: String
initialMessage = unlines [ ""
                         , "Bienvenido al inteprete de hal."
                         , prettyHal
                         , ""
                         , "loadfile [NombreArchivo] | Carga un programa para ejecutar."
                         , "load [Program]           | Carga un programa para ejecutar."
                         , "restart                  | Reinicia la ejecución de un programa cargado."
                         , "step                     | Evalua un paso del programa"
                         , "view                     | Muestra el estado de la ejecución de un programa."
                         , "exit                     | Salir."
                         ]

main :: IO ()
main = putStrLn initialMessage >>
       execStateT runConsole initIState >> return ()
