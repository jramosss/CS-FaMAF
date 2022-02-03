-- | Parser del Lenguaje Hal
module Hal.Parser where

import qualified Data.Map as M
import Data.Text(pack,unpack)

import Control.Monad.Identity

-- Imports Parsec
import Text.Parsec 
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr (Operator(..), Assoc(..), OperatorTable, buildExpressionParser)
import Text.Parsec.Error (ParseError)

-- Imports de Equ
import qualified Equ.Parser as PEqu
import qualified Equ.Expr as Equ


--Imports de Hal
import Hal.Lang
import Hal.Symbols

-- | Estado del parser. Todavía no queda claro si pueden ocurrir variables libres
--   en un programa.
data PHalState = PHalState {
                        pvars :: M.Map String Identifier
                      , lvars :: M.Map String Identifier
                      , equPState :: PEqu.PExprState
}

type ParserH a = ParsecT String PHalState Identity a

instance PEqu.PExprStateClass PHalState where
    getExprState phs@PHalState{ pvars = _
                              , lvars = _
                              , equPState = eps
                              } = eps
                              
    setExprState phs st = phs { equPState = st }

-- |Operadores enteros
intOperators :: [String]
intOperators = [plusSym,substrSym,timesSym,divSym,modSym]

-- | Operadores booleanos
boolOperators :: [String]
boolOperators = [andSym,orSym,notSym]

-- | Operadores de relación
relOperators :: [String]
relOperators = [eqSym,ltSym]

operators :: [String]
operators = intOperators++boolOperators++relOperators

-- |Palabras reservadas del lenguaje.
boolConsts, reservedCmd, keywords :: [String]
boolConsts = ["True", "False"]
reservedCmd = [ "abort","skip","if","then",
                "else","fi","while","do","od",
                "vardef","Pre","Post","Bool", "Int"
              ]
keywords = reservedCmd ++ boolConsts

-- |Aquí configuramos las opciones de análisis sintáctico para el
-- lenguaje.
lang :: GenTokenParser String PHalState Identity
lang = makeTokenParser $
        emptyDef { reservedOpNames = operators
                 , reservedNames = keywords
                 , identStart  = lower <|> char '_'
                 , identLetter = alphaNum <|> char '_'
                 , opStart  = (oneOf . map head) operators
                 , opLetter = (oneOf . concat) operators
                 , caseSensitive = True
                 }

-- |Símbolos léxicos.
sym :: String -> ParserH String
sym = symbol lang

-- |Enteros
int :: ParserH Int
int = integer lang >>= return . fromInteger

paren :: ParserH a -> ParserH a
paren = parens lang

-- |Parsea ';'.
semip :: ParserH String
semip = semi lang

-- |Identificadores (variables y etiquetas de fallas).
ident :: ParserH String
ident = identifier lang <?> "Identificador"


-- |Palabra reservada.
keyword :: String -> ParserH ()
keyword  = reserved lang

-- |Operador reservado (en nuestro caso son todos).
oper :: String -> ParserH ()
oper s = sym s >> return ()

-- |Si @p :: ParserH a@, entonces @brac p@ parsea lo mismo que @p@ pero
-- dentro de llaves ('{' y '}').
brac :: ParserH a -> ParserH a
brac = braces lang

-- |Construye el parser de un operador binario infijo.
binop :: String -> (a -> a -> a) -> Assoc -> Operator String PHalState Identity a
binop s f assoc = Infix (oper s >> return f <?> "operator") assoc

-- |Construye el parser de un operador unario prefijo.
prefix :: String -> (a -> a) -> Operator String PHalState Identity a
prefix s f = Prefix (oper s >> return f <?> "operator")

-- *** Parsers auxiliares definidos en este módulo.

-- |Parser de expresiones enteras con símbolos de relación.
rop :: String -> RelOp -> ParserH BExp
rop s op = try $ intexp >>= \e -> oper s >> intexp >>= \e' -> 
                 return (BRel op e e')

-- |Consume carácteres de espacio en blanco.
whites :: ParserH ()
whites = whiteSpace lang

-- ** Parsers.

-- *** Expresiones enteras.

-- |Variables de programa.
pvar :: Type -> ParserH Identifier
pvar ty = try $ ident >>= \s ->
          getParserState >>= \st ->
          maybe (parserFail $ "Variable " ++ s ++" no definida")
                (\i -> checkType s ty i)
                (M.lookup s (pvars $ stateUser st))

    where checkType s ty i = 
                if ty==idDataType i
                    then return i
                    else parserFail $ "Variable " ++ s ++ " no tiene tipo " ++ show ty

pintvar :: ParserH Identifier
pintvar = pvar IntTy

pboolvar :: ParserH Identifier
pboolvar = pvar BoolTy

intvarExp :: ParserH Exp
intvarExp = pintvar >>= return . IntId

boolvarExp :: ParserH BExp
boolvarExp = pboolvar >>= return . BoolId

-- |Constantes enteras (esto considera los positivos y los negativos).
intcon :: ParserH Exp
intcon = int >>= return . ICon

-- |Factores de expresiones enteras
intatom :: ParserH Exp
intatom = paren intexp <|> intvarExp <|> intcon

-- |Operadores enteros.
intops :: OperatorTable String PHalState Identity Exp
intops = [[ binop timesSym (IBOp Times) AssocLeft, binop divSym (IBOp Div) AssocLeft 
          , binop modSym (IBOp Mod) AssocLeft]
         ,[ binop plusSym (IBOp Plus) AssocLeft, binop substrSym (IBOp Substr) AssocLeft]
         ]

-- |Expresiones enteras
intexp :: ParserH Exp
intexp = buildExpressionParser intops intatom <?> "Expresión entera"

-- *** Expresiones booleanas.
-- |Operadores de relación.
relexps :: [ParserH BExp]
relexps = [ rop eqSym Equal
          , rop ltSym Lt
          ]

-- |Operadores booleanos.
boolops :: OperatorTable String PHalState Identity BExp
boolops = [[ prefix notSym (BUOp Not)]
          ,[ binop andSym (BBOp And) AssocLeft, binop orSym (BBOp Or) AssocLeft]
          ]

-- |Constantes booleanas.
boolcon :: ParserH BExp
boolcon = (choice . map sym) boolConsts >>= return . BCon . read

-- |Átomos booleanos.
boolatom  :: ParserH BExp
boolatom = paren boolexp <|> choice relexps <|> boolcon <|> boolvarExp

-- |Expresiones booleanas.
boolexp :: ParserH BExp
boolexp = buildExpressionParser boolops boolatom <?> "Expresión booleana"

-- *** Comandos.
-- |Comando simples.
single :: String -> Comm -> ParserH Comm
single s c = try $ sym s >> return c

-- | Skip
skip :: ParserH Comm
skip = single "skip" Skip

-- | Abort
abort :: ParserH Comm
abort = single "abort" Abort

-- |Asignación.
assignInt :: ParserH Comm
assignInt = try $ 
            pintvar >>= \acc ->
            oper ":=" >> 
            intexp >>= return . IAssig acc

assignBool :: ParserH Comm
assignBool = try $ 
             pboolvar >>= \acc ->
             oper ":=" >> 
             boolexp >>= return . BAssig acc

-- |Condicional.      
ifthen :: ParserH Comm
ifthen = try $
         keyword "if" >> (boolexp <?> "Expresión booleana") >>= 
         \b -> keyword "then" >> comm >>= 
         \c -> keyword "else" >> comm >>=
         \c' -> keyword "fi" >> return (If b c c')

-- | Fórmulas de Fun
formfun :: ParserH FormFun
formfun = try $
          sym "{" >>
          whites >>
          PEqu.parsePreExpr >>= return . Equ.Expr >>= \e ->
          whites >>
          sym "}" >>
          return e

-- | Assert
assert :: ParserH Comm
assert = try $
         formfun >>=
         return . Assert

-- | Do - While
while :: ParserH Comm
while = try $
        whites >>
        keyword "while" >> 
        boolexp >>=
        \b -> 
        formfun >>= \form ->
        keyword "do" >> comm >>=
        \c -> keyword "od" >> return (Do form b c)

-- | Secuencia
seqc :: ParserH Comm
seqc = try $
      whites >>
      comm >>= \c ->
      whites >>
      sym ";" >>
      whites >>
      comm >>= \c' ->
      return (Seq c c')

-- | Comandos del lenguaje LISA
comms :: [ParserH Comm]
comms = [ skip 
        , abort
        , assignInt
        , assignBool
        , assert
        , ifthen
        , while
        ]

comm :: ParserH Comm
comm = try $ whites >> sepEndBy1 (choice comms) semip >>= return . foldl1 Seq
       
-- | Precondición
prec :: ParserH FormFun
prec = try $ 
       sym "{" >>
       whites >>
       keyword "Pre" >>
       sym ":" >>
       whites >>
       PEqu.parsePreExpr >>= return . Equ.Expr >>= \e ->
       whites >>
       sym "}" >>
       return e
       
-- | Postcondición
post :: ParserH FormFun
post = try $
       sym "{" >>
       whites >>
       keyword "Post" >>
       sym ":" >>
       whites >>
       PEqu.parsePreExpr >>= return . Equ.Expr >>= \e ->
       whites >>
       sym "}" >>
       return e
       
var :: String -> IdType -> ParserH ()
var prefix idtype = try $
         whites >>
         keyword prefix >>
         ident >>= \s ->
         sym ":" >>
         ptype >>= \ty ->
         return Identifier { idName = pack s
                           , idDataType = ty
                           , idType = idtype
         }
         >>= \ids ->
         updState s ids >>
         return ()
    where updState s ids = updateParserState (\st -> st { stateUser = 
                                    (stateUser st) { pvars = M.insert s ids (pvars $ stateUser st)} } )
       
varinput :: ParserH ()
varinput = var "varinput" IsInput

vardef :: ParserH ()
vardef = var "vardef" IsVar

seqvardef :: ParserH ()
seqvardef = try $
            vardefs >>
            sym ";" >>
            vardefs
                   
varinputs :: ParserH ()
varinputs = try $
          whites >> 
          sepEndBy varinput semip >>
          return ()

vardefs :: ParserH ()
vardefs = try $
          whites >> 
          sepEndBy vardef semip >>
          return ()

-- | Parser de tipos
ptype :: ParserH Type
ptype = (keyword "Bool" >>
        return BoolTy)
        <|> 
        (keyword "Int" >>
        return IntTy)
          
-- | Un programa consta de declaraciones de variables, una precondición, un comando y una postcondición 
program :: ParserH Program
program = varinputs >>
          vardefs >>
          prec >>= \pre ->
          comm >>= \c ->
          post >>= \post ->
          getParserState >>= return . M.elems . pvars . stateUser >>=
          \vars ->
          return $ Prog vars pre c post
          
-- | Función principal de parseo desde String
parseFromString :: String -> Either ParseError Program
parseFromString = runParser program initSt "" 
    where initSt = PHalState { lvars = M.empty
                             , pvars = M.empty
                             , equPState = PEqu.initPExprState PEqu.UnusedParen
                             }
                             
parseFromFile :: FilePath -> IO Program
parseFromFile f = 
    readFile f >>= return . parseFromString >>=
    either (error . show) return

parseFromFile' :: FilePath -> IO (Either ParseError Program)
parseFromFile' f = readFile f >>= return . parseFromString
    
parseConFromString :: String -> Either ParseError Exp
parseConFromString = runParser intcon initSt "" 
    where initSt = PHalState { lvars = M.empty
                             , pvars = M.empty
                             , equPState = PEqu.initPExprState PEqu.UnusedParen
                             }

parseBConFromString :: String -> Either ParseError BExp
parseBConFromString = runParser boolcon initSt "" 
    where initSt = PHalState { lvars = M.empty
                             , pvars = M.empty
                             , equPState = PEqu.initPExprState PEqu.UnusedParen
                             }
                             
-- *** EXAMPLES

prg1 = ["{Pre: True}",
         "skip;",
         "{Post: True}"
       ]
       
prg2 = [" vardef x: Bool",
         "{Pre: x ≡ True}",
         "x := False && x;",
         "skip",
         "{Post: x ≡ False}"
       ]
       
prg3 = [" vardef x: Int;",
         "vardef y: Int",
         "{Pre: x = 0 ∧ y = 0}",
         "x := x + 1;",
         "y := x + 1;",
         "{Post: y = 2}"
       ]
         
prg4 = [ " vardef x: Int;",
         " vardef y: Bool",
         "{Pre: (x == X) ∧ (y == True)}",
         " while not (not y) { y == True } ",
         " do ",
         " x := x - 1;",
         " { x < X }; ",
         " y := x < 0; ",
         " abort",
         " od",
         "{Post: False}"
         ]

        
prg5 = [ " vardef x: Int;",
         "{Pre: ¬ (x < 0)}",
         " if (x < 0) ",
         "    then skip ",
         "    else abort",
         "    fi",
         "{Post: True}"
       ]

       
prg6 = [ " vardef x: Int;",
         " vardef y: Bool",
         "{Pre: True}",
         " while not (not y) { y == True } ",
         " do ",
         " x := x - 1;",
         " y := x > 0; ",
         " od",
         "{Post: True}"
         ]

         
{- | En el caso siguiente hay un problema.
        { Pre }
        S1
        if b
            then S2
            else S3
       { Q }
       
       debe generar las siguientes wp:
       
       P => wp.S1.(B => wp.S2.Q) y
       P => wp.S1.(not B => wp.S3.Q)
         
       pero tal como lo tenemos ahora estaría generando:
       
       P && B => wp.S1.(wp.S2.Q) y
       P && not B => wp.S1.(wp.S3.Q)
         
-}


         
prg7 = [ " vardef x: Int;",
         "{Pre: True}",
         " x:= 0;",
         " if (x < 0) ",
         "    then skip ",
         "    else abort",
         "    fi",
         "{Post: True}"
       ]
         
         
         