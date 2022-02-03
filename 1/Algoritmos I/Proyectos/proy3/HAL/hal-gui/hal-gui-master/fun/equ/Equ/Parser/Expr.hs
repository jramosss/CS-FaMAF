{-# Language Rank2Types #-}
-- | Este modulo es el parser a Pre-Expresiones. 
{--
Sobre Parsec

-- Informe de errores; Quisieramos poder marcar la posicion del error
e informacion bonita de cual fue el error. Parece ser que con
ParseError nos alcanza.

-- Funcion que determina el conName.

-- Hace falta hacer algun tipo de analisis para los tipos. Parseando
una funcion no hay problema debido a los constructores definidos en
las teorias pero que pasa con las constantes? por ejemplo, parseExpr
3; deberia quedar parseado con su tipo? que pasa si el usuario lo
especifico o no.  Resolucion: Parseamos con TyUnknown

-- Syntaxis de una escritura general; Como es una prueba bien escrita.
Se permiten comentarios?

-- Operadores de lista; Precedencia, de momento todos tienen la misma.

-- Libreria; criterion para testear rendimiento.
--}

module Equ.Parser.Expr
    (-- * Caracteres especiales comunes a todas las teorías
      LangDef (..)
    , ParserE
    , equLang
    -- * Funciones principales de parseo
    , parseFromString
    , parsePreExpr
    , parseVariable
    , parseVariableWithType
    , parser
    , parserVar
    , parsePattern
    , VarTy
    , initPExprState
    , EitherName
    , PExprState (..)
    , ParenFlag (..)
    , PExprStateClass (..)
    )
    where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Text.Parsec.Expr as PE

import Data.Text (pack,unpack)
import Data.List(group,sort)
import Data.Map (Map)
import Control.Monad.Identity
import Control.Applicative ((<$>),(<$),(<*>))


import Equ.Syntax
import Equ.PreExpr 
import Equ.Types
import Equ.Theories (operatorsList,constantsList,quantifiersList)
import Equ.Theories.List(listApp, listEmpty)
import Equ.Theories.Arith(intToCon)
import Equ.Parser.Types(listAtomTy)


import Equ.IndTypes (constrList)

type VarTy = (Int,Map EitherName Type)

type EitherName = Either VarName FuncName

data PExprState = PExprState { peParenFlag :: ParenFlag }

data ParenFlag = UseParen | UnusedParen

type ParserOper a = PE.Operator String a Identity PreExpr
type ParserTable a = PE.OperatorTable String a Identity PreExpr
-- a tiene que ser instancia PExprStateClass.
type ParserE a b = ParsecT String a Identity b

class PExprStateClass a where
    getExprState :: a -> PExprState
    setExprState :: a -> PExprState -> a
    
instance PExprStateClass PExprState where
    getExprState = id
    setExprState _ exprState = exprState

-- | Configuración del parser.
data LangDef = LangDef {
      quantInit :: String    -- ^ Inicio de una cuantificaci&#243;n.
    , quantEnd :: String     -- ^ Final de una cuantificaci&#243;n.
    , quantSep :: String     -- ^ Separador de la cuantificaci&#243;n.
    , opApp :: String        -- ^ Operador para la aplicaci&#243;n.
    , holeInfoInit :: String -- ^ Inicio de información en un hueco.
    , holeInfoEnd :: String  -- ^ Fin de información en un hueco.
    , opHole :: String       -- ^ Marca de un hueco.
    --, opUnCurriedApp :: String -- ^ Operador para aplicación uncurrificada.
    }

equLang :: LangDef
equLang = LangDef { 
            quantInit = "〈"
          , quantEnd = "〉" 
          , quantSep = ":"
          , opApp = "."
          , holeInfoInit = "{"
          , holeInfoEnd = "}"
          , opHole = "?"
          --, opUnCurriedApp = "%"
          }

-- | Representantes de los operadores. (Salvo para aplicación)
rOpNames :: [String]
rOpNames = map ($ equLang) [opApp, opHole] 
        ++ map (unpack . opRepr) operatorsList

-- | Representantes de las constantes y cuantificadores.
-- Adem&#225;s de los caracteres para representar expresiones cuantificadas.
rNames :: [String]
rNames = map ($ equLang) [quantInit,quantEnd,quantSep]
         ++ map (unpack . conRepr) constantsList
         ++ map (unpack . quantRepr) quantifiersList
         ++ listAtomTy

-- Para lexical analisys.
lexer' :: TokenParser u
lexer' = makeTokenParser $
            emptyDef { reservedOpNames = rOpNames
                     , reservedNames = rNames
                     , identStart  = letter
                     , identLetter = alphaNum <|> char '_'
                     --, opLetter = newline
                     }

lexer :: GenTokenParser String u Identity
lexer = lexer' { whiteSpace = oneOf " \t" >> return ()}

-- Parser principal de preExpresiones.
parsePreExpr :: PExprStateClass s => ParserE s PreExpr
parsePreExpr = getState >>= \st ->
               buildExprParser operatorTable (subexpr (peParenFlag $ getExprState st))
               <?> "Parser error: Expresi&#243;n mal formada"

-- Construimos la table que se le pasa a buildExpressionParser:
-- Primero agregamos el operador aplicaci&#243;n, que precede a cualquier otro
operatorTable :: PExprStateClass s => ParserTable s
operatorTable = [parserApp] : makeTable operatorsList
    where 
        parserApp = PE.Infix (App <$ reservedOp lexer (opApp equLang)) PE.AssocLeft

-- Genera un ParserTable de una lista de operadores.
makeTable :: PExprStateClass s => [Operator] -> ParserTable s
makeTable = map makeSubList . group . reverse . sort 

-- Genera un ParserOper de un operador.
makeOp :: PExprStateClass s => Operator -> [ParserOper s]
makeOp op = map mkop $ opRepr op : opGlyphs op
    where mkop s = case opNotationTy op of 
                     NInfix   -> PE.Infix   (BinOp op <$ parseOp s) assoc
                     NPrefix  -> PE.Prefix  $ UnOp op <$ parseOp s
                     NPostfix -> PE.Postfix $ UnOp op <$ parseOp s
          parseOp = reservedOp lexer . unpack
          assoc = convertAssoc . opAssoc $ op

makeSubList :: PExprStateClass s => [Operator] -> [ParserOper s]
makeSubList = concatMap makeOp

-- Convierte el nuestro tipo de asociaci&#243;n al tipo de asociaci&#243;n de parsec.
convertAssoc :: Assoc -> PE.Assoc
convertAssoc None = PE.AssocNone
convertAssoc ALeft = PE.AssocLeft
convertAssoc ARight = PE.AssocRight

-- Parseamos las subexpresiones
-- Una expresion puede ser una expresion con parentesis, una constante, una expresion cuantificada,
-- una variable, una funci&#243;n o una expresion que viene desde un parseo por syntactic sugar
subexpr :: ParenFlag -> PExprStateClass s => ParserE s PreExpr
subexpr flag =  parseSugarPreExpr parsePreExpr
            <|> parseParen <$> parens lexer parsePreExpr
            <|> Con <$> parseConst
            <|> parseQuant
            <|> parseIf
            <|> parseCase
            <|> Var <$> parseVariable
            <|> parseHole
    where
        parseParen :: (PreExpr -> PreExpr)
        parseParen = case flag of
                        UseParen -> Paren
                        UnusedParen -> id

-- Parser principal de preExpresiones.
parsePattern :: PExprStateClass s => ParserE s PreExpr
parsePattern = buildExprParser (makeTable constrList) subpattern
               <?> "Parser error: Expresi&#243;n mal formada"

subpattern :: PExprStateClass s => ParserE s PreExpr
subpattern = parens lexer parsePattern  
            <|> Con <$> parseConst
            <|> Var <$> parseVariable


-- Parseo de Constantes definidas en la teoria
-- Vamos juntando las opciones de parseo de acuerdo a cada constante en la lista.
-- En el caso en que la lista es vacia, la opcion es un error
parseConst :: PExprStateClass s => ParserE s Constant
parseConst = foldr ((<|>) . pConst) (fail "Constante") constantsList

pConst :: Constant -> ParserE u Constant
pConst c = c <$ (reserved lexer . unpack . conRepr) c

   
-- Parseo de Cuantificadores definidos en la teoria
parseQuant ::  PExprStateClass s => ParserE s PreExpr
parseQuant = foldr ((<|>) . pQuan) (fail "Cuantificador") quantifiersList

-- Funci&#243;n auxiliar para el parseo de quantificadores.
pQuan :: Quantifier -> PExprStateClass s => ParserE s PreExpr
pQuan q = try $ 
          symbol lexer (quantInit equLang) >>
          (symbol lexer . unpack . quantRepr) q >>
          (parseVariable <?> "Cuantificador sin variable") >>= 
          \v -> symbol lexer (quantSep equLang) >> parsePreExpr >>=
          \r -> symbol lexer (quantSep equLang) >> parsePreExpr >>=
          \t -> symbol lexer (quantEnd equLang) >> return (Quant q v r t)

-- Parseo de huecos.
parseHole :: PExprStateClass s => ParserE s PreExpr
parseHole = PrExHole . hole . pack <$> 
                try (reserved lexer (opHole equLang) >> braces lexer parseInfo)
            <|> fail "Hueco"

-- Parseo de la informacion de un hueco.
parseInfo :: PExprStateClass s => ParserE s String
parseInfo = many $ letter <|> oneOf " \t"

-- Parseo de if-then-else
parseIf :: PExprStateClass s => ParserE s PreExpr
parseIf = reserved lexer "if" >>
          parsePreExpr >>= \ cond ->
          reserved lexer "then" >>
          parsePreExpr >>= \ branchT ->
          reserved lexer "else" >>
          parsePreExpr >>= \ branchF ->
          reserved lexer "fi" >>
          return (If cond branchT branchF)
          
parseCase :: PExprStateClass s => ParserE s PreExpr
parseCase = reserved lexer "case" >>
            parsePreExpr >>= \ expr ->
            reserved lexer "of" >>
            manyTill1 parseCases (reserved lexer "end") >>= \ cases ->
            return (Case expr (reverse cases))
    where
        manyTill1 :: PExprStateClass s => ParserE s a -> PExprStateClass s => ParserE s end -> PExprStateClass s => ParserE s[a]
        manyTill1 p till = p >>= \ x ->
                           manyTill p till >>= \ xs ->
                           return (x:xs)
        parseCases :: PExprStateClass s => ParserE s (PreExpr,PreExpr)
        parseCases = parsePattern >>= \ p ->
                     reserved lexer "->" >>
                     parsePreExpr >>= \ ce ->
                     return (p,ce)

-- Esta funcion parsea una variable. Nos fijamos que empiece con
-- minuscula para distinguirla de las funciones (que empiezan con
-- mayuscula). 
parseVariableWithType :: Type -> ParsecT String u Identity Variable
parseVariableWithType ty = try $ 
                    lexeme lexer ((:) <$> letter <*> many alphaNum) >>= 
                    \v -> return (var (pack v) ty)

parseVariable :: ParsecT String u Identity Variable
parseVariable = parseVariableWithType TyUnknown

-- //////// Parser de syntax sugar ////////

-- Parseo de expresiones azucaradas.
parseSugarPreExpr :: PExprStateClass s => ParserE s PreExpr -> PExprStateClass s => ParserE s PreExpr
parseSugarPreExpr p = parseSugarList p <|> parseIntPreExpr

-- | Parseo de la lista escrita con syntax sugar.
sugarList :: PExprStateClass s => ParserE s PreExpr -> PExprStateClass s => ParserE s PreExpr
sugarList p = foldr (BinOp listApp) (Con listEmpty) <$> commaSep lexer p

-- | Parseo de la lista escrita con syntax sugar.
parseSugarList :: PExprStateClass s => ParserE s PreExpr -> PExprStateClass s => ParserE s PreExpr
parseSugarList = brackets lexer . sugarList

-- | Parseo de enteros.
parseInt :: PExprStateClass s => ParserE s Int
parseInt = fromInteger <$> natural lexer <?> fail "Numero"

-- | Parseo de enteros preExpr.
parseIntPreExpr :: PExprStateClass s => ParserE s PreExpr
parseIntPreExpr = intToCon <$> parseInt--parseInt >>= \e -> error (show $ intToCon e)
                 

-- //////// Parser de syntax sugar ////////

-- | Funcion principal de parseo desde string.
parseFromString' :: ParenFlag -> String -> Either ParseError PreExpr
parseFromString' flag = runParser parsePreExpr (initPExprState flag) "TEST"

parseFromString :: String -> Either ParseError PreExpr
parseFromString s = case parseFromString' UseParen s of
                         Left er -> Left er
                         Right pe -> Right $ unParen pe

initPExprState :: ParenFlag -> PExprState
initPExprState = PExprState
-- | Gramatica de parseo.
--
-- @
-- \<PreExpr\> ::= \<Atoms\>
--           | \<UnOp\>
--           | \<BinOp\>
--           | \<App\>
--           | \<Quant\>
--           | \<Parent\>
--           | if <PreExpr> then <PreExpr> else <PreExpr> fi
-- 
-- \<Var\> ::= {a, b, c, ... , z}*
-- \<Func\> ::= {A, B, C, ... , Z}*
-- \<Const\> ::= True | False | [] | 0
-- \<String\> ::= {a, b, c, ... , z, A, B, C, ... , Z}*
-- 
-- \<Atoms\> ::= \<Var\>
--          |  \<Func\>
--          |  \<Const\>
--          |  ?{\<String\>}
--
-- \<UnOp\> ::= &#172; \<PreExpr\>
--         |  # \<PreExpr\>
--         |  Succ \<PreExpr\>
--
-- \<BinOp \> ::= \<PreExpr> &#8743; \<PreExpr\>
--           |  \<PreExpr\> &#8744; \<PreExpr\>
--           |  \<PreExpr\> &#8658; \<PreExpr\>
--           |  \<PreExpr\> &#8656; \<PreExpr\>
--           |  \<PreExpr\> &#8801; \<PreExpr\>
--           |  \<PreExpr\> /&#8801; \<PreExpr\>
--           |  \<PreExpr\> = \<PreExpr\>
--           |  \<PreExpr\> &#9657; \<PreExpr\>
--           |  \<PreExpr\> &#8593; \<PreExpr\>
--           |  \<PreExpr\> &#8595; \<PreExpr\>
--           |  \<PreExpr\> . \<PreExpr\>
--           |  \<PreExpr\> ++ \<PreExpr\>
--           |  \<PreExpr\> + \<PreExpr\>
--           |  \<PreExpr\> * \<PreExpr\>
-- 
-- \<App\> ::= \<PreExpr\> \@ \<PreExpr\>
-- 
-- \<Quant\> ::= &#12296;&#8704;\<Var\> : \<PreExpr\> : \<PreExpr\>&#12297;
--          |  &#12296;&#8707;\<Var\> : \<PreExpr\> : \<PreExpr\>&#12297;
-- 
-- \<Parent\> ::= ( \<PreExpr\> )
-- @
parser :: String -> PreExpr
parser = either showError showPreExpr . parseFromString

parserVar :: ParenFlag -> String -> Either ParseError Variable
parserVar flag = runParser parseVariable (initPExprState flag) "TEST" 

-- Imprimimos el error con version Exception de haskell.
showError :: Show a => a -> b
showError = error . show

-- Imprimimos la preExpresion, usando que tenemos definido la instancia show.
showPreExpr :: a -> a
showPreExpr = id

buildExprParser :: Stream s m t => 
                    [[PE.Operator s u m b]] -> ParsecT s u m b -> ParsecT s u m b
-- buildExprParser :: Stream t t2 t3 =>
--                          [[PE.Operator t t1 t2 b]] -> ParsecT t t1 t2 b -> ParsecT t t1 t2 b
buildExprParser operators simpleExpr = foldl makeParser simpleExpr operators
    where
        initOps = ([],[],[],[],[])
        makeParser term ops = 
            let 
            (rassoc,lassoc,nassoc,prefix,postfix) = foldr splitOp initOps ops

            rassocOp   = choice rassoc
            lassocOp   = choice lassoc
            nassocOp   = choice nassoc
            prefixOp   = choice prefix  <?> ""
            postfixOp  = choice postfix <?> ""

            ambiguous assoc op = try $ do 
                                   _ <- op 
                                   fail ("ambiguous use of a " 
                                        ++ assoc 
                                        ++ " associative operator")

            ambiguousRight    = ambiguous "right" rassocOp
            ambiguousLeft     = ambiguous "left" lassocOp
            ambiguousNon      = ambiguous "non" nassocOp

            termP = do{ pre  <- prefixP
                      ; x    <- term
                      ; post <- postfixP
                      ; return (post (pre x))
                      }

            postfixP   = postfixOp <|> return id

            prefixP    = prefixOp <|> return id

            rassocP x  = do{ f <- rassocOp
                           ; y  <- do{ z <- termP
                                     ; rassocP1 z 
                                     }
                           ; return (f x y)
                           }
                          <|> ambiguousLeft
                          <|> ambiguousNon

            rassocP1 x = try (rassocP x)  <|> return x

            lassocP x  = do{ f <- lassocOp
                           ; y <- termP
                           ; lassocP1 (f x y)
                           }
                          <|> ambiguousRight
                          <|> ambiguousNon

            lassocP1 x = lassocP x <|> return x

            nassocP x = do { f <- nassocOp
                           ; y <- termP
                           ; ambiguousRight <|> ambiguousLeft <|> 
                             ambiguousNon   <|> return (f x y)
                           }
            
            -- Esto originalmente no existe en la versión original de
            -- buildExpressionParser, nosotros lo agregamos para poder
            -- parsear expresiones en pruebas.
            rlnOps x =  try (rassocP x) 
                    <|> try (lassocP x) 
                    <|> try (nassocP x) 
                    <|> return x
            
            in do { x <- termP
                  ; rlnOps x <?> "operator"
                  }

        splitOp (PE.Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
            = case assoc of
                PE.AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix)
                PE.AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix)
                PE.AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)

        splitOp (PE.Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
            = (rassoc,lassoc,nassoc,op:prefix,postfix)

        splitOp (PE.Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
            = (rassoc,lassoc,nassoc,prefix,op:postfix)
