----------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  (c) Proyecto Theona, 2012-2013
--                (c) Alejandro Gadea, Emmanuel Gunther, Miguel Pagano
-- License     :  <license>
-- 
-- Maintainer  :  miguel.pagano+theona@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Modulo que define el tipos de estado para el parser de fun.
-- 
----------------------------------------------------------------------------
module Fun.Parser.Internal where

-- Imports Parsec
import Text.Parsec 
import Text.Parsec.Token
import Text.Parsec.Language

-- Imports Monad.
import Data.Functor.Identity

-- Imports Data.*
import Data.Text (Text)

-- Imports Equ.
import qualified Equ.Parser as EquP ( PProofState, PExprState
                                    , PExprStateClass (..)
                                    , PProofStateClass(..)
                                    )

-- Imports Fun.
import Fun.Declarations
import Fun.Decl
import Fun.Decl.Error (DeclPos(..))
import Fun.Module

import Control.Lens

type TextFilePath = Text

data PDeclState = PDeclState { pDecls :: Declarations
                             , pExprs :: EquP.PExprState
                             , pProofs :: EquP.PProofState
                             }

pdcls :: Lens' PDeclState Declarations
pdcls = lens pDecls (\st d -> st {pDecls = d})
                             
instance EquP.PExprStateClass PDeclState where
    getExprState = pExprs
    setExprState declst es = declst { pExprs = es }
    
instance EquP.PProofStateClass PDeclState where
    getProofState = pProofs
    setProofState declst ps = declst { pProofs = ps }
    
type ParserD a = ParsecT String PDeclState Identity a

lexer :: GenTokenParser String PDeclState Identity
lexer = lexer' { whiteSpace = oneOf " \t" >> return ()}
    where
        lexer' :: TokenParser PDeclState
        lexer' = makeTokenParser $ 
                    emptyDef { reservedNames = rNames
                             , commentStart = "{-"
                             , commentEnd = "-}"
                             , commentLine = "--"
                             , identStart  = alphaNum <|> char '_'
                             , identLetter = alphaNum <|> char '_'
                             , caseSensitive = True
                             }

-- | Nombres reservados de las declaraciones. 
rNames :: [String]
rNames = [ "let", "fun", "spec"
         , "thm", "prop", "val"
         , ":", "=", ".", "with"
         , "end", "import", "module"
         , "deriving", "from", "begin"
         , "proof"
         ]

keyword :: String -> ParserD ()
keyword  = reserved lexer
keywordModule :: ParserD ()
keywordModule = keyword "module"
keywordBegin :: ParserD ()
keywordBegin = keyword "begin"
keywordProof :: ParserD ()
keywordProof = keyword "proof"
keywordImport :: ParserD ()
keywordImport = keyword "import"
keywordVerified :: ParserD ()
keywordVerified = keyword "verified"
keywordFrom :: ParserD ()
keywordFrom = keyword "from"
keywordLet :: ParserD ()
keywordLet = keyword "let"
keywordDot :: ParserD ()
keywordDot = keyword "."
keywordWith :: ParserD ()
keywordWith = keyword "with"
keywordEnd :: ParserD ()
keywordEnd = keyword "end"
keywordBy :: ParserD ()
keywordBy = keyword "by"
keywordOn :: ParserD ()
keywordOn = keyword "on"
keywordCases :: ParserD ()
keywordCases = keyword "cases"
keywordCase :: ParserD ()
keywordCase = keyword "case"
keywordRArrow :: ParserD ()
keywordRArrow = keyword "->"
keywordRecursion :: ParserD ()
keywordRecursion = keyword "recursion"
keywordDefSymbol :: ParserD ()
keywordDefSymbol = keyword defSymbol
keywordAppSymbol :: ParserD ()
keywordAppSymbol = string appSymbol >> return ()

defSymbol :: String
defSymbol = "="

appSymbol :: String
appSymbol = "."

whites :: ParserD ()
whites = whiteSpace lexer

tryNewline :: ParserD ()
tryNewline = try newline >> return ()

lineComment :: ParserD ()
lineComment = try $ symbol lexer "--" >> manyTill anyChar newline >> return ()

blockComment :: ParserD ()
blockComment = try $ do
                _ <- symbol lexer "{-"
                _ <- manyTill anyChar endIn
                return ()
    where
        endIn = (try eof >> error "Esperando -}") <|> try (symbol lexer "-}")

manyTillWithEnd :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> 
                   ParsecT s u m ([a],end)
manyTillWithEnd p ending = scan
    where
        scan =  do{ e <- ending; return ([],e) }
            <|> do{ x <- p; xs <- scan; return (x:fst xs,snd xs)}

currPos :: ParserD SourcePos
currPos = getParserState >>= return . statePos

position :: ModName -> SourcePos -> ParserD DeclPos
position m b = currPos >>= \e -> return (DeclPos b e m)

parseWithPos :: ModName -> ParserD a -> ParserD (Annot a)
parseWithPos m p = currPos >>= \b ->
                   p >>= \a ->
                   position m b >>= \pos -> 
                   return (pos,a)
