{-# Language Rank2Types,OverloadedStrings #-}
-- | Este modulo es el parser de pruebas.
module Equ.Parser.Proof ( parsePfFromString'
                        , rel 
                        , proof
                        , parseFromFileProof
                        , initPProofState
                        , PProofState
                        , pTheoSet
                        , PProofStateClass (..)
                        ) where

import Equ.Parser.Expr
import Equ.Syntax hiding (Hole)
import Equ.Expr (Expr(..))
import Equ.PreExpr ( PreExpr'(..)
                   , PreExpr
                   , toFocus
                   , Focus
                   , toExpr
                   )

import Equ.Proof (validateProof)
import Equ.Proof.Proof ( Proof'(..)
                       , Ctx
                       , Basic (..) 
                       , Theorem (..)
                       , Axiom (..)
                       , Hypothesis (..)
                       , getEnd 
                       , getStart
                       , beginCtx
                       , createVacuousHyp
                       , addHypothesis'
                       , getCtx
                       , setCtx
                       , Proof
                       , instanciateInCtx)
import Equ.Proof.Condition (noCondition)
import Equ.Theories ( axiomGroup
                    , createHypothesis)
import Equ.Rule hiding (rel)

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Data.Text(Text,pack,unpack,intercalate)
import Data.Maybe
import qualified Data.Map as M (Map,empty,lookup) 

import Control.Monad.Identity
import Control.Applicative ((<$>),(<$),(<*>))

type ProofName = Text

type TheoSet = M.Map ProofName Theorem


data PProofState = PProofState { pTheoSet :: TheoSet
                               , pExprSt :: PExprState
                               }

instance Show PProofState where
    show = show . pTheoSet

class PProofStateClass a where
    getProofState :: a -> PProofState
    setProofState :: a -> PProofState -> a

instance PExprStateClass PProofState where
    getExprState = pExprSt
    setExprState s exprState = s { pExprSt = exprState }

instance PProofStateClass PProofState where
    getProofState = id
    setProofState _ pst = pst

type ParserP a b = ParsecT String a Identity b

-- | Parsea un final de prueba que comienza con begin proof.
parseProofEnd :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
parseProofEnd = many newline >> keywordEnd >> keywordProof

-- | Hace un lookup de un teorema declarado antes en base a su nombre.
getDeclProof :: (PExprStateClass s, PProofStateClass s) => 
                ProofName -> ParserP s (Maybe Theorem)
getDeclProof pn = do
                pst' <- getState
                let pst = getProofState pst'
                let theoSet = pTheoSet pst
                case M.lookup pn theoSet of
                    (Just mproof) -> return $ Just mproof
                    _ -> return Nothing

lexer :: (PExprStateClass s, PProofStateClass s) =>
         GenTokenParser String s Identity
lexer = lexer' { whiteSpace = oneOf " \t" >> return ()}
    where
        lexer' :: (PExprStateClass s, PProofStateClass s) => TokenParser s
        lexer' = makeTokenParser $ 
                    emptyDef { reservedNames = rNames
                             , identStart  = alphaNum <|> char '_'
                             , identLetter = alphaNum <|> char '_'
                             , caseSensitive = False
                             }

-- | Nombres reservados.
rNames :: [String]
rNames = [ "proof"
         , "with"
         , "for"
         , "cases"
         , "->"
         , "where"
         , "induction"
         , "in"
         , "begin"
         , "end"
         , "basic"
         , "exhaustive"
         ]

whites :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
whites = whiteSpace lexer

keyword :: (PExprStateClass s, PProofStateClass s) => String -> ParserP s ()
keyword  = reserved lexer
keywordBegin :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordBegin = keyword "begin"
keywordBasic :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordBasic = keyword "basic"
keywordExhaustive :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordExhaustive = keyword "exhaustive"
keywordEnd :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordEnd = keyword "end"
keywordSBOpen :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordSBOpen = try $ symbol lexer "[" >> symbol lexer "~" >> return ()
keywordSBClose :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordSBClose = try $ symbol lexer "~" >> symbol lexer "]" >> return ()
keywordDots :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordDots = symbol lexer ":" >> return ()
keywordComma :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordComma = symbol lexer "," >> return ()
keywordProof :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordProof = keyword "proof"
keywordCases :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordCases = keyword "cases"
keywordInduc :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordInduc = keyword "induction"
keywordFor :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordFor = keyword "for"
keywordIn :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordIn = keyword "in"
keywordDot :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordDot = keyword "."
keywordRArrow :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordRArrow = keyword "->"
keywordWith :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordWith = keyword "with"
keywordWhere :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
keywordWhere = keyword "where"

-- | Parsea nombres de declaración de teoremas.
parseProofName :: (PExprStateClass s, PProofStateClass s) => ParserP s Text
parseProofName = intercalate (pack " ") <$> parseName

-- | Parsea nombres de declaración de hipotesis.
parseDeclHypName :: (PExprStateClass s, PProofStateClass s) => ParserP s Text
parseDeclHypName = intercalate (pack " ") <$> parseName

-- | Parsea nombres de hipotesis en el contexto de una justificación.
parseHypName :: (PExprStateClass s, PProofStateClass s) => ParserP s Text
parseHypName = intercalate (pack " ") <$> parseName

-- | Parsea un nombre.
parseOneName :: (PExprStateClass s, PProofStateClass s) => ParserP s Text
parseOneName = lexeme lexer ((:) <$> lower <*> many alphaNum) >>= return . pack

-- | Parsea nombres.
parseName :: (PExprStateClass s, PProofStateClass s) => ParserP s [Text]
parseName =  many1 parseName'
    where
        parseName' :: (PExprStateClass s, PProofStateClass s) => ParserP s Text
        parseName' = foldr (\str -> (<|>) (failKeyword str)) parseOneName rNames
            where
                failKeyword str = keyword str >> unexpected (show str)
                
tryNewline :: (PExprStateClass s, PProofStateClass s) => ParserP s ()
tryNewline = newline >> return ()

                
-- | Parser de un axioma sin tener en cuenta la teoría.
axiomUnQual :: (PExprStateClass s, PProofStateClass s) => ParserP s Axiom
axiomUnQual = (choice . map axiomInList . concatMap snd) axiomGroup

-- | Construcción del parser a partir del axioma.
axiomInList :: (PExprStateClass s, PProofStateClass s) => Axiom -> ParserP s Axiom
axiomInList ax = try $ (string . unpack . axName) ax >> return ax


-- | Uso del parser de una expresión definido en 'Equ.Parser.Expr'.
-- Ale: No esta bonito como manejamos el pasaje de errores con pass
-- ademas pasa que tenemos que re-acomodar la posición del error.
-- Algo raro es que la posición de la linea siempre esta un lugar mas "adelante"
parseFocus :: (PExprStateClass s, PProofStateClass s) => 
              ParserP s Focus
parseFocus = toFocus <$> parsePreExpr 

-- | Parser de una justificación inmediata de un paso de prueba.
basic :: (PExprStateClass s, PProofStateClass s) => ParserP s Basic
basic =  Ax   <$> axiomUnQual
     <|> Theo <$> parseTheo
     <|> Hyp  <$> parseHyp
    where
        parseHyp :: (PExprStateClass s, PProofStateClass s) => 
                    ParserP s Text
        parseHyp = try $ parseHypName
                    
        parseTheo :: (PExprStateClass s, PProofStateClass s) => ParserP s Theorem
        parseTheo = try $
                    do
                    n <- parseProofName
                    mp <- getDeclProof n
                    maybe (fail $ theoErr n) return mp
        theoErr :: ProofName -> String
        theoErr n = "Prueba del teorema: " ++ show (unpack n)

-- | Parser de entidades entre llaves.
braced :: (PExprStateClass s, PProofStateClass s) => ParserP s a -> ParserP s a
braced = between (string "{" >> spaces) (spaces >> string "}" )

-- | Parser de un paso de prueba.
justification :: (PExprStateClass s, PProofStateClass s) => 
                 ParserP s (Relation, Maybe Basic)
justification = rel >>= \r -> spaces >>
                optionMaybe (braced basic) >>= \j -> 
                return (r, j)

-- | Parsea todas las pruebas de un archivo y retorna una lista con estas mismas.
prooflist :: (PExprStateClass s, PProofStateClass s) => Maybe Ctx -> 
             ParserP s [Proof]
prooflist mc = many $ proof mc True 

-- | Parser de declaraciones de hipótesis.
parseHypothesis :: (PExprStateClass s, PProofStateClass s) => 
                    ParserP s [Hypothesis]
parseHypothesis = between keywordSBOpen keywordSBClose parseHyps
    where
        parseHyps :: (PExprStateClass s, PProofStateClass s) => 
                     ParserP s [Hypothesis]
        parseHyps = sepBy parseHyp keywordComma
        parseHyp :: (PExprStateClass s, PProofStateClass s) => 
                     ParserP s Hypothesis
        parseHyp = do
                n <- parseDeclHypName
                keywordDots
                f <- parseFocus
                return $ createHypothesis n (Expr $ toExpr f) noCondition

-- | Parser de pruebas.
proof :: (PExprStateClass s, PProofStateClass s) => Maybe Ctx -> 
         Bool -> ParserP s Proof
proof mc flag = do
        _ <- many newline
        when flag keywordBegin
        when flag keywordProof
        if flag then parseProof else transProof ctx flag
    where
        ctx :: Ctx
        ctx = fromMaybe beginCtx mc
        makeCtx :: (PExprStateClass s, PProofStateClass s) =>
                   Maybe [Hypothesis] -> ParserP s Ctx
        makeCtx = maybe (return ctx) (return . foldr addHypothesis' ctx)
        
        parseProof :: (PExprStateClass s, PProofStateClass s) => ParserP s Proof
        parseProof = 
            parsePrefix >>= \mhyps -> many newline >>
            makeCtx mhyps >>= \ctxWithHyps ->
            choice [ inducProof ctxWithHyps
                   , casesProof ctxWithHyps
                   , transProof ctxWithHyps flag
                   ]
        parsePrefix :: (PExprStateClass s, PProofStateClass s) =>
                        ParserP s (Maybe [Hypothesis])
        parsePrefix = 
                (parseProofHyps >>= \hres -> return hres)
                <|>
                return Nothing
        
        parseProofHyps :: (PExprStateClass s, PProofStateClass s) => 
                          ParserP s (Maybe [Hypothesis])
        parseProofHyps = parseHypothesis >>= return . Just

-- | Parseo de una prueba inductiva.
inducProof :: (PExprStateClass s, PProofStateClass s) => Ctx -> ParserP s Proof
inducProof ctx = do
            keywordInduc
            keywordIn
            fInduc <- parseFocus
            keywordFor
            fei <- parseFocus
            keywordDot
            rels <- manyTill rel keywordDot
            when (null rels) (fail "No relation")
            let relOp = head rels
            fef <- parseFocus
                        
            keywordWhere
            cs <- parseInducCases (toExpr fInduc)
            parseProofEnd
            iproof <- return $ Ind ctx relOp fei fef fInduc cs 
            return iproof
    where
        parseInducCases:: (PExprStateClass s, PProofStateClass s) => 
                           PreExpr -> ParserP s [(Focus,Proof)]
        parseInducCases (Var indv) = do
                    keywordBasic
                    c <- parseCases ctx indv
                    cs <- manyTill (parseCases ctx indv) (many newline >> keywordInduc)
                    patt <- parseFocus
                    keywordWith
                    nameHyp <- parseOneName
                    let hypInds = createVacuousHyp nameHyp
                    _ <- many whites
                    keywordRArrow
                    p <- proof (Just $ addHypothesis' hypInds ctx) False
                    return ((c:cs) ++ [(patt,p)])
        parseInducCases _ = fail "No es una variable!"

-- | Parseo de una prueba por casos.
-- TODO: Es igual a la de arriba, pero esto tal vez vaya a cambiar, así que 
-- espero para acomodarla.
casesProof :: (PExprStateClass s, PProofStateClass s) => Ctx -> ParserP s Proof
casesProof ctx = do
        keywordCases
        keywordIn
        fc <- parseFocus
        keywordFor
        fei <- parseFocus
        keywordDot
        rels <- manyTill rel keywordDot
        when (null rels) (fail "No se pudo parsear una relación")
        let relOp = head rels
        fef <- parseFocus
        keywordWhere
        (cs, mPEx) <- manyTillWithEnd (parseCases ctx undefined) (endExhaustive <|> endProof)
        let cs' = map (\ p -> (fst p, fromJust $ addHypothesisCase p)) cs
        return (Cases ctx relOp fei fef fc cs' mPEx)
    where
        endExhaustive :: (PExprStateClass s, PProofStateClass s) => 
                         ParserP s (Maybe Proof)
        endExhaustive = do
                        pExh <- parseExhaustive
                        parseProofEnd
                        return $ Just pExh
        endProof :: (PExprStateClass s, PProofStateClass s) => 
                    ParserP s (Maybe Proof)
        endProof = parseProofEnd >> return Nothing
        parseExhaustive :: (PExprStateClass s, PProofStateClass s) => 
                            ParserP s Proof
        parseExhaustive = keywordExhaustive >> keywordRArrow >>
                          proof (Just beginCtx) False
        addHypothesisCase :: (Focus,Proof) -> Maybe Proof
        addHypothesisCase (f,p) =
            let hyp = createHypothesis "Caso" 
                                        (Expr $ toExpr f) 
                                        noCondition
            in getCtx p >>= \ctx' ->
               setCtx (addHypothesis' hyp ctx') p
                    
manyTillWithEnd :: (Stream s m t) => ParsecT s u m a -> 
                    ParsecT s u m end -> ParsecT s u m ([a],end)
manyTillWithEnd p end = scan
    where
        scan =  do{ e <- end; return ([], e) }
            <|> do{ x <- p; xs <- scan; return (x:fst xs,snd xs)}

-- -- | Parsea casos, de la forma expr -> transProof
parseCases :: Ctx -> Variable -> (PExprStateClass s, PProofStateClass s) => 
              ParserP s (Focus, Proof)
parseCases ctx v = do
            fi <- parseFocus
            keywordRArrow
            let ctx' = instanciateInCtx ctx v fi
            p <- proof (Just ctx) False  -- TODO: está bien esto? Debería ser ctx' ?
            return (fi,p)
            
-- | Parser de pruebas transitivas, estan incluidas las pruebas simples.
transProof :: Ctx -> Bool -> (PExprStateClass s, PProofStateClass s) => 
              ParserP s Proof
transProof ctx flag = do
                      _ <- many whites
                      e1 <- parseFocus
                      mkTrans ctx e1 <$> manyExprLine
    where
        parseStep :: (PExprStateClass s, PProofStateClass s) => 
                     ParserP s (Focus,(Relation, Maybe Basic))
        parseStep = do
                    rj <- justification
                    _ <- many (whites <|> tryNewline)
                    e <- parseFocus
                    return (e,rj)
        manyExprLine :: (PExprStateClass s, PProofStateClass s) => 
                        ParserP s [(Focus,(Relation, Maybe Basic))]
        manyExprLine = do 
                        frb <- parseStep
                        frbs <- if flag
                               then manyTill parseStep parseProofEnd
                               else many parseStep
                        return (frb:frbs)

-- | Parser de la relación sobre la que estamos haciendo la prueba.
rel :: (PExprStateClass s, PProofStateClass s) => ParserP s Relation
rel = foldr ((<|>) . uncurry prel) parserZero relations
    where prel iden ty = ty <$ try (many whites >> string iden)
          relations = [("=",relEq), ("≡",relEquiv), ("⇒",relImpl), ("⇐",relCons)]

-- | Parser de prueba.
parsePfFromString' :: String -> Either ParseError [Proof]
parsePfFromString' = either handleError Right . runParser 
                            (prooflist Nothing) 
                            (initPProofState M.empty initPES) ""
    where
        initPES = initPExprState UnusedParen
        -- Esto esta pensando en que hay que hacer algo para obtener bien
        -- la posición del error.
        handleError :: ParseError -> Either ParseError [Proof]
        handleError = Left 

-- | Parsea una prueba desde un archivo.
parseFromFileProof :: FilePath -> IO ()
parseFromFileProof fp = readFile fp >>= \s -> 
                        case parsePfFromString' s of
                            Right ps -> putStrLn $ unlines [ "-------"
                                                           , show ps
                                                           , "-------"
                                                           , show $ map validateProof ps
                                                           ]
                            Left err -> putStrLn $ show err

-- | Estado inicial del parser de pruebas.
initPProofState :: TheoSet -> PExprState -> PProofState
initPProofState theoSet estate = PProofState theoSet estate

{- Pasar estas funciones a Equ.Proof -}
-- | construcción de una prueba simple.
mkSimple :: Ctx -> Relation -> Focus -> Focus -> Maybe Basic -> Proof
mkSimple c r e e' = maybe (Hole c r e e') (Simple c r e e')

-- | Construcción de una prueba transitiva; estas pruebas son
-- up-leaning, en el sentido que construyen pruebas donde el último
-- paso es simple mientras que todos los demás son transitivos.
mkTrans :: Ctx -> Focus -> [(Focus,(Relation, Maybe Basic))] -> Proof
mkTrans _ _ [] = error "impossible"
mkTrans c e ((e',(r,j)):[]) = mkSimple c r e e' j
mkTrans c e ((e',(r,j)):steps) = go (mkSimple c r e e' j) steps
    where 
        go :: Proof -> [(Focus,(Relation, Maybe Basic))] -> Proof
        go p [] = p
        go p ((f,(r', j')):ps) = go prf' ps
            where 
                e0 = fromJust (getStart p)
                e1 = fromJust (getEnd p)
                prf' = Trans c r' e0 e1 f p (mkSimple c r' e1 f j')

