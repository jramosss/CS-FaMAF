-- | Módulo de la sintáxis lenguaje imperativo simple con anotaciones (LISA).
{-# Language GADTs #-}
module Hal.Lang where

import qualified Data.Text as T

import Equ.Expr
import qualified Equ.PreExpr as PE
import qualified Equ.Types as ETy
import qualified Equ.Theories.FOL as ETheoriesF
import qualified Equ.Theories.Arith as ETheoriesA

import Hal.Symbols

type FormFun = Expr

type LIdentifier = [Identifier]

-- Tipo de dato básico para las expresiones.
data Type = IntTy | BoolTy
    deriving Eq

instance Show Type where
    show IntTy = "Int"
    show BoolTy = "Bool"

-- Lo usamos para determinar si un identificador es una variables o una
-- constante.
data IdType = IsVar | IsInput
    deriving (Eq,Show)

-- Identificador de variable y constante.
data Identifier = Identifier { idName     :: T.Text
                             , idDataType :: Type
                             , idType     :: IdType
                             }
                             
instance Show Identifier where
    show (Identifier i _ _) = T.unpack i
                             
instance Eq Identifier where
    i == i' = idName i == idName i'

-- Operadores binarios boleanos.
data BoolBOp = And | Or 

instance Show BoolBOp where
    show And = andSym
    show Or = orSym

-- Operadores unarios boleanos.
data BoolUOp = Not

instance Show BoolUOp where
    show Not = notSym

-- Operadores binarios enteros.
data IntBOp = Plus | Times | Substr | Div | Mod

instance Show IntBOp where
    show Plus = plusSym
    show Times = timesSym
    show Substr = substrSym
    show Div = divSym
    show Mod = modSym
    
-- Operadores unarios enteros
-- NO HAY

-- Relaciones binarias.
data RelOp = Equal | Lt

instance Show RelOp where
    show Equal = eqSym
    show Lt = ltSym

-- Expresiones enteras.
data Exp where
    IntId  :: Identifier -> Exp
    ICon   :: Int -> Exp
    
    IBOp :: IntBOp -> Exp -> Exp -> Exp
    
instance Show Exp where
    show (IntId i) = show i
    show (ICon i) = show i
    show (IBOp op e1 e2) = (show e1) ++ " " ++ (show op) ++ " " ++ (show e2)

-- Expresiones boleanas.
data BExp where
    BoolId :: Identifier -> BExp
    BCon   :: Bool -> BExp
    
    BBOp :: BoolBOp -> BExp -> BExp -> BExp
    BUOp :: BoolUOp -> BExp -> BExp
    
    BRel :: RelOp -> Exp -> Exp -> BExp
   
instance Show BExp where
    show (BoolId i) = show i
    show (BCon b) = show b
    show (BBOp op e1 e2) = (show e1) ++ " " ++ (show op) ++ " " ++ (show e2)
    show (BUOp op e) = (show op) ++ " " ++ (show e)
    show (BRel rel e1 e2) = (show e1) ++ " " ++ (show rel) ++ " " ++ (show e2)

-- Los terminos que representan los comandos.
data Comm where
    Skip  :: Comm
    Abort :: Comm
    
    Assert :: FormFun -> Comm
    
    If     :: BExp -> Comm -> Comm -> Comm
    
    IAssig :: Identifier -> Exp -> Comm
    BAssig :: Identifier -> BExp -> Comm
    
    Do     :: FormFun -> BExp -> Comm -> Comm
    Seq    :: Comm -> Comm -> Comm
    
instance Show Comm where
    show Skip = "skip"
    show Abort = "abort"
    show (Assert f) = "{" ++ (show f) ++ "}"
    show (If b c1 c2) = "if (" ++ (show b) ++ ")\n\tthen "++ (show c1) ++
                        "\n\telse " ++ (show c2)
    show (IAssig i e) = (show i) ++ ":= " ++ (show e)
    show (BAssig i e) = (show i) ++ ":= " ++ (show e)
    show (Do inv b c) = "while ("++ (show b) ++ ") {"++ (show inv) ++ "} " ++
                        "\n\tdo " ++ (show c) ++ "\n\tod"
    show (Seq c1 c2) = (show c1) ++ ";\n" ++ (show c2)
    
-- Un programa se separa en dos partes principales, la declaración de las
-- variables y los comandos en sí que conforman el programa.
data Program where
    Prog :: LIdentifier -> FormFun -> Comm -> FormFun -> Program

instance Show Program where
    show (Prog is pre c pos) =
        showvars ++ "\n{ " ++ (show pre) ++ " }\n" ++
        (show c) ++ "\n{ " ++ (show pos) ++ " }\n"
        
        where showvars =
                foldl (\s i -> s ++ "vardef " ++ (show i) ++ ":" ++ 
                        (show $ idDataType i) ++ "; ")
                      ""
                      is
    
funBoolType = ETy.TyAtom ETy.ATyBool

funNatType = ETy.TyAtom ETy.ATyNat
    
    
bExpToFun = Expr . bExpToFun'
    
bExpToFun' :: BExp -> PE.PreExpr
bExpToFun' (BoolId i) = PE.Var $ PE.Variable (idName i) funBoolType
bExpToFun' (BCon c) = PE.Con $ 
                     if c
                        then ETheoriesF.folTrue
                        else ETheoriesF.folFalse
bExpToFun' (BBOp op e1 e2) = 
    case op of
        And -> PE.BinOp ETheoriesF.folAnd (bExpToFun' e1) (bExpToFun' e2)
        Or -> PE.BinOp ETheoriesF.folOr (bExpToFun' e1) (bExpToFun' e2)
bExpToFun' (BUOp Not e) = 
    PE.UnOp ETheoriesF.folNeg (bExpToFun' e)
bExpToFun' (BRel rel e1 e2) = 
    case rel of
        Equal -> PE.BinOp ETheoriesF.folEqual (expToFun e1) (expToFun e2)
        Lt -> PE.BinOp ETheoriesA.lessOper (expToFun e1) (expToFun e2)
    
expToFun :: Exp -> PE.PreExpr
expToFun (IntId i) = PE.Var $ PE.Variable (idName i) funNatType
expToFun (ICon i) = intToFun i
    where intToFun i = if i<0
                        then PE.UnOp ETheoriesA.natNegNum $ expToFun (ICon i)
                        else case i of
                              0 -> PE.Con ETheoriesA.natZero
                              n -> PE.UnOp ETheoriesA.natSucc $ intToFun $ n-1
expToFun (IBOp op e1 e2) =
    case op of
         Plus -> PE.BinOp ETheoriesA.natSum (expToFun e1) (expToFun e2)
         Times -> PE.BinOp ETheoriesA.natProd (expToFun e1) (expToFun e2)
         Substr -> PE.BinOp ETheoriesA.natDif (expToFun e1) (expToFun e2)
         Div -> PE.BinOp ETheoriesA.natDiv (expToFun e1) (expToFun e2)
         Mod -> PE.BinOp ETheoriesA.natMod (expToFun e1) (expToFun e2)
