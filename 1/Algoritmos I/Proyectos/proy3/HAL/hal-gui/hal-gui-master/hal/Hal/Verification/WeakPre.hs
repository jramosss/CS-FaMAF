module Hal.Verification.WeakPre where

import Hal.Verification.THoare
import Hal.Verification.VerCond
import Hal.Lang
import Hal.Parser(parseFromString,parseFromFile)

import qualified Equ.PreExpr as PE
import qualified Equ.Theories.FOL as FOL
import Equ.Expr
import Equ.Types

import qualified Data.Map as M


proofObligations :: Program -> [FormFun]
proofObligations = (map weakp) . verConditions 

-- Genera un archivo de fun con las obligaciones de prueba ingresadas 
-- como teoremas, para que puedan ser probadas en fun-gui.
generateFunFile :: String -> Program -> IO FilePath
generateFunFile pname prg =
    let pobs = proofObligations prg in
        writeFile (pname ++ ".fun") (proofOblgToTheoremStr pname pobs) >>
        return (pname ++ ".fun")

generateFunFileString :: String -> Program -> IO String
generateFunFileString pname prg =
    let pobs = proofObligations prg in
        return $ proofOblgToTheoremStr pname pobs

proofOblgToTheoremStr :: String -> [FormFun] -> String
proofOblgToTheoremStr pname pobs =
    "module " ++ pname ++ "\n\n" ++
                fst (foldl (\(s,i) f -> (s ++ (generateThm f i),i+1))
                        ("",0)
                        pobs)
                        
    where generateThm (Expr f) i = 
                "let thm \n\toblig" ++ (show i) ++ " = " ++ (PE.prettyShow f) ++
                     "\n\nbegin proof\n{- completar prueba -}\n\nend proof\n\n"
                     
weakp :: THoare -> FormFun
weakp th = FOL.impl (pre th) $ wp (comm th) (post th)


-- | Función que calcula la wakest precondition de un comando y una post-condición
wp :: GuardComm -> FormFun -> FormFun
wp [NGuard Skip] f = f
wp [NGuard Abort] _ = FOL.false
wp [NGuard (IAssig i e)] (Expr f) = Expr $ PE.applySubst f subst
    where 
        subst :: PE.ExprSubst
        subst = M.fromList [(vari,expToFun e)]
        vari :: PE.Variable
        vari = PE.var (idName i) (TyAtom ATyInt)
wp [NGuard (BAssig b e)] (Expr f) = Expr $ PE.applySubst f subst
    where 
        subst :: PE.ExprSubst
        subst = M.fromList [(varb,bExpToFun' e)]
        varb :: PE.Variable
        varb = PE.var (idName b) (TyAtom ATyBool)
wp (c1:gs) f = case c1 of
                    Guard b -> FOL.impl (bExpToFun b) (wp gs f)
                    NGuard c -> wp [c1] (wp gs f)

                    

-- | SOLO PARA PROBARRRR
ej s =
    let Right p = parseFromString (unlines s) in
        map weakp $ verConditions p

ej2 = parseFromFile "Examples/div.lisa" >>= \prg ->
      putStrLn $ show $ map weakp $ verConditions prg

ejFun sp name =
    let Right p = parseFromString (unlines sp) in
        generateFunFile name p
      
      