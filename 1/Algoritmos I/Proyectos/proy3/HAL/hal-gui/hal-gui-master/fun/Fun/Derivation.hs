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
-- Define la noción de derivación de una función en base a una
-- especificación y una prueba, es decir, una derivación propia de la
-- función.
-- 
----------------------------------------------------------------------------
module Fun.Derivation (
      Derivation (..)
    , module Fun.Derivation.Error
    , createDerivations
    , checkDerivation
    )
    where

import Fun.Decl
import Fun.Decl.Error
import Fun.Declarations
import Fun.Derivation.Derivation
import Fun.Derivation.Error

import Equ.Proof
import Equ.Expr
import Equ.Rule (getRelationFromType,Relation)
import Equ.TypeChecker (getType)
import Equ.Theories

import Equ.Syntax
import qualified Equ.PreExpr as PE

import qualified Data.Text as T hiding (map,foldl)
import Data.List as L (map, find)
import Data.Either 
import qualified Data.Map as M

import Data.Monoid

import Control.Arrow ((***),second)
import Control.Monad
import Control.Lens

-- | A partir de las declaraciones, crea los objetos "Derivation" juntando la informacion
--   de cada especificación con la correspondiente derivación y def de función en caso que
--   ocurra. Por ahora no se puede hacer más de una derivación de una misma especificación.
--   Además, la especificación de una derivación debe estar EN EL MISMO módulo que la derivación.
createDerivations:: Declarations -> [EDeriv]
createDerivations decls = foldl (createDeriv spcs fncs) [] drvs
    where drvs = decls ^. derivs 
          spcs = bare specs decls
          fncs = bare functions decls


createDeriv :: [SpecDecl] -> [FunDecl] -> [EDeriv] -> (DeclPos,DerivDecl) -> [EDeriv]
createDeriv pSpecs pFuncs prevDerivs (derPos,der) = der':prevDerivs
    where  der' = alreadyDefined der (rights prevDerivs) >>
                  maybe (Left ([NotSpecification],der)) newDer 
                        (L.find (sameDecl der) pSpecs)
           newDer sp = Right $ Derivation der derPos sp 
                              $ L.find (sameDecl der) pFuncs

-- | Verifica que no haya derivaciones duplicadas.
alreadyDefined :: DerivDecl -> [Derivation] -> EDeriv' ()
alreadyDefined derDecl = mconcat . map (checkRedef derDecl . deriv)

-- | Controla si ya existe una derivación para la misma función.
checkRedef :: (Decl d) => d -> DerivDecl -> EDeriv' ()
checkRedef d d' = when (sameDecl d d') $ Left ([RedefinedDerivation $ getNameDecl d'],d')

-- | Funcion que dada una derivacion dice si es válida o no. Esto es
--   solo para las derivaciones por recursión. Si luego se implementa
--   otro tipo de derivación, entonces debería diferenciarse.
checkDer :: Declarations -> Declarations -> [ThmDecl] -> Derivation -> EDeriv' (Annot FunDecl)
checkDer decls imDecls thms der = do
               
                let (declS,mDeclF,declD) = (spec der,prog der,deriv der) 
                -- primero chequeamos que la variable sobre la que se hace
                -- la derivación por recursión es la misma que está definida en
                -- la especificación.
                    v = declD ^. derVar
                    f = declD ^. derFun
                    pfsnt = declD ^. derCases
                    vname = tRepr v
                    spVars = L.map tRepr $ declS ^. specArgs
                    expr = declS ^. specSpec
                    (Just texpr) = getType expr
                    rel = getRelationFromType texpr
                whenDer (vname `elem` spVars) ([InvalidVariable  v],declD)
                -- Remplazamos la variable por la de la especificación, ya que ésta está
                -- tipada.
                vspec <- getVarInSpec v declS declD
                let declD' = Deriv f vspec pfsnt
                -- Ahora construimos una prueba inductiva con los datos de la derivación.
                -- Se asume que la declaración está bien tipada
                    fexpr = firstExpr declS
                    caseExpr = caseExprFromDerivation vspec declD'
                --  agregar como hipotesis la especificacion
                    hypSpec = mkIndHyp f rel fexpr expr
                    pfs' = addHypInSubProofs hypSpec pfsnt
                    ctx = addHypothesis' hypSpec M.empty
                    prf' = Ind ctx rel (PE.toFocus fexpr) 
                                       (PE.toFocus caseExpr) 
                                       (PE.toFocus $ PE.Var vspec) 
                                       pfs'
                     -- Agregamos todas las declaraciones como hipotesis                                       
                    prf = addDeclHypothesis decls thms imDecls prf'
                    valPrf = validateProof prf
                    funPos = derivPos der
                    derivedFun = createFunDecl declS vspec declD'
                _ <- maybe (return ()) (isDeclared declD' derivedFun) mDeclF
                
                case valPrf of
                     Left err -> Left ([ProofNotValid err],declD')
                     Right _ -> return (funPos,derivedFun)
                                

addHypInSubProofs :: Hypothesis -> [(PE.Focus,Proof)] -> [(PE.Focus,Proof)]
addHypInSubProofs hyp = L.map (second updCtx)
     where updCtx p = let Right ctx = getCtx p 
                          ctx' = addHypothesis' hyp ctx
                          Right p' = setCtx ctx' p 
                      in p'

isDeclared :: DerivDecl -> FunDecl -> FunDecl -> Either ([DerivationError], DerivDecl) ()
isDeclared der derF declF = when (not (isEq derF declF)) $ 
                                Left ([DerivedFunctionDeclaredNotEqual (derF ^. funDeclName)],der)


mkIndHyp :: Variable -> Relation -> PE.PreExpr -> PE.PreExpr -> Hypothesis
mkIndHyp fun rel fexpr expr = createHypothesis name hypExpr (GenConditions [])
         where hypExpr = Expr $ PE.BinOp (relToOp rel) fexpr expr
               name = T.concat [T.pack "spec ",tRepr fun]

checkDerivation :: Declarations -> Declarations -> [ThmDecl] -> 
                   EDeriv -> EDeriv' (DeclPos,FunDecl)
checkDerivation decls imDecls thms = either Left (checkDer decls imDecls thms) 

getVarInSpec :: Variable -> SpecDecl -> DerivDecl -> EDeriv' Variable
getVarInSpec v spc derDecl = getVarInSpec' v (spc ^. specArgs)
    where getVarInSpec' v' [] = Left ([InvalidVariable v'],derDecl)
          getVarInSpec' v' (w:ws) = if v' == w
                                    then return w
                                    else getVarInSpec' v' ws


firstExpr :: SpecDecl ->  PE.PreExpr
firstExpr spDecl = exprApplyFun (spDecl ^. specName) (spDecl ^. specArgs)
    where exprApplyFun :: Variable -> [Variable] -> PE.PreExpr
          exprApplyFun f = foldl (\e -> PE.App e . PE.Var) (PE.Var f)

createFunDecl :: SpecDecl -> Variable -> DerivDecl -> FunDecl
createFunDecl specDecl v derDecl = Fun fun vars expr Nothing
    where vars = specDecl ^. specArgs
          fun  = specDecl ^. specName
          expr = caseExprFromDerivation v derDecl

caseExprFromDerivation :: Variable -> DerivDecl -> PE.PreExpr
caseExprFromDerivation v derDecl = PE.Case pattern cases
   where pattern = PE.Var v
         pfs = derDecl ^. derCases
         cases = L.map (PE.toExpr *** PE.toExpr . finalExpr) pfs
         finalExpr = fromRight . getEnd 
         fromRight = either (error "fromRight Left") id
