{-# LANGUAGE OverloadedStrings #-}
module HGUI.Evaluation.TranslateLang where

import HGUI.ExtendedLang
import Hal.Lang
import HGUI.Evaluation.EvalState ( StateTuple (..) )

import qualified Language.Syntax as AS
import qualified Language.Semantics as ASem
import qualified Language.ListAssoc as LA

import qualified Data.Map as M

import Data.Text ( unpack )
import Control.Arrow ((***))

type VarToId = [StateTuple] -- M.Map AS.AS.VarName Identifier


-- Traduce del lenguaje del proyecto al ExtendedLanguage
syntaxToBE :: AS.BoolExpr -> VarToId -> BExp
syntaxToBE (AS.ConstB b) _  = BCon b
syntaxToBE (AS.VB v)     m  = BoolId (lookupVar v m)
syntaxToBE (AS.And b1 b2) m = 
                   let (b1',b2') = (syntaxToBE b1 m, syntaxToBE b2 m)
                   in BBOp And b1' b2'
syntaxToBE (AS.Or b1 b2) m =
                   let (b1',b2') = (syntaxToBE b1 m, syntaxToBE b2 m)
                   in BBOp Or b1' b2'
syntaxToBE (AS.Not b) m =
                   let b' = syntaxToBE b m
                   in BUOp Not b'
syntaxToBE (AS.Equal e1 e2) m =
                   let (e1',e2') = (syntaxToIE e1 m, syntaxToIE e2 m)
                   in BRel Equal e1' e2'
syntaxToBE (AS.Less e1 e2) m =
                   let (e1',e2') = (syntaxToIE e1 m, syntaxToIE e2 m)
                   in BRel Lt e1' e2'
        
syntaxToIE :: AS.IntExpr -> VarToId -> Exp
syntaxToIE (AS.ConstI i) _ = ICon i
syntaxToIE (AS.VI v)     m = IntId (lookupVar v m)
syntaxToIE (AS.Neg e)    m = let e' = syntaxToIE e m
                             in IBOp Substr (ICon 0) e'
syntaxToIE (AS.Plus e1 e2) m =
                       let (e1',e2') = (syntaxToIE e1 m, syntaxToIE e2 m)
                       in IBOp Plus e1' e2'
syntaxToIE (AS.Prod e1 e2) m =
                       let (e1',e2') = (syntaxToIE e1 m, syntaxToIE e2 m)
                       in IBOp Times e1' e2'
syntaxToIE (AS.Div e1 e2) m =
                       let (e1',e2') = (syntaxToIE e1 m, syntaxToIE e2 m)
                       in IBOp Div e1' e2'
syntaxToIE (AS.Mod e1 e2) m =
                       let (e1',e2') = (syntaxToIE e1 m, syntaxToIE e2 m)
                       in IBOp Mod e1' e2'
      
lookupVar :: AS.VarName -> VarToId -> Identifier
lookupVar v [] = error $ "La variable " ++ v ++ " no existe."
lookupVar v (IntVar ident _ : rest)  | v == unpack (idName ident) = ident 
                                     | otherwise          = lookupVar v rest
lookupVar v (BoolVar ident _ : rest) | v == unpack (idName ident) = ident 
                                     | otherwise          = lookupVar v rest

-- Chequea que el comando que se devuelve desde el proyecto
-- corresponda con el que deberia ser.
syntaxToEC :: AS.Statement -> ExtComm -> VarToId -> ExtComm
syntaxToEC AS.Skip (ExtSkip p) _ = ExtSkip p
syntaxToEC (AS.AssignB (AS.Var v AS.BoolT) e) (ExtBAssig p _ _) m = 
                             ExtBAssig p (lookupVar v m) (syntaxToBE e m)

syntaxToEC (AS.AssignI (AS.Var v AS.IntT) e) (ExtIAssig p _ _) m = 
                             ExtIAssig p (lookupVar v m) (syntaxToIE e m)

syntaxToEC (AS.Seq s1 s2) (ExtSeq s1' s2') m =
                             ExtSeq (syntaxToEC s1 s1' m) (syntaxToEC s2 s2' m)

syntaxToEC (AS.Do be s) (ExtDo p f _ s') m =
                             ExtDo p f (syntaxToBE be m) (syntaxToEC s s' m)

syntaxToEC (AS.If cs) (ExtIf p ecs) m = ExtIf p (ifCont cs ecs m)
    where ifCont :: [(AS.BoolExpr,AS.Statement)] -> [(CommPos,BExp,ExtComm)] ->
                    VarToId -> [(CommPos,BExp,ExtComm)]
          ifCont [] [] _ = []
          ifCont ((be,s):cs') ((p',_,es):ecs') m' =
              (p',syntaxToBE be m',syntaxToEC s es m') : (ifCont cs' ecs' m')
          ifCont _ _ _ = error $ unwords [ "La definición de continuación de" 
                                         , "ejecución no es la correcta."
                                         ]

syntaxToEC _ _ _ = error $ unwords [ "La definición de continuación de" 
                                   , "ejecución no es la correcta."
                                   ]

contToMEC :: ASem.Continuation -> Maybe ExtComm -> VarToId -> Maybe ExtComm
contToMEC (ASem.ToExec stmt) (Just comm) m = Just (syntaxToEC stmt comm m)
contToMEC (ASem.Finish) Nothing _ = Nothing
contToMEC _ _ _ = error $ unwords [ "La definición de continuación de ejecución"
                                  , "no es la correcta."
                                  ]

-- Traducción del lenguaje de Hal al del proyecto                        
beToSyntax :: BExp -> AS.BoolExpr
beToSyntax (BoolId i)  = AS.VB (unpack $ idName i)
beToSyntax (BCon b)    = AS.ConstB b
beToSyntax (BBOp op b1 b2) = 
    case op of
         And -> AS.And (beToSyntax b1) (beToSyntax b2)
         Or  -> AS.Or (beToSyntax b1) (beToSyntax b2)
beToSyntax (BUOp Not b) = AS.Not (beToSyntax b)
beToSyntax (BRel rel e1 e2) =
    case rel of
         Equal -> AS.Equal (ieToSyntax e1) (ieToSyntax e2)
         Lt    -> AS.Less (ieToSyntax e1) (ieToSyntax e2)

ieToSyntax :: Exp -> AS.IntExpr
ieToSyntax (IntId i) = AS.VI (unpack $ idName i)
ieToSyntax (ICon e)  = AS.ConstI e
ieToSyntax (IBOp op e1 e2) =
    let sop = case op of
                  Plus   -> AS.Plus 
                  Times  -> AS.Prod
                  Substr -> (\ie1 -> AS.Plus ie1 . AS.Neg)
                  Div    -> AS.Div
                  Mod    -> AS.Mod
    in
        sop (ieToSyntax e1) (ieToSyntax e2)
                     
                     
ecToSyntax :: ExtComm -> AS.Statement
ecToSyntax (ExtSkip _) = AS.Skip
ecToSyntax (ExtBAssig _ i e) = 
    AS.AssignB (AS.Var vari AS.BoolT) (beToSyntax e)
    where vari = unpack $ idName i
ecToSyntax (ExtIAssig _ i e) = 
    AS.AssignI (AS.Var vari AS.IntT) (ieToSyntax e)
    where vari = unpack $ idName i
ecToSyntax (ExtDo _ _ be s) =
    AS.Do (beToSyntax be) (ecToSyntax s)
ecToSyntax (ExtSeq s1 s2) = AS.Seq (ecToSyntax s1) (ecToSyntax s2)
ecToSyntax (ExtIf _ cs) = AS.If (foldl (\ls (_,be,s) -> ls ++ [(beToSyntax be,ecToSyntax s)]) [] cs)
ecToSyntax e = error $ "No existe esta sentencia en la sintaxis del proyecto: " ++ show e
            
            
-- Traducción del estado de Hal al estado del proyecto
stHalToSt' :: [StateTuple] -> (M.Map AS.VarName Int,M.Map AS.VarName Bool)
stHalToSt' = foldl f (M.empty,M.empty)
    where f :: (M.Map AS.VarName Int,M.Map AS.VarName Bool) -> StateTuple -> 
               (M.Map AS.VarName Int,M.Map AS.VarName Bool)
          f (sti,stb) (IntVar i mint) = 
              let v = (unpack $ idName i)
              in case mint of
                    Nothing -> (M.insert v ASem.defaultIntValue sti,stb)
                    Just e  -> (M.insert v e sti,stb)
          f (sti,stb) (BoolVar i mbool) = 
              let v = (unpack $ idName i)
              in case mbool of
                    Nothing -> (sti,M.insert v ASem.defaultBoolValue stb)
                    Just e  -> (sti,M.insert v e stb)

stHalToSt :: [StateTuple] -> ASem.State
stHalToSt = (mapToLAssoc *** mapToLAssoc) . stHalToSt'
    where mapToLAssoc :: M.Map a b -> LA.ListAssoc a b
          mapToLAssoc m = lpairToLAssoc (M.toList m)
          lpairToLAssoc :: [(a,b)] -> LA.ListAssoc a b
          lpairToLAssoc [] = LA.Empty
          lpairToLAssoc ((a,b):ls) = LA.Node a b (lpairToLAssoc ls)
                    
stToStHal :: ASem.State -> VarToId -> [StateTuple]
stToStHal (sti,stb) m = 
    let stires = M.foldlWithKey fi [] (laToMap sti)
    in
        M.foldlWithKey fb stires (laToMap stb)
    where fi :: [StateTuple] -> AS.VarName -> Int -> [StateTuple]
          fi st v i = let ident = lookupVar v m 
                      in
                          (IntVar ident (Just i) : st)
          fb :: [StateTuple] -> AS.VarName -> Bool -> [StateTuple]
          fb st v b = let ident = lookupVar v m
                      in
                          (BoolVar ident (Just b) : st)

          laToMap :: (Ord a) => LA.ListAssoc a b -> M.Map a b
          laToMap = M.fromList . laToList
          laToList :: LA.ListAssoc a b -> [(a,b)]
          laToList LA.Empty = []
          laToList (LA.Node a b l) = (a,b):laToList l
          
        
    
