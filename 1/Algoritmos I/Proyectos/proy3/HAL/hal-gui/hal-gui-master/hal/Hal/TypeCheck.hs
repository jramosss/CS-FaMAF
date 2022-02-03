module Hal.TypeCheck where

import qualified Control.Monad.State as MS
import qualified Data.List as L

import Hal.Lang

-- Los errores de tipeo.
data TypeCheckError = TCError
    deriving Show

-- Contexto para el checkeo de tipos.
type Ctx = [Identifier]

-- El estado del chequeo de tipos.
type TypeCheckState = [TypeCheckError]

-- La mónada para el chequeo de tipos.
type TypeCheck = MS.StateT TypeCheckState IO

dtypeUnify :: Type -> Type -> Bool
dtypeUnify = (==)

typeCheckAcc :: Acc -> Ctx -> TypeCheck (Maybe Type)
typeCheckAcc (IdAcc i) ctx = 
            if idType i == IsVar 
                then return $ maybe (error "Impossible") 
                                    (return . ATy . idDataType) $ L.find (==i) ctx
                else typeCheckPutErr TCError >> return Nothing

typeCheckExpr :: Expr -> Ctx -> TypeCheck (Maybe Type)
typeCheckExpr (IdExpr i) ctx = return $ maybe (error "Impossible") 
                                              (Just . ATy . idDataType) $ L.find (==i) ctx
typeCheckExpr (ICon _) ctx = return $ Just $ ATy IntTy
typeCheckExpr (BCon _) ctx = return $ Just $ ATy BoolTy
typeCheckExpr (Op op args) ctx = checkTypeArgs (opType op) args
    where
        checkTypeArgs :: Type -> [Expr] -> TypeCheck (Maybe Type)
        checkTypeArgs dt [] = return $ Just dt
        checkTypeArgs (dt :-> dtt) (e:es) = typeCheckExpr e ctx >>= \mdt ->
                case mdt of
                    Nothing -> return Nothing
                    Just edt -> MS.unless (dtypeUnify dt edt) (typeCheckPutErr TCError) >> 
                                checkTypeArgs dtt es
        checkTypeArgs _ (e:es) = MS.get >>= \errs -> MS.put (errs ++ [TCError]) >> 
                                 return Nothing

typeCheckPutErr :: TypeCheckError -> TypeCheck ()
typeCheckPutErr er = MS.get >>= \errs -> MS.put (errs ++ [er])

tcTypeCheck :: Maybe Type -> Type -> 
               TypeCheckError -> TypeCheck () -> TypeCheck ()
tcTypeCheck mdt dt err tc = case mdt of
                                Nothing -> return ()
                                Just dt' -> if dtypeUnify dt dt'
                                                then tc
                                                else typeCheckPutErr err

typeCheckComm :: Comm -> Ctx -> TypeCheck ()
typeCheckComm Skip ctx = return ()
typeCheckComm Abort ctx = return ()
typeCheckComm (Assert b) ctx = typeCheckExpr b ctx >>= \mdt ->
                                tcTypeCheck mdt (ATy BoolTy) TCError (return ())
typeCheckComm (If b c c') ctx = typeCheckExpr b ctx >>= \mdt ->
                                tcTypeCheck mdt (ATy BoolTy) TCError 
                                                (typeCheckComm c ctx >>
                                                 typeCheckComm c' ctx)
typeCheckComm (Assig a e) ctx = 
                typeCheckExpr e ctx >>= \mdt ->
                typeCheckAcc a ctx >>= \madt ->
                case (mdt,madt) of
                    (Nothing,_) -> return ()
                    (_,Nothing) -> return ()
                    (Just edt,Just adt) -> if dtypeUnify adt edt
                                            then return ()
                                            else MS.get >>= \errs -> 
                                                 MS.put (errs ++ [TCError])
typeCheckComm (Do inv b c) ctx = typeCheckExpr b ctx >>= \mdt ->
                                 tcTypeCheck mdt (ATy BoolTy) TCError (typeCheckComm c ctx)
typeCheckComm (Seq c c') ctx = typeCheckComm c ctx >> typeCheckComm c' ctx 
