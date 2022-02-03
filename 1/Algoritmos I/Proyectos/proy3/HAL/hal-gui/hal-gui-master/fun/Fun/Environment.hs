 -------------------------------------------------------------------------
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
-- Environment es el conjunto de módulos que se tienen cargados en un
-- momento dado en Fun. Cada vez que se hace un import desde un
-- módulo, debe referirse a un módulo que se encuentre en el
-- environment.
-- 
----------------------------------------------------------------------------
{-# Language DoAndIfThenElse,TemplateHaskell #-}
module Fun.Environment where

import Fun.Module
import Fun.Module.Graph
import Fun.Module.Error
import Fun.Parser 
import Fun.Decl 
import Fun.Declarations
import Fun.Verification
import Fun.Derivation
import Fun.TypeChecker

import Equ.Syntax
import Equ.Types

import Data.Either (lefts,rights,partitionEithers)
import qualified Data.List as L (map)
import Data.Text (unpack,pack)
import Data.Graph.Inductive
import Data.List(find)
import Data.Monoid(mconcat)
import System.FilePath.Posix

import Control.Lens
import Control.Arrow ((&&&))
import Control.Monad (foldM,(>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State

import qualified Control.Exception as C

type Environment = [Module]

data StateCM = StateCM { _imMGraph   :: ImModuleGraph
                       , _modulesEnv :: Environment
                       }
    deriving Show

$(makeLenses ''StateCM)

type CheckModule a = StateT StateCM IO a

initStateCM :: ImModuleGraph -> Environment -> StateCM
initStateCM = StateCM

-- No usar este añadir si se quiere actualizar el módulo.
addModuleEnv :: Module -> Environment -> Environment
addModuleEnv m env = if m `elem` env then env else m : env

-- | Chequea la lista de módulos en el environment.
checkEnvModules :: CheckModule (Maybe ModuleError)
checkEnvModules = get >>= foldM proceed Nothing . (^. modulesEnv)
    where proceed so m = maybe (checkModule m) (return . Just) so

-- | Chequea un módulo del environment.
checkModule :: Module -> CheckModule (Maybe ModuleError)
checkModule m = do
    env     <- use modulesEnv 
    imGraph <- use imMGraph
    
    let rImports = reachableImports (m ^. modName) imGraph
    let mImports = filter ((`elem` rImports) . (^. modName)) env
    
    let mImportedDecls = mconcat $ map (^. validDecls) mImports

    let ass = getTypesAss mImports
    let tcRes = typeCheckDecls m ass
    let m' = either (const m) id tcRes
    let tcErr = either show (const "") tcRes
    let invalidSpec = lefts $ checkSpecs (m' ^. validDecls ) mImportedDecls
    let invalidFuns = lefts $ checkFuns  (m' ^. validDecls) mImportedDecls

    let invalidVals = lefts $ checkVals  (m' ^. validDecls) mImportedDecls



    let thmsCheck = checkThm (m' ^. validDecls) mImportedDecls
    let invalidThms  = lefts thmsCheck

    -- Si hay derivaciones sin especificación, o derivaciones
    -- repetidas, entonces la lista eDerivs tendrá errores de
    -- derivación.
    let eDerivs = createDerivations (m' ^. validDecls)
        
    let validThms = rights thmsCheck ++ bareThms mImportedDecls
    let checkedDerivs = partitionEithers $ 
             L.map (checkDerivation (m' ^. validDecls) mImportedDecls validThms) eDerivs
    
    let eVerifs = createVerifications (m' ^. validDecls) mImportedDecls
    let checkedVerifs = partitionEithers $ L.map checkVerification eVerifs
    
    let inDeclarations = InvalidDeclarations [] [] invalidThms [] [] (fst checkedDerivs)

    let warning (inVerif,verif) ders = updateModuleEnv .
                                       execState (do validDecls %= updateValidDecls inDeclarations ders;
                                                     verifications .= verif;
                                                     invalidDecls .= InvalidDeclsAndVerifs inDeclarations inVerif)


    case (invalidSpec, invalidFuns, invalidVals) of
          ([],[],[]) -> warning checkedVerifs (snd checkedDerivs) m'
          (e1,e2,e3)  -> return . Just $ createError (m' ^. modName) e1 e2 e3 invalidThms (fst checkedVerifs) (fst checkedDerivs) tcErr

                           
updateValidDecls :: InvalidDeclarations -> [Annot FunDecl] -> Declarations -> Declarations
updateValidDecls ind nf vd = over functions (++ nf) $ filterValidDecls vd ind 

updateModuleEnv :: Module -> CheckModule (Maybe a)
updateModuleEnv m = modulesEnv %= map update >> return Nothing
    where update :: Module -> Module
          update m' = if m == m' then m else m'

-- | Dado un nombre de módulo, comienza la carga buscado en el archivo
-- correspondiente al módulo.
loadMainModule :: FilePath -> ModName -> IO (Either ModuleError Environment)
loadMainModule path modN = liftIO (parseFromFileModule modN) >>= either (return . Left) load 
    where loadAndCheck :: Module -> CheckModule (Maybe ModuleError)
          loadAndCheck = loadEnv path >=> maybe checkEnvModules (return . Just)
          load m = runStateT (loadAndCheck m) initCM >>= \(mErr,st) ->
                   return $ maybe (Right $ (^. modulesEnv) st) Left mErr
              where initCM = initStateCM (insModuleImports m emptyImMG) [m]

loadMainModuleFromFile :: TextFilePath -> IO (Either ModuleError (Environment,ModName))
loadMainModuleFromFile fp = parseFromFileModule fp >>= either (return . Left) load
    where
        load m = runStateT (loadAndCheck m) initCM >>= \(mErr,st) ->
                 return $ maybe (Right (st ^. modulesEnv,m ^. modName)) Left mErr
            where initCM = initStateCM (insModuleImports m emptyImMG) [m]
        loadAndCheck :: Module -> CheckModule (Maybe ModuleError)
        loadAndCheck = loadEnv folder >=> maybe checkEnvModules (return . Just)
            where folder = takeDirectory (unpack fp) ++ [pathSeparator]
                    


-- | Carga los módulos en al environment, esto implica parsear el módulo inicial
-- y cargarlo, así como los imports en cadena.
loadEnv :: FilePath -> Module -> CheckModule (Maybe ModuleError)
loadEnv path m = foldM loadImp Nothing $ m ^. imports
    where 
        loadImp :: Maybe ModuleError -> Import -> CheckModule (Maybe ModuleError)
        loadImp (Just err) _ = return $ Just err
        loadImp Nothing (Import mn) = liftIO (parseFromFileModule mnf) >>= 
                                      either (return . Just) loadEnv'
            where mnf = pack $ path ++ unpack mn ++ ".fun"

        loadEnv' :: Module -> CheckModule (Maybe ModuleError)
        loadEnv' m' = updateEnv m' >> checkCycle >>= maybe (loadEnv path m') (return . Just)
        checkCycle :: CheckModule (Maybe ModuleError)
        checkCycle = do graph <- use imMGraph
                        let cycleList = filter ((>1) . length) . scc $ graph
                        if null cycleList 
                        then return Nothing
                        else return . Just $ ModuleCycleImport $ map (pack.show) $ head cycleList
        updateEnv :: Module -> CheckModule ()
        updateEnv m' = imMGraph   %= insModuleImports m >> 
                       modulesEnv %= addModuleEnv m'


-- | Parsea una módulo en base a una dirección de archivo.
parseFromFileModule :: TextFilePath -> IO (Either ModuleError Module)
parseFromFileModule= readModule . unpack >=> return . either Left load
    where
        load :: String -> Either ModuleError Module
        load = either (Left . ModuleParseError (pack "")) Right . parseFromStringModule 

readModule :: FilePath -> IO (Either ModuleError String)
readModule fp = C.try (readFile fp) >>= either discardError (return . Right)
    where discardError :: C.IOException -> IO (Either ModuleError String)
          discardError _ = return $ Left $ ModuleErrorFileDoesntExist $ pack fp

getModule :: Environment -> ModName -> Maybe Module
getModule env mname = find ((== mname) . (^. modName)) env


-- | Queries for environments
getFuncs :: Environment -> [FunDecl]
getFuncs = concatMap (bare functions . (^. validDecls))

getSpecs :: Environment -> [SpecDecl]
getSpecs = concatMap (bare specs . (^. validDecls))

getVals :: Environment -> [ValDecl]
getVals = concatMap (bare vals . (^. validDecls))

getTypesAss :: Environment -> [(VarName,Type)]
getTypesAss = mconcat [ map (ass . (^. funDeclName)) . getFuncs
                      , map (ass . (^. specName))    . getSpecs
                      , map (ass . (^. valVar))      . getVals ]
    where ass = (varName &&& varTy)
             
