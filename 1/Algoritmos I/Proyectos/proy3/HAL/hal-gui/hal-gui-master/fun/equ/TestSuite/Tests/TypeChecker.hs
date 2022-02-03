-- | Propiedades de algoritmos relacionados con el type-checker.
module TestSuite.Tests.TypeChecker 
    ( -- * Propiedades a verificar con QuickCheck.
     prop_unification
    , prop_freshVars
    )
    where

import Equ.Types
import Equ.TypeChecker

-- | Si la unificación fue exitosa, entonces los tipos son iguales después
-- de aplicar la sustitución.
prop_unification :: (Type,Type) -> Bool
prop_unification (t,t') = case unify t t' emptySubst of
                            Left _ -> True
                            Right s -> rewrite s t == rewrite s t'

-- | Si 'v `elem` freshVars t', entonces 'not (occurs v t)'.
prop_freshVars :: Type -> Bool
prop_freshVars t = and . take 2 . map (not . flip occurs t) . freshVars $ t
