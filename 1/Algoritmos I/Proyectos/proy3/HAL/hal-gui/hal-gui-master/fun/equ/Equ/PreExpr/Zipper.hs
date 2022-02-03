-- Los zippers pueden ser convenientes; la referencia es: ``The
-- Zipper'' de G&#233;rard Huet en JFP. 

module Equ.PreExpr.Zipper 
    ( Focus
    , Path
    , toExpr, toFocus, toFocuses, toFocusesWithGo, focusToFocuses
    , replace
    , goDown, goUp, goLeft, goRight, goDownR, goDownL, goTop
    , goIfTrue, goIfFalse
    ) where

import Equ.PreExpr.Internal
import Equ.Syntax

import Data.Serialize(Serialize, get, getWord8, put, putWord8)
import Control.Applicative ((<$>), (<*>),Applicative(..))
import Test.QuickCheck(Arbitrary, arbitrary, oneof)
import Data.Maybe(fromJust)
import Control.Monad((>=>))

-- | Definici&#243;n de los posibles lugares en los que podemos estar
-- enfoc&#225;ndonos.
data Path = Top
          | UnOpD Operator Path
          | BinOpL Operator Path PreExpr
          | BinOpR Operator PreExpr Path
          | AppL Path PreExpr
          | AppR PreExpr Path
          | QuantL Quantifier Variable Path PreExpr
          | QuantR Quantifier Variable PreExpr Path
          | ParenD Path
          | IfCond Path PreExpr PreExpr
          | IfTrue PreExpr Path PreExpr
          | IfFalse PreExpr PreExpr Path
          | CaseD Path [(PreExpr,PreExpr)]
          | CasePatternL PreExpr Path PreExpr [(PreExpr,PreExpr)] [(PreExpr,PreExpr)]
          | CasePatternR PreExpr Path PreExpr [(PreExpr,PreExpr)] [(PreExpr,PreExpr)]
            deriving (Eq,Show)
            
            
            
-- | Dado un Path y un Focus, le aplica el camino que indica ese path al focus.
-- applyPathToFocus :: Path -> Focus -> Maybe Focus
-- applyPathToFocus Top f = Just f
-- applyPathToFocus (UnOpD _ p) f = goDown f >>= applyPathToFocus p
-- applyPathToFocus (BinOpL _ p _) f = goDownL f >>= applyPathToFocus p
-- applyPathToFocus (BinOpR _ _ p) f = goDownR f >>= applyPathToFocus p
-- applyPathToFocus (AppL p _) f = 
--             
{- El path en una expresion "case" lo definimos: Si estamos enfocados en la expresion sobre la que se hace pattern
   matching, entonces usamos CaseD.
   Si queremos enfocarnos en un pattern de la lista de patterns, entonces necesitamos guardar la expresión sobre la
   que hacemos pattern matching, el path, la expresion que se aplica en caso de que el pattern es correcto (el segundo
   elemento de la tupla) y dos listas que son la parte izquierda y derecha de la lista original de patterns, tal que
   el pattern que estoy viendo quede entre ambas.
   Ejemplo:
   case e0 of
        p1 -> e1
        p2 -> e2
   
   Enfocarse en e0 tendria el path:  CaseD Top [(p1,e1),(p2,e2)]
   Enfocarse en p1 tendria el path:  CasePatternL e0 Top e1 [] [(p2,e2)]
   Enfocarse en e1 tendria el path:  CasePatternR e0 Top p1 [] [(p2,e2)]
   Enfocarse en p2 tendria el path:  CasePatternL e0 Top e2 [(p1,e1)] []
   Enfocarse en e2 tendria el path:  CasePatternR e0 Top p2 [(p1,e1)] []
-}
        
   
instance Serialize Path where
    put Top = putWord8 0
    put (UnOpD op p) = putWord8 1 >> put op >> put p
    put (BinOpL op p pe) = putWord8 2 >> put op >> put p >> put pe
    put (BinOpR op pe p) = putWord8 3 >> put op >> put pe >> put p
    put (AppL p pe) = putWord8 4 >> put p >> put pe
    put (AppR pe p) = putWord8 5 >> put pe >> put p
    put (QuantL q v p pe) = putWord8 6 >> put q >> put v >> put p >> put pe
    put (QuantR q v pe p) = putWord8 7 >> put q >> put v >> put pe >> put p
    put (ParenD p) = putWord8 8 >> put p
    put (IfCond p e e') = putWord8 9 >> put p >> put e >> put e'
    put (IfTrue b p e') = putWord8 10 >> put b >> put p >> put e'
    put (IfFalse b e p) = putWord8 11 >> put b >> put e >> put p
    put (CaseD p es) = putWord8 12 >> put p >> put es
    put (CasePatternL c p e es es') = putWord8 13 >> put c >> put p >> put e >> put es >> put es'
    put (CasePatternR c p e es es') = putWord8 14 >> put c >> put p >> put e >> put es >> put es'


    get = do
    tag_ <- getWord8
    case tag_ of
        0 -> return Top
        1 -> UnOpD <$> get <*> get
        2 -> BinOpL <$> get <*> get <*> get 
        3 -> BinOpR <$> get <*> get <*> get
        4 -> AppL <$> get <*> get
        5 -> AppR <$> get <*> get
        6 -> QuantL <$> get <*> get <*> get <*> get
        7 -> QuantR <$> get <*> get <*> get <*> get
        8 -> ParenD <$> get
        9 -> IfCond <$> get <*> get <*> get
        10 -> IfTrue <$> get <*> get <*> get
        11 -> IfFalse <$> get <*> get <*> get
        12 -> CaseD <$> get <*> get
        13 -> CasePatternL <$> get <*> get <*> get <*> get <*> get
        14 -> CasePatternR <$> get <*> get <*> get <*> get <*> get
        _ -> fail $ "SerializeErr Path " ++ show tag_
            

instance Arbitrary Path where
    arbitrary =
        oneof [ return Top
              , UnOpD <$> arbitrary <*> arbitrary
              , BinOpL <$> arbitrary <*> arbitrary <*> arbitrary
              , BinOpR <$> arbitrary <*> arbitrary <*> arbitrary
              , AppL <$> arbitrary <*> arbitrary
              , AppR <$> arbitrary <*> arbitrary
              , QuantL <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
              , QuantR <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
              , ParenD <$> arbitrary
              ]


-- | Un Focus representa la expresi&#243;n que consiste de completar el
-- hueco denotado por Path con la expresi&#243;n PreExpr (eso es lo que
-- hace toExpr).
type Focus = (PreExpr,Path)

toExpr :: Focus -> PreExpr
toExpr (pe, Top) = pe
toExpr (pe, UnOpD op path) = toExpr (UnOp op pe, path)
toExpr (pe, BinOpL op path pe0) = toExpr (BinOp op pe pe0, path)
toExpr (pe, BinOpR op pe0 path) = toExpr (BinOp op pe0 pe, path)
toExpr (pe, AppL path pe0) = toExpr (App pe pe0, path)
toExpr (pe, AppR pe0 path) = toExpr (App pe0 pe, path)
toExpr (pe, QuantL qua v path pe0) = 
    toExpr (Quant qua v pe pe0, path)
toExpr (pe, QuantR qua v pe0 path) = 
    toExpr (Quant qua v pe0 pe, path)
toExpr (pe, ParenD path) = toExpr (Paren pe, path)
toExpr (pe,IfCond path e1 e2) = toExpr (If pe e1 e2,path)
toExpr (pe,IfTrue cond path e2) = toExpr (If cond pe e2,path)
toExpr (pe,IfFalse cond e1 path) = toExpr (If cond e1 pe,path)
toExpr (pe,CaseD path patterns) = toExpr (Case pe patterns,path)
toExpr (pe,CasePatternL e path e1 left right) = toExpr (Case e (left++[(pe,e1)]++right),path)
toExpr (pe,CasePatternR e path p1 left right) = toExpr (Case e (left++[(p1,pe)]++right),path)


-- | Dado una expresi&#243;n la enfocamos. Es decir luego de llamar a toFocus con 
-- preExp queda el focus que tiene a la expresi&#243;n y estamos en el Top.
toFocus :: PreExpr -> Focus
toFocus e = (e,Top)

-- Funcion auxiliar para calcular la lista de todos los focus de una expresion
-- dado un focus inicial. En nuestro caso particular la llamamos con (preExp, Top)
-- donde preExp es la expresion de la que queremos la lista de focus posibles.
-- Nota: En cada llamada recursiva, el elemento que agregamos en a la lista
-- es el respectivo elemento que devuelve (go* focus), * in {Down, DownR, DownL}.
focusToFocuses :: Maybe Focus -> [Focus]
focusToFocuses Nothing = []
focusToFocuses (Just f) = 
            case (goDownL f, goDownR f) of
                (glf@(Just lf), grf@(Just rf)) -> (lf : focusToFocuses glf) ++
                                                    (rf : focusToFocuses grf)
                (glf@(Just lf), Nothing) -> lf : focusToFocuses glf
                (Nothing, _) -> []

-- | Dado una preExpresion obtenemos todas las subexpresiones navegando con el
-- zipper.
-- Propiedades (forall e):
--   forall t \in toFocuses e, toExpr t = e
toFocuses :: PreExpr -> [Focus]
toFocuses pe = (pe, Top) : focusToFocuses (Just (pe, Top))


focusToFocusesWithGo :: Maybe (Focus, Focus -> Focus) -> [(Focus, Focus -> Focus)]
focusToFocusesWithGo Nothing = []
focusToFocusesWithGo (Just (f, go)) = 
            case (goDownL f, goDownR f) of
                (Just lf, Just rf) -> 
                    ((lf, fromJust . goDownL . go) : 
                    focusToFocusesWithGo (Just (lf, fromJust . goDownL . go))) 
                        ++
                    ((rf, fromJust . goDownR . go) : 
                    focusToFocusesWithGo (Just (rf, fromJust . goDownR . go)))
                (Just lf, Nothing) -> 
                    ((lf, fromJust . goDownL . go) : 
                    focusToFocusesWithGo (Just (lf, fromJust . goDownL . go)))
                (Nothing, _) -> []

-- | Dado una preExpresion obtenemos todas las subexpresiones navegando con el
-- zipper y ademas obtenemos la función que al aplicarla a esta preExpresion
-- nos retorna el focus relacionado.
toFocusesWithGo :: PreExpr -> [(Focus, Focus -> Focus)]
toFocusesWithGo pe = ((pe, Top), id) : focusToFocusesWithGo (Just ((pe, Top), id))

-- | Reemplaza la expresi&#243;n enfocada por una nueva expresi&#243;n.
replace :: Focus -> PreExpr -> Focus
replace (_,p) e' = (e',p)

-- Bajar un nivel en el focus, yendo por izquierda.
goDownL :: Focus -> Maybe Focus
goDownL = goDown

-- Bajar un nivel en el focus, yendo por derecha.
goDownR :: Focus -> Maybe Focus
goDownR f = goDown f >>= goRight

-- Navegaci&#243;n dentro de un Zipper.
-- | Bajar un nivel en el focus.
goDown :: Focus -> Maybe Focus
goDown (Var _, _) = Nothing
goDown (Con _, _) = Nothing
goDown (PrExHole _, _) = Nothing
goDown (UnOp op pe, path) = Just (pe, UnOpD op path)
goDown (BinOp op pe0 pe1, path) = Just (pe0, BinOpL op path pe1)
goDown (App pe0 pe1, path) = Just (pe0, AppL path pe1)
goDown (Quant qua v pe0 pe1, path) = Just (pe0, QuantL qua v path pe1)
goDown (Paren pe, path) = Just (pe, ParenD path)
goDown (If c e1 e2,path) = Just (c, IfCond path e1 e2)
goDown (Case e patterns,path) = Just (e,CaseD path patterns)


-- | Subir un nivel en el focus.
goUp :: Focus -> Maybe Focus
goUp (_, Top) = Nothing
goUp (pe, UnOpD op path) = Just (UnOp op pe, path)
goUp (pe, BinOpL op path pe0) = Just (BinOp op pe pe0, path)
goUp (pe, BinOpR op pe0 path) = Just (BinOp op pe0 pe, path)
goUp (pe, AppL path pe0) = Just (App pe pe0, path)
goUp (pe, AppR pe0 path) = Just (App pe0 pe, path)
goUp (pe, QuantL qua v path pe0) = Just (Quant qua v pe pe0, path)
goUp (pe, QuantR qua v pe0 path) = Just (Quant qua v pe0 pe, path)
goUp (pe, ParenD path) = Just (Paren pe, path)
goUp (pe, IfCond path e1 e2) = Just (If pe e1 e2,path)
goUp (pe, IfTrue cond path e2) = Just (If cond pe e2,path)
goUp (pe, IfFalse cond e1 path) = Just (If cond e1 pe,path)
goUp (pe, CaseD path patterns) = Just (Case pe patterns,path)
goUp (pe, CasePatternL e path e1 left right) = Just (Case e (left++[(pe,e1)]++right),path)
goUp (pe, CasePatternR e path p1 left right) = Just (Case e (left++[(p1,pe)]++right),path)

-- | Ir a la izquierda en un focus, sin cambiar de nivel.
goLeft :: Focus -> Maybe Focus
goLeft (_, Top) = Nothing
goLeft (_, UnOpD _ _) = Nothing
goLeft (_, BinOpL _ _ _) = Nothing
goLeft (pe, BinOpR op pe0 path) = Just (pe0, BinOpL op path pe)
goLeft (_, AppL _ _) = Nothing
goLeft (pe, AppR pe0 path) = Just (pe0, AppL path pe)
goLeft (_, QuantL _ _ _ _) = Nothing
goLeft (pe, QuantR qua v pe0 path) = Just (pe0, QuantL qua v path pe)
goLeft (_, ParenD _) = Nothing
goLeft (_,IfCond _ _ _) = Nothing
goLeft (pe,IfTrue cond path e2) = Just (cond, IfCond path pe e2)
goLeft (pe,IfFalse cond e1 path) = Just (e1, IfTrue cond path pe)
goLeft (_,CaseD _ _) = Nothing
goLeft (pattern,CasePatternL e path e1 [] right) = Just (e,CaseD path ((pattern,e1):right))
goLeft (pattern,CasePatternL e path e1 left right) = 
    Just (prevPattern,CasePatternL e path prevExpr (init left) ((pattern,e1):right))
    where prevPattern = fst . last $ left
          prevExpr = snd . last $ left
goLeft (pe,CasePatternR e path pattern left right) =
    Just (pattern,CasePatternL e path pe left right)

-- | Ir a la derecha en un focus, sin cambiar de nivel.
goRight :: Focus -> Maybe Focus
goRight (_, Top) = Nothing
goRight (_, UnOpD _ _) = Nothing
goRight (pe, BinOpL op path pe0) = Just (pe0, BinOpR op pe path)
goRight (_, BinOpR _ _ _) = Nothing
goRight (pe, AppL path pe0) = Just (pe0, AppR pe path)
goRight (_, AppR _ _) = Nothing
goRight (pe, QuantL qua v path pe0) = Just (pe0, QuantR qua v pe path)
goRight (_, QuantR _ _ _ _) = Nothing
goRight (_, ParenD _) = Nothing
goRight (pe, IfCond path e1 e2) = Just (e1, IfTrue pe path e2)
goRight (pe, IfTrue c path e2) = Just (e2, IfFalse c pe path)
goRight (_, IfFalse _ _ _) = Nothing
goRight (pe, CaseD path patterns) = 
    case patterns of 
        [] -> Nothing
        ((p1,e1):ps) -> Just (p1, CasePatternL pe path e1 [] ps)
goRight (pattern, CasePatternL e path e1 left right) =
    Just (e1, CasePatternR e path pattern left right)
goRight (pe, CasePatternR e path pattern left right) =
    case right of
         [] -> Nothing
         ((p2,e2):_) -> Just (p2, CasePatternL e path e2 (left++[(pattern,pe)]) right)


-- | Sube hasta el tope.
goTop :: Focus -> Focus
goTop (e,Top) = (e,Top)
goTop f = goTop $ fromJust $ goUp f




goIfTrue,goIfFalse :: Focus -> Maybe Focus
goIfTrue = goDown >=> goRight
goIfFalse = goDown >=> goRight >=> goRight
