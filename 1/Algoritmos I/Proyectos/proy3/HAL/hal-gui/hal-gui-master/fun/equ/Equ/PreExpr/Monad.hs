{-# Language TypeSynonymInstances, FlexibleInstances #-}
{-# Language MultiParamTypeClasses, FlexibleContexts #-}
module Equ.PreExpr.Monad 
    ( MonadTraversal
    , Log
    , localGo
    )
    where

import Equ.PreExpr.Zipper

import qualified Data.Text as T
import qualified Data.Sequence as S

import Data.Maybe (fromJust)
import Control.Monad.Trans.Either(EitherT (..))
import Control.Monad.RWS (RWS)
import Control.Monad.RWS.Class ( local
                               , MonadReader
                               )

-- | Navegamos por el focus cambiando el enviroment.
localGo :: MonadReader Focus m => (Focus -> Maybe Focus) -> m a -> m a
localGo go = local (fromJust . go)

-- | Tipo correspondiente a nuestros logs.
type Log = S.Seq T.Text

{- | M&#243;nada de estado para preExpresiones (usando focus)
    La id&#233;a en esta monada es poder llevar, un log, un focus y un estado.
    El log lo tenemos para tener un poco de informaci&#243;n verbosa sobre que
    esta ocurriendo durante la computaci&#243;n.
    El focus lo utilizamos para tener contexto a cerca de donde estamos
    parados en relaci&#243;n a la expresi&#243;n, de la cual pretendemos comprobar alguna
    propiedad.
    El estado esta bastante libre, de momento la id&#233;a es utilizarlo para llevar
    las substituci&#243;nes resultantes de correr los algoritmos de matching y 
    type checking.
-}
type MonadTraversal e a = EitherT e (RWS Focus Log a)    
