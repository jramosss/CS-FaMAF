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
-- Exportamos los parsers de módulos y expresiones?
-- 
----------------------------------------------------------------------------
module Fun.Parser 
            ( module Fun.Parser.Module
            , module Fun.Parser.Internal
            , module Text.Parsec
            )
    where

import Fun.Parser.Module
import Fun.Parser.Internal
import Text.Parsec
