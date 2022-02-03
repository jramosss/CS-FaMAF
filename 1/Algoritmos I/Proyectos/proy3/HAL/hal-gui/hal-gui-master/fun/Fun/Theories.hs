
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
-- Conjunto de teorías conocidas por Fun.
-- 
----------------------------------------------------------------------------

module Fun.Theories (
      module Fun.FunTheories.Arith
    , module Fun.FunTheories.List
    , module Fun.FunTheories.FOL
    , funTheory
    )
    where

import Fun.Theory
import Fun.FunTheories.Arith
import Fun.FunTheories.List
import Fun.FunTheories.FOL

funTheory :: [Theory]
funTheory = [arithTheory,folTheory,listTheory]
