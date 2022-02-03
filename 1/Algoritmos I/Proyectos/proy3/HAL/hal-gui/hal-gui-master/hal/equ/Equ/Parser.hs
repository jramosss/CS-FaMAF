-- | Este modulo es el parser a Pre-Expresiones. 
{--
Sobre Parsec

-- Informe de errores; Quisieramos poder marcar la posicion del error
e informacion bonita de cual fue el error. Parece ser que con
ParseError nos alcanza.

-- Funcion que determina el conName.

-- Hace falta hacer algun tipo de analisis para los tipos. Parseando
una funcion no hay problema debido a los constructores definidos en
las teorias pero que pasa con las constantes? por ejemplo, parseExpr
3; deberia quedar parseado con su tipo? que pasa si el usuario lo
especifico o no.  Resolucion: Parseamos con TyUnknown

-- Syntaxis de una escritura general; Como es una prueba bien escrita.
Se permiten comentarios?

-- Operadores de lista; Precedencia, de momento todos tienen la misma.

-- Libreria; criterion para testear rendimiento.
--}

module Equ.Parser 
    (-- * Caracteres especiales comunes a todas las teorías
      module Equ.Parser.Expr
    , module Equ.Parser.Types
    , module Equ.Parser.Proof
    )
    where

import Equ.Parser.Expr
import Equ.Parser.Types
import Equ.Parser.Proof