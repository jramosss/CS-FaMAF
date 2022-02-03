{-| Algoritmo de matching entre expresiones. La función principal de
este módulo toma dos expresiones @ptn@ y @expr@ e intenta generar la
substitución de las variables que aparecen en @ptn@ de manera que al
aplicarla sobre @ptn@ se obtenga expr.  Este es un algoritmo bien
conocido. La unica variacion es que contamos con expresiones
cuantificadas; en esas expresiones, las variables cuantificadas son
tratadas como parámetros.  -}

module Equ.Matching
    ( module Equ.Matching.Error
    , match
    , matchWithSubst
    , MatchMErr
    , VariableRename
    )
    where

import Equ.Matching.Error
import Equ.PreExpr

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.RWS (runRWS)
import Control.Monad.Trans.Either (runEitherT, hoistEither)
import Control.Monad.RWS.Class(ask)

import Control.Arrow (first)

-- | Estructura general para los errores informativos con contexto.
type MatchMErr = (Focus,MatchError)


type VariableRename = M.Map Variable Variable

-- | M&#243;nada de estado para matching.
type MatchState = MonadTraversal MatchMErr (ExprSubst,VariableRename)

-- | Generaci&#243;n de mensaje de Error.
matcherr :: MatchError -> MatchState a
matcherr err = ask >>= \foc -> hoistEither $ Left (foc, err)



{- WhenM y whenML.
    Para un valor de verdad y un error particular; 
        True => seguir la computaci&#243;n.
        False => devolver el error particular.
-}
whenM :: Bool -> MatchError -> MatchState (ExprSubst,VariableRename) -> MatchState (ExprSubst,VariableRename)
whenM True _ = id
whenM False er = const $ matcherr er

whenML :: Bool -> MatchError -> (ExprSubst,VariableRename) -> MatchState (ExprSubst,VariableRename)
whenML True _ = return
whenML False er = const $ matcherr er

{- Funci&#243;n que implementa el algoritmo de matching. Toma una lista de variables
que est&#225;n ligadas a alg&#250;n cuantificador, una expresi&#243;n patr&#243;n, otra expresi&#243;n y
un mapa de sustituciones. 
-}
match' :: [Variable] -> PreExpr -> PreExpr -> (ExprSubst,VariableRename) -> MatchState (ExprSubst,VariableRename)
{- El caso principal del algoritmo, donde el patr&#243;n es una variable. 
* Si la expresi&#243;n e' es igual al patr&#243;n Var v, se devuelve el mismo mapa de 
sustituciones (es decir, no hay que reemplazar nada para llegar desde una 
expresi&#243;n a la otra).
* Si las expresiones son distintas y v pertenece bvs, entonces no hay matching.
(para que dos expresiones cuantificadas matcheen, se considera a sus variables 
ligadas como la misma variable, y es la que se agrega a la lista bvs, por eso 
no hay matching entre una de esas variables y cualquier otra cosa distinta).
* Si las expresiones son distintas y v no pertenece a bvs, nos fijamos si en el 
mapa de sustituciones se encuentra la variable. Si no, entonces podemos matchear
v por e'. Si v est&#225; en el mapa, entonces para que haya matching tiene que estar 
asociada con la expresi&#243;n e'.
-}
match' bvs e@(Var v) e' (s,rnm) 
                          | e == e' = return $ (M.insert v e' s,rnm)
                          | v `elem` bvs = matcherr $ BindingVar v
                          | otherwise = 
                              maybe (return $ (M.insert v e' s,rnm))
                                    (\f -> whenML (e' == f) (DoubleMatch v f e') (s,rnm))
                                    $ M.lookup v s

match' bvs (UnOp op1 e1) (UnOp op2 e2) s = whenM (op1==op2) 
                                            (InequOperator op1 op2) $ 
                                            localGo goDown (match' bvs e1 e2 s)


{-
    VERSI&#211;N 2; Para operadores iguales, cada vez que pretendo intentar matchear
    las expresiones internas, cambio el enviroment segun corresponda.
-}
match' bvs (BinOp op1 e1 e2) (BinOp op2 f1 f2) s = 
                    whenM (op1==op2) (InequOperator op1 op2) $
                                    localGo goDown (match' bvs e1 f1 s) >>= 
                                    (localGo goDownR . match' bvs e2 f2)

match' bvs (App e1 e2) (App f1 f2) s = localGo goDown (match' bvs e1 f1 s) >>= 
                                       (localGo goDownR . match' bvs e2 f2)

{-
    VERSI&#211;N 2; Un detalle no menor ac&#225; es que como navegamos solamente
    por el focus de la expresi&#243;n a matchear, es decir no la expresi&#243;n patron,
    en el primer caso de los parentesis no cambiamos el enviroment.
-}
match' bvs (Paren e1) e2 s = match' bvs e1 e2 s
match' bvs e1 (Paren e2) s = localGo goDown $ match' bvs e1 e2 s

{-
Para matchear dos expresiones cuantificadas, deben ser el mismo cuantificador.
Si las variables cuantificadas v y w son la misma, entonces hacemos matching en 
las subexpresiones, agregando v a la lista de variables ligadas bvs.
Si v/=w, entonces reemplazamos v y w por una variable fresca en ambas expresiones
y luego realizamos matching en las subexpresiones, agregando la variable fresca
a bvs.

VERSI&#211;N 2; Cada vez que voy a intentar matchear las expresiones internas del
    cuantificador, cambio el enviroment, es decir, navego el focus con la
    direcci&#243;n que corresponda. Para esto uso dos funciones localGoL y localGoR
    que representan navegar por izquierda o por derecha respectivamente.
-}    

match' bvs (Quant q v e1 e2) (Quant p w f1 f2) (s,rnm) =
    whenM (q == p) (InequQuantifier q p) $ -- En caso de error devuelvo InequQuant
        if v == w
        then let newRnm = M.insert v w rnm in
                localGo goDown (match' (v:bvs) e1 f1 (s,newRnm)) >>= 
                localGo goDownR . match' (v:bvs) e2 f2
        else let newRnm = M.insert v fv rnm in
             localGo goDown (match' (fv:bvs) (rename v fv e1) (rename w fv f1) (s,newRnm)) >>=
             localGo goDownR . match' (fv:bvs) (rename v fv e2) (rename w fv f2)
    where fv= freshVar $ S.unions [ S.singleton v, freeVars e1, freeVars e2
                                  , S.singleton w, freeVars f1,freeVars f2
                                  ]

                                  
match' bvs (If b e1 e2) (If b' e1' e2') (s,rnm) =
    localGo goDown (match' bvs b b' (s,rnm)) >>=
    (localGo goDownR . match' bvs e1 e1') >>=
    (localGo (\f -> goDownR f >>= goRight) . match' bvs e2 e2')
                                  
                                  
-- Caso particular de intentar matchear una variable con una funci&#243;n.
{-match' _ (Fun _) (Var _) s = Left FuncWithVar
-- Caso particular de intentar matchear una variable con una constante.
match' _ (Con _) (Var _) s = Left ConstWithVar
-- El nombre de las funciones debe ser el mismo.
match' _ (Fun f1) (Fun f2) s = whenML (f1==f2) InequNameFunc s
-- Para matchear constantes deben ser exactamente la misma.
match' _ (Con c1) (Con c2) s = whenML (c1==c2) InequNameConst s
-- Si no estamos en ningun caso anterior, entonces solo hay matching
-- si las expresiones son iguales.
-- En caso de error devuelvo InequPreExpr 

    VERSI&#211;N 2; Ninguna diferencia con el original.
   -}
match' _ e1 e2 s = whenML (e1==e2) (InequPreExpr e1 e2) s

{-  VERSI&#211;N 2; Funci&#243;n principal de matching.
    
    Primer intento de agregar informaci&#243;n al intentar realizar matching.
    Hasta el momento se podr&#237;a decir que tenemos dos grandes novedades con
    respecto a la funci&#243;n vieja. Disponemos de un log y hacemos uso de focus
    para rastrear donde estamos en la expresi&#243;n que intentamos matchear.
    Sobre el log todav&#237;a no hago ninguna utilizaci&#243;n, estar&#237;a bueno usarlo
    para llevar la cuenta de que matching he podido realizar?.
    Sobre el focus, la idea es recorrer la expresi&#243;n que estamos intentando
    matchear, esta aclaraci&#243;n es importante ya que est&#225; la otra opci&#243;n de 
    recorrer la expresi&#243;n patr&#243;n.
    
    Cosas interesantes; me base fuertemente en el modulo TypeChecker. Tan as&#237;
    que la funci&#243;n principal basicamente la copie y pegue de la funci&#243;n que 
    hizo miguel, creditos a &#233;l :). 
    Yo no termino de entender bien como es que operta.
    
    Algunas aclaraciones a parte; no quise borrar lo hecho por las dudas y de 
    ah&#237; que tenemos esta versi&#243;n 2.

-}

{-| @match@ toma una expresi&#243;n patr&#243;n y otra que quiere matchearse con el patr&#243;n.
Si hay matching, retorna el mapa de sustituciones que deben realizarse
simult&#225;neamente para llegar desde la expresi&#243;n patr&#243;n a la expresi&#243;n dada.
-}
match :: PreExpr -> PreExpr -> Either (MatchMErr,Log) (ExprSubst,VariableRename)
match e e' = matchWithSubst e e' (M.empty,M.empty)

matchWithSubst :: PreExpr -> PreExpr -> (ExprSubst,VariableRename) -> Either (MatchMErr,Log) (ExprSubst,VariableRename)
matchWithSubst e e' subs = case runRWS (runEitherT (match' [] e e' subs)) (toFocus e') subs of
                   (res, _, l) -> either (\err -> Left (err,l)) (Right . first prune) res
    where prune s = M.filterWithKey (\v -> (Var v /=)) s
