-- | Este m&#243;dulo define las funciones principales de re-escritura de expresiones.
module Equ.Rewrite
    ( exprRewrite
    , focusedRewrite
    , rewriteAllFocuses
    , typedRewrite
    , RewriteError
    , RM
    , rewriteAllFocusesInformative
    , focusedRewriteInformative
    )
    where

import Equ.Matching
import Equ.Rule
import Equ.Expr
import Equ.PreExpr
import Equ.TypeChecker hiding (match)

-- Constructor del tipo mas general para manejar errores de matching o 
-- unificaci&#243;n de manera indistinta. Como referencia al error usamos el
-- prefijo acerca sobre de que tipo fue el error.
data RewriteError' a b = Matching a | Type b
    deriving (Show, Eq)

-- | Tipo general de errores de re-escritura. Contiene errores de matching
-- y unificaci&#243;n.
type RewriteError = RewriteError' (MatchMErr, Log) TyErr

type RM a = Either RewriteError a

{- | Dada una expresi&#243;n y una regla, si la expresi&#243;n matchea con el lado
izquierdo de la regla, entonces se reescribe de acuerdo al lado derecho
de la regla.
-}
exprRewrite :: Expr -> Rule -> RM Expr
exprRewrite (Expr e) (Rule{lhs=Expr l,rhs=Expr r}) = 
                            case match l e of
                                Left er -> Left $ Matching er
                                Right (subs,_) -> Right $ Expr $ applySubst r subs

-- | Igual a exprRewrite pero ademas retorna la lista de sustituciones.
rewriteInformative :: Expr -> Rule -> RM (Expr, ExprSubst)
rewriteInformative (Expr e) (Rule{lhs=Expr l,rhs=Expr r}) = 
                        case match l e of
                            Left er -> Left $ Matching er
                            Right (subs,_) -> Right (Expr $ applySubst r subs, subs)

-- | Dado un focus y una regla, aplicamos re-escrituda con la regla a la 
--  expresi&#243;n focalizada, en caso de exito reemplazamos la expresi&#243;n inicial
--  por la expresi&#243;n resultante dentro del focus.
focusedRewrite :: Focus -> Rule -> RM (Focus,Focus)
focusedRewrite f@(pe, _) r = exprRewrite (Expr pe) r >>= 
                             \(Expr pe')-> return $ (replace f pe',f)
                             
-- | Igual que la anterior, pero ademas retorna la lista de substituciones.
focusedRewriteInformative :: Focus -> Rule -> RM (Focus,Focus,ExprSubst)
focusedRewriteInformative f@(pe, _) r = rewriteInformative (Expr pe) r >>= 
                                        \(Expr pe',subs)-> return $ (replace f pe',f,subs)
                             
rewriteAllFocuses :: PreExpr -> Rule -> [RM (Focus,Focus)]
rewriteAllFocuses e r = map (flip focusedRewrite r) (toFocuses e)

rewriteAllFocusesInformative :: PreExpr -> Rule -> [RM (Focus,Focus,ExprSubst)]
rewriteAllFocusesInformative e r = map (flip focusedRewriteInformative r) (toFocuses e)

{- 
    Me di cuenta que no termino de entender que deber&#237;a hacer esta funci&#243;n.
    Por ejemplo, si hacemos checkPreExpr (parser "0+0") obetenemos
    Right (TyAtom ATyNat) y hasta ac&#225; todo bien, hacemos lo mismo para el
    lado izq de la regla. Ahora, con esta substituci&#243;n que obtengo deber&#237;a
    cambiar el tipo de 0+0, pero la preExpresion 0+0 no tiene ningun tipo
    asociado fijo que pueda cambiar.
    Sera cambiar el tipo "final", siguiendo con el ejemplo anterior
    tType + = TyAtom ATyNat :-> (TyAtom ATyNat :-> TyAtom ATyNat)
    la idea es cambiar el tipo y que quede;
    tType + = TyAtom ATyNat :-> (TyAtom ATyNat :-> TIPO_NUEVO)
    Me acabo de dar cuenta que solamente se aplicar&#237;a a variables de tipo,
    pero bueno aun as&#237; me parece que el ejemplo puede ayudar a entender sobre
    lo que dudo.
    
    Dejo escrita la funci&#243;n (con casos incompletos) para ejemplificar lo que
    entiendo. No la termino porque muy probablemente este mal :P
    
    Resolci&#243;n:
    La idea es re-escribir expresiones tipadas, primero testeando que los tipos
    de estas se puedan unificar, si es as&#237; entonces procedemos a re-escribir
    de otra forma devolvemos error de unificaci&#243;n. Aprovechamos que unify 
    tiene un bonito log sobre errores para devolver eso en caso de que no
    existe unificaci&#243;n.
-}
{- | Variante de reescritura donde se chequea que los tipos de las
expresiones a reescribir tengan el mismo tipo (salvo unificación).
-}
typedRewrite :: Expr -> Rule -> RM Expr
typedRewrite e@(Expr pe) ru@(Rule{lhs=Expr l,rhs=Expr _}) = 
    let (Right te) = checkPreExpr pe
        (Right tr) = checkPreExpr l
    in case unify te tr emptySubst of
            Left er -> Left $ Type er
            Right _ -> exprRewrite e ru
            
