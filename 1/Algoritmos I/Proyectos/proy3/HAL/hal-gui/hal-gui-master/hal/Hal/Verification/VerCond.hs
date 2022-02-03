{-# LANGUAGE RecordWildCards #-}
module Hal.Verification.VerCond where


-- | Imports de Hal
import Hal.Lang
import Hal.Verification.THoare
import Hal.Parser

-- | Imports de Equ
import qualified Equ.Expr as Equ
import qualified Equ.Theories.FOL as EquFol


data PartTHoare = PartTHoare { partPre :: FormFun
                             , partComm :: Maybe GuardComm
                             }

type THoares = [THoare]
type PartTHoares = [PartTHoare]

-- | Es una lista de comandos sin secuencia. Las secuencias pueden existir dentro
--   de comandos if y while.
type LComm = [Comm]



-- | Convierte un comando en una lista de comandos. Ninguno de los elementos de la lista
--   es una secuencia.
commToList :: Comm -> LComm
commToList = commToList' []
    where commToList' :: LComm -> Comm -> LComm
          commToList' ls (Seq c1 c2) = (commToList' (commToList' ls c1) c2)
          commToList' ls c = ls++[c]

          
-- | Dada una fórmula y una terna de Hoare incompleta, se completa la terna de Hoare.
--   Si la parte incompleta tenía solo precondición, entonces se agrega el comando Skip
--   y luego la fórmula dada como postcondición. Si además tenía un comando, entonces
--   se agrega la postcondición.
completeTHoare :: FormFun -> PartTHoare -> THoare
completeTHoare postc pth = 
            THoare { pre = partPre pth
               , comm = maybe [NGuard Skip] id (partComm pth)
               , post = postc
            }


-- | Dado un comando y una terna de hoare incompleta, se le agrega ese comando. Si ya había un comando
--   Entonces se lo agrega en una secuencia.
addToPartTHoare :: Comm -> PartTHoare -> PartTHoare
addToPartTHoare c pth@PartTHoare{ partComm = pc } = pth { partComm = Just $ maybe [NGuard c] (flip (++) [NGuard c]) pc }

-- | A partir de una precondición, se crea una terna de Hoare incompleta.
newPTHoare :: FormFun -> PartTHoare
newPTHoare f = PartTHoare { partPre = f
                          , partComm = Nothing
                          }

-- | Dada una expresión booleana b del lenguaje y una terna de hoare incompleta, si pre es
--   la precondición de la terna, entonces se modifica la precondición quedando: pre and b
addGuard :: BExp -> PartTHoare -> PartTHoare
addGuard b pth@PartTHoare{ partComm = pc } = pth { partComm = Just $ maybe [Guard b] (flip (++) [Guard b]) pc }

{- | Función auxiliar para obtener las ternas de hoare de un programa.
     cths son las ternas de Hoare acumuladas hasta un momento dado.
     pths son las ternas de Hoare incompletas acumuladas.
     El comando no puede ser una secuencia. Al encontrar un assert, se completan
     las ternas de Hoare incompletas pasándolas al conjunto cths. 
     Si el comando es un If o un While, se convierte/n el/los subcomando/s en LComm
     y se llama a vc2, la cual obtendrá las ternas de Hoare de los subcomandos.
     -}
vc :: THoares -> PartTHoares -> Comm -> (THoares,PartTHoares)
vc cths pths (Assert f) = 
    (cths ++ (map (completeTHoare f) pths),[newPTHoare f])
vc cths pths (If b c1 c2) = (cths++cthsB++cthsNB,pthsB++pthsNB)
    where (cthsB,pthsB) = vc2 [] (map (addGuard b) pths) (commToList c1) Nothing
          (cthsNB,pthsNB) = vc2 [] (map (addGuard (BUOp Not b)) pths) (commToList c2) Nothing
-- | Las ternas de Hoare en el Do son 3:
--   { Pre Skip Inv }
--   { (Inv && not b) Skip Pos }
--   { (Inv && b) c Inv }
vc cths pths (Do inv b c) = 
    (cths ++ t1 ++ cths',pths'++[pth])
    
    where t1 = map (completeTHoare inv) pths -- { Pre Skip Inv }
          (cths',pths') = vc2 [] 
                             [PartTHoare (EquFol.and inv (bExpToFun b)) Nothing] -- { (Inv && b) c Inv }
                             (commToList c)
                             (Just inv)
          pth = PartTHoare (EquFol.and inv (bExpToFun $ BUOp Not b)) Nothing -- { (Inv && not b) Skip Pos }
          
-- Skip, Abort, Assign
vc cths pths c = (cths,map (addToPartTHoare c) pths)

{- | Función auxiliar para obtener ternas de hoare de un programa.
     A diferencia de vc, esta función toma una lista de comandos sin secuencia en el primer
     nivel, y posiblemente una postcondición. Para cada comando de la lista se llama a vc
     y al finalizar la lista, si hay una postcondición, se la agrega.
-}
vc2 :: THoares -> PartTHoares -> LComm -> Maybe FormFun -> (THoares,PartTHoares)
vc2 cths pths [] mpostc = 
    maybe (cths,pths) 
          (\postc -> (cths++(map (completeTHoare postc) pths),[]))
          mpostc
vc2 cths pths (c:lcomms) mpostc = vc2 cths' pths' lcomms mpostc
    where (cths',pths') = vc cths pths c
          

-- | Dado un programa genera ternas de Hoare que representan las condiciones de verificación. 
verConditions :: Program -> THoares
verConditions (Prog _ pre c postc) = cths
    where (cths,pths) = vc2 [] 
                            [PartTHoare { partPre = pre
                                           , partComm = Nothing} ]
                            (commToList c)
                            (Just postc)





                            
                            
                            