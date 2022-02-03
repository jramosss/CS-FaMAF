{-# Language OverloadedStrings #-}
module Equ.PreExpr.Symbols where


import Equ.Syntax
import Equ.Types

import Equ.Theories.AbsName

-- | Constante cero.
natZero :: Constant
natZero = Constant { conRepr = "0"
                   , conName = Zero
                   , conTy = TyAtom ATyNat
                   }

-- | Operador sucesor.
natSucc :: Operator
natSucc = Operator { opRepr = "succ"
                   , opName = Succ
                   , opTy = TyAtom ATyNat :-> TyAtom ATyNat
                   , opAssoc = None
                   , opNotationTy = NPrefix
                   , opPrec = 23 -- Analizar.
                   , opGlyphs = []
                   }

-- | Operador suma.
natSum :: Operator
natSum = Operator { opRepr = "+"
                  , opName = Sum
                  , opTy = TyAtom ATyNat :-> TyAtom ATyNat :-> TyAtom ATyNat
                  , opAssoc = ALeft
                  , opNotationTy = NInfix
                  , opPrec = 21
                  , opGlyphs = []
                  }

-- | Operador producto.
natProd :: Operator
natProd = Operator { opRepr = "*"
                   , opName = Prod
                   , opTy = TyAtom ATyNat :-> TyAtom ATyNat :-> TyAtom ATyNat
                   , opAssoc = ALeft
                   , opNotationTy = NInfix
                   , opPrec = 22
                   , opGlyphs = []
                   }

-- | Operador división.
natDiv :: Operator
natDiv = Operator { opRepr = "//"
                   , opName = Div
                   , opTy = TyAtom ATyNat :-> TyAtom ATyNat :-> TyAtom ATyNat
                   , opAssoc = ALeft
                   , opNotationTy = NInfix
                   , opPrec = 22
                   , opGlyphs = []
                   }

-- | Operador resto.
natMod :: Operator
natMod = Operator { opRepr = "%"
                   , opName = Mod
                   , opTy = TyAtom ATyNat :-> TyAtom ATyNat :-> TyAtom ATyNat
                   , opAssoc = ALeft
                   , opNotationTy = NInfix
                   , opPrec = 22
                   , opGlyphs = []
                   }

-- | Operador predecesor
natPred :: Operator
natPred = Operator { opRepr = "pred"
                   , opName = Pred
                   , opTy = TyAtom ATyNat :-> TyAtom ATyNat
                   , opAssoc = None
                   , opNotationTy = NPrefix
                   , opPrec = 23 -- Analizar.
                   , opGlyphs = []
                   }
                   

-- | Operador resta
natDif :: Operator
natDif = Operator { opRepr = "-"
                  , opName = Dif
                  , opTy = TyAtom ATyNat :-> TyAtom ATyNat :-> TyAtom ATyNat
                  , opAssoc = ALeft
                  , opNotationTy = NInfix
                  , opPrec = 21
                  , opGlyphs = []
                  }

-- | Operador negación numerica.
natNegNum :: Operator
natNegNum = Operator { opRepr = "-"
                     , opName = NegNum
                     , opTy = TyAtom ATyNat :-> TyAtom ATyNat
                     , opAssoc = ALeft
                     , opNotationTy = NPrefix
                     , opPrec = 21
                     , opGlyphs = []
                     }

-- | Constructor del tipo de listas polimorficas; el string indica el
-- nombre de la variable de tipo
tyListVar :: String -> Type
tyListVar = TyList . tyVar

-- | La lista vacia.
listEmpty :: Constant
listEmpty = Constant { conRepr = "[]"
                     , conName = Empty
                     , conTy = tyListVar "B"
                     }

-- | Extender la lista con un elemento por la izquierda.
listApp :: Operator
listApp = Operator { opRepr = "▹"
                   , opName = Append
                   , opTy = tyVar "A" :-> tyListVar "A" :-> tyListVar "A"
                   , opAssoc = ARight
                   , opNotationTy = NInfix
                   , opPrec = 12
                   , opGlyphs = ["|>"]
                   }  

-- | Tomar el n-esimo elemento de la lista.
listIndex :: Operator
listIndex = Operator { opRepr = "!"
                     , opName = Index
                     , opTy = tyListVar "A" :-> TyAtom ATyNat :-> tyVar "A"
                     , opAssoc = ALeft
                     , opNotationTy = NInfix
                     , opPrec = 24
                     , opGlyphs = []
                     }

-- | Concatenacion de listas.                     
listConcat :: Operator
listConcat = Operator { opRepr = "++"
                      , opName = Concat
                      , opTy = tyListVar "A" :-> tyListVar "A" :-> tyListVar "A"
                      , opAssoc = ALeft
                      , opNotationTy = NInfix
                      , opPrec = 10
                      , opGlyphs = []
                      }

-- | Cardinal de la lista.
listLength :: Operator
listLength = Operator { opRepr = "#"
                      , opName = Length
                      , opTy = tyListVar "A" :-> TyAtom ATyNat
                      , opAssoc = None
                      , opNotationTy = NPrefix
                      , opPrec = 10
                      , opGlyphs = []
                      }

-- | Toma los primeros n elementos de una lista.
listTake :: Operator
listTake = Operator { opRepr = "↑"
                    , opName = Take
                    , opTy = tyListVar "A" :-> TyAtom ATyNat :-> tyListVar "A"
                    , opAssoc = ALeft
                    , opNotationTy = NInfix
                    , opPrec = 10
                    , opGlyphs = ["^"]
                    }

-- | Tira los primeros n elementos de una lista.
listDrop :: Operator
listDrop = Operator { opRepr = "↓"
                    , opName = Drop
                    , opTy = tyListVar "A" :-> TyAtom ATyNat :-> tyListVar "A"
                    , opAssoc = ALeft
                    , opNotationTy = NInfix
                    , opPrec = 10
                    , opGlyphs = ["!"]
                    }
