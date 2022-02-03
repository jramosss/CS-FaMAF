module TADBooleano where
data Boolean = T | F

conjuncion :: Boolean -> Boolean -> Boolean
conjuncion T x = x
conjuncion F x = F

disyuncion :: Boolean -> Boolean -> Boolean
disyuncion T _ = T
disyuncion F x = x

implica :: Boolean -> Boolean -> Boolean
implica T F = F
implica x y = T

eq :: Boolean -> Boolean -> Boolean
eq T T = T
eq T F = F
eq F T = F
eq F F = F

not :: Boolean -> Boolean
not T = F
not F = T