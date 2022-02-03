data IntExpr = ConstI Int    -- Constantes
| VI VarName              -- Variables enteras
| Neg IntExpr             -- Negación
| Plus IntExpr IntExpr    -- Suma
| Prod IntExpr IntExpr    -- Producto
| Div IntExpr IntExpr     -- División
| Mod IntExpr IntExpr     -- Módulo

data BoolExpr = ConstB Bool             -- Constantes
| VB VarName             -- Variables booleanas
| And BoolExpr BoolExpr  -- Conjunción
| Or BoolExpr BoolExpr   -- Disjunción
| Not BoolExpr           -- Negación
| Equal IntExpr IntExpr    -- igual
| Less IntExpr IntExpr   -- Menor estricto

data Statement = Skip          -- No hacer nada
| AssignB Var BoolExpr      -- Asignación booleana
| AssignI Var IntExpr       -- Asignación entera
| Seq Statement Statement   -- Secuencia
| If [(BoolExpr,Statement)] -- Condicional
| Do BoolExpr Statement     -- Ciclo
deriving Show


