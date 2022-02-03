type Ballena = tuple
              unsigned int life
              string name 

Fun salvar_ballenas (B : set of ballena, T : Nat) ret  S list of ballena  {
Var c : set of Ballenas
Var r = 0 : Nat // tiempo
Var b : Ballena
C = set_copy (B)
// Invariante: c tiene ballenas vivas y no salvadas
while (c != vacio) {
     do 
          b = mejor_ballena (c) // mas muerta, de acuerdo al criterio de seleccion, elijo el mejor candidato
          s = s : b // le pego por atras
          c = c - {b}
          r = r + t // sumo el tiempo que gaste en salvar a la mejor ballena
          sacar_muertas (c,r)
  }
}
{Pre c != 0}
fun mejor_ballena (c : set of ballena) ret b: Ballena {
var n = inf : nat
    for c in C do --› 
         if (c.life ‹ n) --›
             n = c.life;
             b = c;
         fi
    od
end fun
}

proc sacar_muertas ( in /out C : set of Ballenas, in r : Nat) {
var d : set of Ballenas
     d = set_copy (c)
     for c in D do --›
         if (c.life ‹ r) --›
             C = C - {c}
         fi
} 