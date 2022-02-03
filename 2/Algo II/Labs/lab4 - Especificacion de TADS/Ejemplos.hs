import Defecto
import TADNatural
import TADCosto
import TADGrafo
import TADCGrafo
import TADMatriz
import TADConjunto

instance Defecto Costo where
  defecto = infinitoC

uno = suc cero
dos = suc uno
tres = suc dos
cuatro = suc tres
cinco = suc cuatro
seis = suc cinco
siete = suc seis
ocho = suc siete
nueve = suc ocho
diez = suc nueve
once = suc diez
doce = suc once
trece = suc doce

grafo1 = vacíoG
grafo2 = aristaG cero uno grafo1
grafo3 = aristaG cero dos grafo2
grafo4 = aristaG cero tres grafo3
grafo5 = aristaG uno dos grafo4
grafo6 = aristaG tres dos grafo5
grafo7 = aristaG dos uno grafo6

grafoC1 = vacíoCG
grafoC2 = aristaCG cero uno siete grafoC1
grafoC3 = aristaCG cero dos cuatro grafoC2
grafoC4 = aristaCG cero tres uno grafoC3
grafoC5 = aristaCG uno dos uno grafoC4
grafoC6 = aristaCG tres dos tres grafoC5
grafoC7 = aristaCG dos uno dos grafoC6

-- ejercicio completar los "AAA" de las especificaciones provistas
-- (hacer grep AAA * para averiguar dónde están)

-- ejercicio calcular los costos del camino óptimo entre dos vértices en el grafo7 jugando con la operación costo_caminoCG

-- ejercicio: agregar lo que haga falta al tad CGrafo para generar su matriz de adyacencia (matriz donde en la celda i j se encuentra el costo de la arista que va de i a j o infinito si no existe). Usar el tad Matriz.

