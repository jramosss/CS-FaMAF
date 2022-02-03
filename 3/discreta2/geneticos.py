from random import randint, shuffle

#Fi -> Fitness
#F` -> sum(Fi)/n
#Ei (esperanzas) -> Fi/F`

#!Para ruleta
#Calcular Sj = sum(Ei) para i < = j
#Se tiran n numeros al azar rk entre 0 y k 
#Para cada k se selecciona reproducirse el pimer j tal que Sj >= rk
def mengele_ruleta (fitness,randoms,n,j):
    fprima = sum(fitness)/n 
    S = [0 for _ in range (0,j)]
    randoms = [r*n for r in randoms]
    #Esperanza usual TODO, usar la esperanza sigma scaling
    esperanzas = [f/fprima for f in fitness]

    for i in range (0,j):
        for t in range (0,i):
            S[i] += esperanzas[t]

    best_genes = []

    i = 0
    for k in randoms:
        for ji in S:
            if ji >= k:
                best_genes.append(i)
                break
            i+=1
        i = 0

    return best_genes[0:n]


def mengele_SUS (fitness,n,j,k):
    fprima = sum(fitness)/n 
    S = [0 for _ in range (0,j)]
    esperanzas = [f/fprima for f in fitness]
    r = randint(0,1)    
    perm = [i for i in range (0,n-1)]
    shuffle(perm)
    randoms = [k + r for k in perm]

    for i in range (0,j):
        for t in range (0,i):
            S[i] += esperanzas[t]

    best_genes = []

    i = 0
    for k in randoms:
        for ji in S:
            if ji >= k:
                best_genes.append(i)
                break
            i+=1
        i = 0
    
    return best_genes[0:n]



if __name__ == '__main__':
    ALEATORIOS1 = [0.72 ,0.15 ,0.38 ,0.57 ,0.88 ,0.32 ,0.22 ,0.98]
    ALEATORIOS2 = [0, 22,0.54,0.81,0.12,0.75,0.64,0.47,0.33]
    FITNESS1    = [0.3,90.8 ,45.2 ,71.7 ,30.2 ,9.3]
    FITNESS2    = [7.7,0.3,0.5,0.9,4.1,2.5]
    FITNESS3    = [8.09,0.16,7.07,3.59,9.98,4.07,6.52,9.1]
    #ruleta1 = mengele_ruleta([0.3,90.8 ,45.2 ,71.7 ,30.2 ,9.3],ALEATORIOS1,6,6)
    #ruleta2 = mengele_ruleta([0.22,0.54,0.81,0.12,0.75,0.64,0.47,0.33],ALEATORIOS1,8,8)
    #print("Ruleta1: ",ruleta1)
    #print("Ruleta2: ",ruleta2)
    sus1 = mengele_SUS(FITNESS1,len(FITNESS1),len(FITNESS1),1.1)
    print(sus1)