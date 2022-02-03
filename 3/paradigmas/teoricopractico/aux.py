'''
def aniadir_a_lista_en_diccionario ( diccionario , nombrelista , elemento ):
    if nombrelista in diccionario:
        l = diccionario [nombrelista]
        print ("%s ya tiene %d elementos \n" % (nombrelista ,len (l)))
    else :
        diccionario [ nombrelista ] = []
        print ( " Creamos %s . " % nombrelista )
        diccionario [ nombrelista ]. append ( elemento )
        print ( " Aniadimos %s a %s . " % ( elemento , nombrelista ) )


def excep (diccionario , nombrelista , elemento):
    try: 
        l = diccionario[nombrelista]
    except KeyError:
        diccionario[nombrelista] = []
        diccionario[nombrelista].append(elemento)
    finally:
        print("Honestamente no se que poner aca\n")
'''

def minfilas(a,n,m):
    min = 0
    minimos = [n]
    for i in range (n):
        min = pow(2,32)-1
        for j in range (m):
            print("a[i][j]: " + str(a[i][j]))
            if a[i][j] < min:
                print("si")
                min = a[i][j]
        minimos.append(min)

def main():
    mat = [[1,4,5],[6,1,3],[9,7,2]]
    mins = minfilas(mat,3,3)
    print(mins)


main()
