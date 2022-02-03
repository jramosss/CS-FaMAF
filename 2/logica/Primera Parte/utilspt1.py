def encontrar_divisores (x):
    for i in range (1,x):
        if x % i == 0:
            print(i)


def main():
    inp = int(input("1: Encontrar Divisores de un numero: "))

    if inp == 1:
        num = int(input("Numero: "))
        print(encontrar_divisores(num))



main()