from numpy import matmul

if __name__ == '__main__':
    #choice = int(input("1: Multiplicacion"))
    m1 = [[1,1,1,1,1],[1,2,0,3,4],[1,2,3,0,4],[0,1,2,3,4],[1,0,2,3,4]]
    m2 = [1,0,5,2,0]
    print(matmul(m1,m2))