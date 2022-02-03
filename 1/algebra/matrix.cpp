#include <iostream>

using namespace std;

int suma (int** m1,int** m2) {
    const int fm1 = (sizeof(m1)/sizeof(m1[0]));
    const int fm2 = (sizeof(m2)/sizeof(m2[0]));
    const int cm1 = (sizeof(m1[0])/sizeof(m1[0][0]));
    const int cm2 = (sizeof(m2[0])/sizeof(m2[0][0]));
    int sum = 0;
    for (int i = 0; i < fm1; i++)
        for (int j = 0; j < cm1; j++) 
            sum += m1[i][j] + m2[i][j];

    return sum;
}

int main (void) {
    int** m1 = (int*)malloc(9*sizeof(int));
    int** m2 = (int*)malloc(9*sizeof(int));

    m1 = {{1,2,3},{4,5,6}};
    m2 = {{10,20,30},{40,50,60}};

    int sum = suma(m1,m2);

    cout << "SUMA: " << sum;

    return 0;
}