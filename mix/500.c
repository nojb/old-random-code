#include <stdlib.h>
#include <stdio.h>
#include <time.h>

int main ()
{
    int PRIME[501];
    int J, K, N, Q, R;

    PRIME[1] = 2;
    J = 1;
    N = 3;

P2: J = J + 1;
    PRIME[J] = N;

P3: if (500 == J)
        goto P9;

P4: N = N + 2;
    K = 2;

P6: Q = N / PRIME[K];
    R = N % PRIME[K];

    if (R == 0)
        goto P4;

    if (Q <= PRIME[K])
        goto P2;

    K = K + 1;
    goto P6;

P9: printf ("FIRST FIVE HUNDRED PRIMES\n    ");
    
    for (K = 1; K <= 500; K += 10)
    {
        for (J = 0; J < 10; J ++)
        {
            printf (" %04d", PRIME[K+J]);
        }
        printf ("\n    ");
    }

    return 0;
}
