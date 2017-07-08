///////////////////////////////////////////////////////
//  Created by sclereid on 2017/7/8.                 //
//  Copyright © 2017 sclereid. All rights reserved.  //
///////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

long long square(long long);
long long expmod(long long, long long, long long);
int fermat_test(long long, long long);

int main(int argc, const char *argv[])
{
    int n, i;
    srand((unsigned int)time(NULL));
    scanf("%d", &n);
    for(i = 0; i < 100; i++)
        if(!fermat_test(n, rand()%n))
            puts("\\n"), exit(0);
    puts("\\t");
    return 0;
}

long long square(long long x)
{
    return x*x;
}

long long expmod(long long base, long long exp, long long mod)
{
    if(exp==0)
        return 1;
    if(exp&1)
        return (base * expmod(base, exp-1, mod))%mod;
    else
        return square(expmod(base, exp/2, mod))%mod;
    
}

int fermat_test(long long n, long long a)
{
    return expmod(a, n, n) == a;
}

