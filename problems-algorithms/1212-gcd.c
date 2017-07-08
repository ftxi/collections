///////////////////////////////////////////////////////
//  Created by sclereid on 2017/7/8.                 //
//  Copyright © 2017 sclereid. All rights reserved.  //
///////////////////////////////////////////////////////

#include <stdio.h>

int gcd(int, int);

int main(int argc, const char *argv[])
{
    int m, n;
    scanf("%d %d",&m, &n);
    printf("%d\n", gcd(m, n));
    return 0;
}

int gcd(int a, int b)
{
    return b == 0 ? a : gcd(b, a%b);
}
