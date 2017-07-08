///////////////////////////////////////////////////////
//  Created by sclereid on 2017/7/7.                 //
//  Copyright © 2017 sclereid. All rights reserved.  //
///////////////////////////////////////////////////////

#include <stdio.h>

int count_unique_factors(int m);

int main(int argc, const char *argv[])
{
    int m, n;
    scanf("%d %d",&m, &n);
    if(n%m!=0)
        puts("0");
    else
        printf("%d\n", 1<<count_unique_factors(n/m));
    return 0;
}

int count_unique_factors(int m)
{
    int n = 0, last = 0;
    static int i;
    for(i = 2; i <= m; i++)
    {
        if(m%i==0)
        {
            m/=i;
            if(last!=i)
                last=i, n++;
            i--;
        }
    }
    return n;
}
