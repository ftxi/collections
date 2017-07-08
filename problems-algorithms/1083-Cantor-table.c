///////////////////////////////////////////////////////
//  Created by sclereid on 2017/7/7.                 //
//  Copyright © 2017 sclereid. All rights reserved.  //
///////////////////////////////////////////////////////

#include <stdio.h>

int main(int argc, const char *argv[])
{
    int n;
    scanf("%d", &n);
    int i, s, k;
    i = 0, s = 0;
    while(s<=n-1)
        s += ++i;
    s -= i;
    k = n-1 - s;
    if(i%2==1)
        printf("%d/%d\n", i-k, k+1);
    else
        printf("%d/%d\n", k+1, i-k);
    return 0;
}
