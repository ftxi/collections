///////////////////////////////////////////////////////
//  Created by sclereid on 2017/7/8.                 //
//  Copyright © 2017 sclereid. All rights reserved.  //
///////////////////////////////////////////////////////

#include <stdio.h>
#include <math.h>

int main(int argc, const char *argv[])
{
    int n, a[105], i, average = 0, ans = 0;
    scanf("%d", &n);
    for(i = 0; i < n; i++)
    {
        scanf("%d", a+i);
        average += a[i];
    }
    average /= n;
    for(i = 0; i < n; i++)
    {
        if(a[i] == average)
            continue;
        a[i+1] += a[i] - average;
        a[i] = average;
        ans++;
    }
    printf("%d\n", ans);
    return 0;
}
