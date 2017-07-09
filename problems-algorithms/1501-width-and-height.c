///////////////////////////////////////////////////////
//  Created by sclereid on 2017/7/8.                 //
//  Copyright © 2017 sclereid. All rights reserved.  //
///////////////////////////////////////////////////////

#include <stdio.h>


int main(int argc, const char *argv[])
{
    int lev[1005], wids[1005];
    int n, i, u, v, dep = 0, wid = 0;
    scanf("%d", &n);
    lev[1] = 1;
    for(i = 0; i <= n; i++)
    wids[i] = 0;
    for(i = 1; i <= n; i++)
    {
        scanf("%d %d", &u, &v);
        lev[u] = lev[v] = lev[i]+1;
        u != 0 && (wids[lev[u]]++, lev[u] > dep && (dep = lev[u]));
        v != 0 && (wids[lev[v]]++, lev[v] > dep && (dep = lev[v]));
    }
    for(i = 1; i <= dep; i++)
        wids[i] > wid && (wid = wids[i]);
    n == 1 && (puts("1 1"), exit(0), 1);
    printf("%d %d\n", wid, dep);
    return 0;
}
