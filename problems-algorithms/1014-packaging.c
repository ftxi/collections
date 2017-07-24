///////////////////////////////////////////////////////
//  Created by sclereid on 2017/7/9.                 //
//  Copyright © 2017 sclereid. All rights reserved.  //
///////////////////////////////////////////////////////

#include <stdio.h>
#include <math.h>

typedef char boolean;

boolean foo, bar;

int n, v, a[105];

void swap_i(int *, int *);
void qsort_(int low, int high);
int search(int k, int remain);

int main(int argc, const char *argv[])
{
    int i, m, tmp;
    scanf("%d", &v);
    scanf("%d", &n);
    for(i = 0; i < n; i++)
        scanf("%d", a+i);
    qsort_(0, n-1);
    m = v;
    for(i = 0; i < n; i++)
    {
        tmp = search(i, v);
        if(tmp >= 0 && tmp < m)
            m = tmp;
    }
    printf("%d\n", m);
    return 0;
}

int search(int k, int remain)
{
    int i;
    int u = remain - a[k], m = u, tmp;
    for(i = 0; i < k; i++)
    {
        tmp = search(i, u);
        if(tmp >= 0 && tmp < m)
            m = tmp;
    }
    return m;
}

void swap_i(int *a, int *b)
{
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

void qsort_(int low, int high)
{
    if(low<high)
    {
        int i, j, x;
        x = a[high];
        i = low-1;
        for(j = low; j < high; j++)
        {
            if(a[j] < x)
            {
                i++;
                swap_i(a+i, a+j);
            }
        }
        swap_i(a+i+1, a+high);
        qsort_(low, i);
        qsort_(i+2, high);
    }
}

