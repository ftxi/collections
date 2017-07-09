///////////////////////////////////////////////////////
//  Created by sclereid on 2017/7/8.                 //
//  Copyright © 2017 sclereid. All rights reserved.  //
///////////////////////////////////////////////////////

#include <stdio.h>
#include <math.h>

int n, a[105], b[105];

void swap_i(int *, int *);
void qsort_(int low, int high);

int main(int argc, const char *argv[])
{
    int i, current, contracts;
    scanf("%d", &n);
    for(i = 0; i < n; i++)
    {
        scanf("%d %d", a+i, b+i);
        if(a[i] > b[i])
            swap_i(a+i, b+i);
    }
    qsort_(0, n-1);
    current = b[0];
    contracts = 1;
    for(i = 0; i < n; i++)
        printf("%d %d\n", a[i], b[i]);
    puts("---------");
    printf("%d %d\n", a[0], b[0]);
    for(i = 1; i < n; i++)
    {
        if(a[i] >= current)
        {
            contracts++;
            current = b[i];
            printf("%d %d\n", a[i], b[i]);
        }
    }
    printf("%d\n", contracts);
    return 0;
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
        x = b[high];
        i = low-1;
        for(j = low; j < high; j++)
        {
            if(b[j] < x)
            {
                i++;
                swap_i(a+i, a+j);
                swap_i(b+i, b+j);
            }
        }
        swap_i(a+i+1, a+high);
        swap_i(b+i+1, b+high);
        qsort_(low, i);
        qsort_(i+2, high);
    }
}
