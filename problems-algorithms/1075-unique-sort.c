///////////////////////////////////////////////////////
//  Created by sclereid on 2017/7/7.                 //
//  Copyright © 2017 sclereid. All rights reserved.  //
///////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>

typedef int dtype;

typedef struct {
    dtype *val;
    size_t len;
} array;

array unique_sort(array);
array unique_merge(array, array);


int main(int argc, const char * argv[])
{
    int n;
    scanf("%d", &n);
    dtype *a_ = (dtype *)malloc(sizeof(dtype)*n);
    int i;
    for(i = 0; i < n; i++)
        scanf("%d", a_+i);
    array a = {a_, n};
    array b = unique_sort(a);
    printf("%d\n", (int)b.len);
    for(i = 0; i < b.len; i++)
        printf("%d ", b.val[i]);
    puts("");
    return 0;
}

array unique_sort(array a)
{
    if(a.len==1)
        return a;
    size_t mid = a.len/2;
    array b = {a.val, mid};
    array c = {a.val + mid, a.len - mid};
    return unique_merge(unique_sort(b), unique_sort(c));
}

array unique_merge(array a, array b)
{
    size_t len = a.len + b.len;
    dtype* c_ = (int *)malloc(sizeof(int)*len);
    int m = 0, n = 0;
    int k = 0;
    while (m + n < len)
    {
        if(n >= b.len || (m < a.len && a.val[m] < b.val[n]))
        {
            c_[k] = a.val[m];
            m++;
        }
        else
        {
            c_[k] = b.val[n];
            n++;
        }
        if(k == 0 || c_[k-1] < c_[k])
            k++;
    }
    array c = {c_, k};
    return c;
}
