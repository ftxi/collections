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

array merge_sort(array);
array merge(array, array);


int main(int argc, const char * argv[])
{
    int n;
    scanf("%d", &n);
    dtype *a_ = (dtype *)malloc(sizeof(dtype)*n);
    int i;
    for(i = 0; i < n; i++)
        scanf("%d", a_+i);
    array a = {a_, n};
    array b = merge_sort(a);
    for(i = 0; i < b.len; i++)
        printf("%d ", b.val[i]);
    puts("");
    return 0;
}

array merge_sort(array a)
{
    if(a.len==1)
        return a;
    size_t mid = a.len/2;
    array b = {a.val, mid};
    array c = {a.val + mid, a.len - mid};
    return merge(merge_sort(b), merge_sort(c));
}

array merge(array a, array b)
{
    size_t len = a.len + b.len;
    dtype* c_ = (dtype *)malloc(sizeof(int)*len);
    array c = {c_, len};
    int m = 0, n = 0;
#define k (m + n)
    while (k < c.len)
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
    }
#undef k
    return c;
}
