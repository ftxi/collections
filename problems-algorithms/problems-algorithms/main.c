///////////////////////////////////////////////////////
//  Created by sclereid on 2017/7/7.                 //
//  Copyright © 2017 sclereid. All rights reserved.  //
///////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int *val;
    size_t len;
} array;

array merge_sort(array);
void swap_i(int *, int *);
array merge(array, array);


int main(int argc, const char * argv[])
{
    int a[] = {2, 1};
    array arr = {a, 2};
    merge_sort(arr);
    int i;
    for(i = 0; i < arr.len; i++)
        printf("%d\n", arr.val[i]);
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

void swap_i(int *a, int *b)
{
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

array merge(array a, array b)
{
    size_t len = a.len + b.len;
    int* c_ = (int *)malloc(sizeof(int)*len);
    array c = {c_, len};
    //
    return c;
}
