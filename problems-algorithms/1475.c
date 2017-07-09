///////////////////////////////////////////////////////
//  Created by sclereid on 2017/7/8.                 //
//  Copyright © 2017 sclereid. All rights reserved.  //
///////////////////////////////////////////////////////

#include <stdio.h>

int m_to_dec(int n, int m);

int main(int argc, const char *argv[])
{
    int n, m;
    scanf("%x %d", &n, &m);
    printf("%d\n", m_to_dec(n, m));
    return 0;
}

int m_to_dec(int n, int m)
{
    return n/0x10 == 0 ?
        n : (n&0xf) + m*m_to_dec(n>>4, m);
}

