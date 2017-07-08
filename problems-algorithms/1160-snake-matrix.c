///////////////////////////////////////////////////////
//  Created by sclereid on 2017/7/7.                 //
//  Copyright © 2017 sclereid. All rights reserved.  //
///////////////////////////////////////////////////////

#include <stdio.h>

#define change_direction (direction = (direction+1)%4)

enum {down, right, up, left} direction = right;

int main(int argc, const char *argv[])
{
    int n, i;
    scanf("%d", &n);
    int a[105][105];
    int x = (n-1)/2, y = (n-1)/2;
    int t = 0, s = 0;
    while (x>=0&&x<n&&y>=0&&y<n)
    {
        t++;
        for(i = 0; i < t; i++)
        {
            s++;
            a[x][y] = s;
            switch (direction)
            {
                case down:
                    y++;
                    break;
                case right:
                    x++;
                    break;
                case up:
                    y--;
                    break;
                case left:
                    x--;
                    break;
            }
        }
        change_direction;
        for(i = 0; i < t; i++)
        {
            s++;
            a[x][y] = s;
            switch (direction)
            {
                case down:
                    y++;
                    break;
                case right:
                    x++;
                    break;
                case up:
                    y--;
                    break;
                case left:
                    x--;
                    break;
            }
        }
        change_direction;
    }
    for(y = 0; y < n; y++)
    {
        for(x = 0; x < n; x++)
            printf("%d ", a[x][y]);
        putchar('\n');
    }
    s = 0;
    for(i = 0; i < n; i++)
    {
        s += a[i][i];
        if(i != n-i)
        {
            s += a[i][n-i];
        }
    }
    printf("%d\n", s);
    return 0;
}
