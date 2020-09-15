
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char *escape(char *s)
{
  size_t l = strlen(s);
  char *t = (char *)malloc(sizeof(char)*l*2);
  int m = 0, n = 0;
  while(s[m] != 0)
  {
    switch(s[m])
    {
    case '\n':
      t[n++] = '\\';
      t[n++] = 'n';
      m++;
      break;
    case '\"':
      t[n++] = '\\';
      t[n++] = '\"';
      m++;
      break;
    case '\\':
      t[n++] = '\\';
      t[n++] = '\\';
      m++;
      break;
    default:
      t[n++] = s[m++];
    }
  }
  t[n] = 0;
  return t;
}

int main()
{
  char s[10000];
  char *t = s;
  while(gets(t) != NULL)
  {
    t = t + strlen(t);
    *(t++) = '\n';
  }
  *t = '\0';
  puts(escape(s));
  return 0;
}
