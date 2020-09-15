
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

const char* data = "\n#include <stdio.h>\n#include <string.h>\n#include <stdlib.h>\n\nconst char* data = \"%s\";\n\nchar *escape(const char *s)\n{\n  size_t l = strlen(data);\n  char *t = (char *)malloc(sizeof(char)*l*2);\n  int m = 0, n = 0;\n  while(s[m] != 0)\n  {\n    switch(s[m])\n    {\n    case '\\n':\n      t[n++] = '\\\\';\n      t[n++] = 'n';\n      m++;\n      break;\n    case '\\\"':\n      t[n++] = '\\\\';\n      t[n++] = '\\\"';\n      m++;\n      break;\n    case '\\\\':\n      t[n++] = '\\\\';\n      t[n++] = '\\\\';\n      m++;\n      break;\n    default:\n      t[n++] = s[m++];\n    }\n  }\n  t[n] = 0;\n  return t;\n}\n\nint main()\n{\n  printf(data, escape(data));\n  return 0;\n}\n";

char *escape(const char *s)
{
  size_t l = strlen(data);
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
  printf(data, escape(data));
  return 0;
}
