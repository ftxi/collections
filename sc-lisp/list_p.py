
# -*- coding:utf-8 -*-
import re

def parse(s):
    l = re.sub(r'\s+', ', ', (' '+s.lower()+' ').replace('(', '[').replace(')', ']'))[2:-2]
    return eval(re.sub(r'(?P<symbol>[\w#%\\/^*+_\|~<>?!:-]+)', lambda m : '"%s"' % m.group('symbol'), l))

def cons(a, d):
    return (lambda *args : list(args))(a, *d)

def car(s):
    return s[0]

def cdr(s):
    if len(s) == 1:
        return []
    return s[1:]

def atom(s):
    return not isinstance(s, list)

def eq(s, t):
    return s == t

def cond(l, s):
    for [p, e] in cdr(l):
        if eval_(p, s):
            return eval_(e, s)

class lambda_object:
    def __init__(self, l, d):
        self.dic = d
        self.li = l[1]
        self.ex = l[2]

    def __call__(self, *args):
        for i in range(len(self.li)):
            self.dic[self.li[i]] = args[i]
        return eval_(self.ex, self.dic)

def label(l, d):
    d[l[1]] = eval_(l[2])

def quote(l, d):
    return l[1]

symbol_s = {'cons':cons, 'car':car, 'cdr':cdr, 'atom?':atom, 'eq?':eq, '#t':True, '#f':False}
syntax_s = {'cond':cond, 'lambda':lambda_object, 'quote':quote, 'label':label}

def eval_(l, s=symbol_s):
    if atom(l):
        return symbol_s[l]
    if l[0] in syntax_s:
        return syntax_s[l[0]](l, s)
    else:
        operator = eval_(l[0], s)
        operands = map(lambda e: eval_(e,s), l[1:])
        return operator(*operands)

code = '''
(label ff
  (lambda (s)
    (cond
      ((atom? s) s)
      (#t (ff (car s))))))
'''

print eval_(parse(code))
print symbol_s

print eval_(parse("(ff (quote (((a b) c))))"))
