
# -*- coding:utf-8 -*-
import re

SYNTAX = 0
VALUE = 1
trivil_count = 0

def parse(s):
    l = re.sub(r'\s+', ', ', (' '+s.lower()+' ').replace('(', '[').replace(')', ']'))[2:-2]
    return eval(re.sub(r'(?P<symbol>[\w#%\\/^*+_\|~<>?!:-]+)', lambda m : '"%s"' % m.group('symbol'), l))

def cons(a, d):
    if atom(d):
        return (a, d)
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

def cond(l):
    for [p, e] in cdr(l):
        if eval_(p):
            return eval_(e)

def _assoc(li, dict):
    for i in range(len(li)):
        if atom(li[i]):
            if li[i] in dict:
                li[i] = ['quote', dict[li[i]]]
        elif li[0] != 'quote':
            _assoc(li[i], dict)

def lambda_(l):
    def ret(*args):
        dict = {'':''}
        for i, key in enumerate(l[1]):
            dict[key] = args[i]
        _assoc(l[2], dict)
        return eval_(l[2])
    global trivil_count
    trivil_count += 1
    ret.__name__ = 'COMPOND-PROCEDURE-#%d' % trivil_count
    return ret  

def label(l):
    symbol_s[l[1]] = l[0]
    symbol_s[l[1]] = eval_(l[2])

def quote(l):
    return l[1]

def eval_(l):
    print 'code =>', l
    if atom(l):
        return symbol_s[l]
    if not atom(l[0]):
        l[0] = eval_(l[0])
    if l[0] in syntax_s:
        return syntax_s[l[0]](l)
    else:
        for i in range(len(l))[1:]:
            l[i] = eval_(l[i])
        print 'sval =>', l
        if isinstance(l[0], str):
            l[0] = symbol_s[l[0]]
        return l[0](*l[1:])

symbol_s = {'cons':cons, 'car':car, 'cdr':cdr, 'atom?':atom, 'eq?':eq, '#t':True, '#f':False}
syntax_s = {'cond':cond, 'lambda':lambda_, 'quote':quote, 'label':label}


'''
(cdr ((lambda (x)
  (cons x
        (cons x
              (quote ())))) (quote (once twice))))
'''

code = '''
(LABEL not
  (lambda (s)
    (cond
      (s #f)
      (#t #t))))
'''
'''
(cons (quote apple)
  (cons (quote banana)
    (cdr (quote (orange pineapple)))))
'''
'''
(label ff
  (lambda (s)
    (cond
      ((atom? s) s)
      (#t (ff (car s))))))
'''

#print parse(code)
#print _inside('foo', parse(code))
print eval_(parse(code))
print symbol_s

print eval_(parse("(not #f)"))
