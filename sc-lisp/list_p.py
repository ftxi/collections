
# -*- coding:utf-8 -*-
import re

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
    if isinstance(s, tuple):
        return s[1]
    if len(s) == 1:
        return []
    return s[1:]

def atom(s):
    return not isinstance(s, list)

def eq(s, t):
    return s == t

def cond(l, d):
    for [p, e] in cdr(l):
        if eval_(p, d):
            return eval_(e, d)
        
class lambda_object:
    count = 0
    def __init__(self, l, d):
        self.dic = d
        self.li = l[1]
        self.ex = l[2]
        lambda_object.count += 1
        self.serial = lambda_object.count
    
    def __call__(self, *args):
        for i in range(len(self.li)):
            self.dic[self.li[i]] = args[i]
        return eval_(self.ex, self.dic)
    
    def __str__(self):
        return '<COMPOND-PROCEDURE-#%d>' % self.serial
    
    __repr__ = __str__

def label(l, d):
    d[l[1]] = eval_(l[2])
    try:
        if re.match(r'<COMPOND-PROCEDURE-#\d+>', str(d[l[1]])):
            symbol_t[str(d[l[1]])] = '%s' % l[1]
    finally:
        pass

def quote(l, d):
    return l[1]

symbol_s = {'cons':cons, 'car':car, 'cdr':cdr, 'atom?':atom, 'eq?':eq, '#t':True, '#f':False}
syntax_s = {'cond':cond, 'lambda':lambda_object, 'quote':quote, 'label':label}
symbol_t = dict()
for k, v in symbol_s.items():
    symbol_t[str(v)] = '%s' % k
symbol_t[True] = '#t'
symbol_t[False] = '#f'

def sstring(l, align=0):
    if atom(l):
        if str(l) in symbol_t:
            return symbol_t[str(l)]
        elif l == None:
            return 'unspecific-return-value'
        elif isinstance(l, tuple):
            return '%s . %s' % (l[0], l[1])
        else:
            return str(l)
    elif l == []:
        return '()'
    s = '('
    for x in l:
        s += sstring(x, align) + ' '
    return s[:-1] + ')'

def eval_(l, s=symbol_s):
    if atom(l):
        return symbol_s[l]
    eval_.depth += 1
    print '; ='+'>'*eval_.depth, sstring(l)
    if atom(l[0]) and l[0] in syntax_s:
        u = syntax_s[l[0]](l, s)
        print '; ='+'|'*eval_.depth, sstring(u), '--', l[0]
        eval_.depth -= 1
        return u
    else:
        operator = eval_(l[0], s)
        operands = map(lambda e: eval_(e,s), l[1:])
        #print 'sval ='+'|'*eval_.depth, sstring(cons(operator, operands))
        u = operator(*operands)
        print '; -' +'|'*eval_.depth, sstring(u), '<<', '%s[%s]' % (sstring(operator), (len(operands) > 1) and str.join(', ', map(sstring, operands)) or sstring(*operands))
        eval_.depth -= 1
        return u
eval_.depth = 0

if __name__ == '__main__':

    code = '''
(label ff
  (lambda (s)
    (cond
      ((atom? s) s)
      (#t (ff (car s))))))
    '''
    print eval_(parse(code))
    print symbol_s
    print symbol_t
    print sstring(eval_(parse("(cons (ff (quote (((a b) c)))) (quote (d)))")))
    
    eval_(parse('''
    ((cond (#f cdr) (#t car)) (quote a b c))'''))
