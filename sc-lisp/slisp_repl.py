
# -*- coding: utf-8 -*-

from slisp import *

def unbalance_count(s):
    k = 0
    for c in s:
        if c == '(':
            k += 1
        elif c == ')':
            k -= 1
    return k

class natuals_t:
    def __init__(self):
        self.i = 0
    
    def __iter__(self):
        return self
    
    def next(self):
        self.i += 1
        return self.i
    
    def rewind(self):
        self.i -= 1
        return self.i
    
def _lisp_wrapper(s):
    def _lisp_function(f):
        symbol_s[s] = f
        symbol_t[str(f)] = s
        return f
    return _lisp_function
    
def lisp_function(f):
    if isinstance(f, str):
        return _lisp_wrapper(f)
    return _lisp_wrapper(f.__name__)(f)

N = natuals_t()

_inputs = [['repl']]
_outputs = [[None]]

@lisp_function
def inputs():
    return _inputs

@lisp_function
def outputs():
    return _outputs

@lisp_function('eval')
def _eval(ex):
    return eval_(ex)

@lisp_function('null?')
def _null(s):
    return s == []

@lisp_function
def save():
    with open('002__lisp_.txt', 'a+') as file_ :
        for i in range(len(_inputs))[1:]:
            print >> file_
            print >> file_, sstring(_inputs[i])
            print >> file_, '; value:', sstring(_outputs[i])

@lisp_function
def load():
    with open('002__lisp_.txt', 'a+') as file_ :
        while True:
            s = file_.readline()
            if s == '':
                continue
            if s[0] == '(':
                print s
                eval_(s)
        
for n in N:
    code = raw_input('\n%2d ]=> ' % n)
    if code == '':
        N.rewind()
        continue
    k = unbalance_count(code)
    while k > 0:
        code += '\n' + raw_input(' '*(2*k+6))
        k = unbalance_count(code)
    try:
        ex = parse(code)
        value = eval_(ex)
        _inputs.append(ex)
        _outputs.append(value)
        print '; value:', sstring(value)
    except SyntaxError, err:
        print '\nsyntax error: %s:' % err
        N.rewind()
    
    except Exception, err:
        m = re.match(r"^<.*'.*\.(.*)'>$", '%s' % type(err))
        print '\n%s:' % m.group(1), err
        N.rewind()
    finally:
        pass
    
