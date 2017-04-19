# -*- coding: utf-8 -*-

import re
import os

from math import *

print os.uname()
print

while True :
    code = raw_input('>>> ')
    if re.match(r'.+:\s*$', code) :
        while True :
            tmp = raw_input('... ')
            code = code + '\n' + tmp
            if tmp == '' :
                break
    try :
        ans = eval(code)
        if ans != None :
            if ans.__repr__() != '':
                print ans.__repr__()
            else :
                print ans
    except Exception, _err :
        try :
            exec code
        except Exception, err :
            m = re.match(r"^<.*'.*\.(.*)'>$", '%s' % type(err))
            print '%s:' % m.group(1), err
    finally :
        print
