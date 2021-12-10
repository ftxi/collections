#!/usr/bin/python

import hashlib

def escape(s):
    d = {"\n":"\\n","\"":"\\\"","\\":"\\\\"}
    return "".join(d[k] if k in d else k for k in s)

data = "#!/usr/bin/python\n\nimport hashlib\n\ndef escape(s):\n    d = {\"\\n\":\"\\\\n\",\"\\\"\":\"\\\\\\\"\",\"\\\\\":\"\\\\\\\\\"}\n    return \"\".join(d[k] if k in d else k for k in s)\n\ndata = \"$\"\n\ncontent = data.replace(chr(36),escape(data))\nprint(hashlib.md5(content).hexdigest())\n"

content = data.replace(chr(36),escape(data))
print(hashlib.md5(content).hexdigest())
