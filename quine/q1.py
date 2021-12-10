#!/usr/bin/python

def escape(s):
    d = {"\n":"\\n","\"":"\\\"","\\":"\\\\"}
    return "".join(d[k] if k in d else k for k in s)

data = "#!/usr/bin/python\n\ndef escape(s):\n    d = {\"\\n\":\"\\\\n\",\"\\\"\":\"\\\\\\\"\",\"\\\\\":\"\\\\\\\\\"}\n    return \"\".join(d[k] if k in d else k for k in s)\n\ndata = \"$\"\n\nprint(data.replace(chr(36),escape(data)))\n"

print(data.replace(chr(36),escape(data)))
