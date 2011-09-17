import re

def html_strip(input):
    html_construct = re.compile(r'<.*>')
    output = re.sub(html_construct,'',input)
    return output
