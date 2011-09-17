
import Ns, sys, cgi, urllib
import string
import xml.parsers.expat
from xml.sax import Contenthandler

#Tom Hickerson, Problem set 1, Exercise 10-11, publishing data in XML
#April 12th, 2001

header = 'Content-type: test/html\n'

html_st = '''<html><head><title>Pset 1, Exercise 11</title></head>
<body bgcolor=white>'''

startform = '''<h3>%s</h3>
<form action=xml-quotations.py>Enter address here:
<input name=address type=text size=25><br>
Enter port here:
<input name=port type=text size=10>(default is 80)<br>
Or, enter filename here:
<input name=file type=text size=20>
<input type=submit value="Parse XML">
</form>'''

resform = '''<h3>%s</h3>%s<br>'''

html_end = '''<hr>
<address>
<a href="tom_hickerson@hotmail.com">tom_hickerson@hotmail.com</a>
</address>
</body>
</html>'''

htmlstr = 'http://%s:%s/basics/quotations-xml'

def initform():
    print header
    print html_st
    print startform % ('Parsing XML from foreign servers')
    print html_end

def errorform(err):
    print header
    print html_st
    print resform % ('Error parsing XML',err)
    print startform % ('Please try again')
    print html_end

def resultform(src,res):
    print header
    print html_st
    print resform % ('Results from ' + src,res)
    print startform % ('Please try again')
    print html_end

def xmlportaddress(xmladdr,xmlport):
    target = htmlstr % (xmladdr,str(xmlport))
    
def xmlfilescan(xmlfile):

def process():
    form = cgi.FieldStorage()
    if form.has_key('address'):
       if form.has_key('port'):
          xmlport = form['port'].value
       else:
          xmlport = '80'
       xmladdr = form['address'].value
       xmlportaddress(xmladdr,xmlport) 
    elif form.has_key('file'):
       xmlfile = form['file'].value
       xmlfilescan(xmlfile)
    else:
       initform()
       

if __name__ == '__main__':
    process()
