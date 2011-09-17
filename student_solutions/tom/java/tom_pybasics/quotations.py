
import Ns, sys
import cgi
import os
from cStringIO import StringIO
from urllib import quote, unquote
import string
import Cookie

## Tom Hickerson, Problem set 1, Exercise 6-9, Talking to the RDBMS
# upgraded version of quotations -- we intend to personalize the 
# board via cookies

html_st = '''<html><head><title>Quotation exercise</title></head>
<body bgcolor=white>
<h2>Quotations aplenty!</h2>
<form action=simple-search.py>Search quotes here:
<input name=sear type=text size=25>
<input type=submit value=Search>
</form>
<hr>'''

quo_head = '''<form method=get action="quotations.py">
<input type=submit value="Exclude Quotes">
<input type=submit name="clear" value="Include All">
<table>
<tr>
<td><i>Exclude</i></td>
<td><i>Topic</i></td>
<td><i>Quote</i></td>
<td><i>Author</i></td>
</tr>'''

quo_fmt = '''<tr>
<td><input type=checkbox value="%s" name=banned></td>
<td>%s</td>
<td>%s</td>
<td>%s</td>
</tr>'''

quo_end = '''</table>
<input type=submit value="Exclude Quotes">
<input type=submit name="clear" value="Include All"></form>
<br>
<br>
<form action=quotation2.py>Enter a new quote:
<input name=quote type=text size=60>
<br>
Who is the author? <input name=author type=text size=20>
<select name=category>
<option value=None selected>Pick a category'''

opt_item = '''<option value=%s>%s'''

form_end = '''</select><br>
Make your own category: <input name=new_cat type=text size=20>
<input type=submit value=Enter>
</form><br>'''

html_end = '''<hr><address>
<a href="tom_hickerson@hotmail.com">tom_hickerson@hotmail.com</a>
</address></body></html>'''

###html form strings are now in order; what follows are error messages

failstr='''<tr>
<td><i>FAIL</i></td>
<td><i>FAIL</i></td>
<td><i>FAIL</i></td>
<td><i>FAIL</i></td>
</tr>'''

###define some functions to read strings

def quoteprn(num,cat,quo,auth):
    print quo_fmt % (num,cat,quo,auth,)

def list_sql(slist):
    if slist:
       return string.join(string.split(slist,' '),',')
    else:
       return '2'

    
###open a connection with the database here

conn = Ns.GetConn()

### begin working with the cookies here

C = Cookie.Cookie()

try:
   C.load(os.environ["HTTP_COOKIE"])
except KeyError:
   pass

form = cgi.FieldStorage()

killed = killme = ''

#x = len(form.getvalue("banned"))
#elif form.has_key("banned") and 

if form.has_key("clear"):
   C["banned"] = "3 4"
elif form.has_key("banned") and (len(form.getvalue("banned")) > 1):
###C["banned"] = C["banned"].value + ' ' + (form.getvalue("banned"))
   C["banned"] = C["banned"].value + ' ' + string.join(form.getvalue("banned"),' ')
elif form.has_key("banned"):
   C["banned"] = C["banned"].value + ' ' + (form.getvalue("banned"))
else:
   try:
      C["banned"] = C["banned"].value
   except:
      C["banned"] = "3 4"

#begin building the web page here

C["banned"]["path"] = "/"
print str(C) + 'Content-type: text/html\n'

Ns.Log(Ns.Notice,str(C))
print html_st

try:
   #print len(form.getvalue("banned"))
   #print x
   killme = ' ' + (form.getvalue("banned"))
   #killme = ' ' + string.join(form.getvalue("banned"),' ')
except TypeError,KeyError:
   #print 'hit an error with killme'
   pass

#try:
killed = C["banned"].value
#except AttributeError, KeyError:
#   pass

Ns.Log(Ns.Notice,"Killme:" + str(killme))

killed = str(killed) + str(killme) 

#print '<br>'
#print killed

print '<br>'
#print killme

try:
   dbase = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
   print 'can\'t get handle -- do you have a database installed??'

### previous sql line here for future reference--
#sql = 'SELECT quotation_id,category,quote,author_name FROM quotations;'

newsql = 'SELECT quotation_id,category,quote,author_name FROM quotations WHERE quotation_id NOT IN (%s);' % (list_sql(killed))

Ns.Log(Ns.Notice,newsql)

print quo_head

try:
   results = dbase.Select(newsql)
   while dbase.GetRow(results) == Ns.OK:
      quoteprn(results[0],results[1],results[2],results[3],)
except:
   print failstr

print quo_end

### a while loop with all the categories

sql2 = 'SELECT DISTINCT category FROM quotations;'

try: 
   results2 = dbase.Select(sql2)
   while dbase.GetRow(results2) == Ns.OK:
      print opt_item % (results2[0],results2[0],)
except:
   print opt_item % ('Test','Test',)

print form_end

print html_end

###close the connection with the database

del dbase

### testing the cookie process
### stolen from python.org: i'm leaving this at the end  as a structured
### cookie-test
### for other pythonites integrating cookies into their code.  
### Read and learn.
#def cookieProcess():
#    C = Cookie.Cookie()
#    try:
#       C.load(os.environ["HTTP_COOKIE"])
#    except KeyError:
#       pass
#    form = cgi.FieldStorage()
#    try:
#       user = form["user"].value
#    except KeyError:
#       pass
#    try:
#       user = C["user"].value
#    except KeyError:
#       user = "nobody"
#    C["user"] = user
#    print C
#    print ''' <form action="quotations.py" method="get">
#              <input type="text" name="user" value="%s">
#        <input type=submit value=Test_Cookie>      
#	</form>''' % cgi.escape(user)
#    print cgi.escape(str(C))









