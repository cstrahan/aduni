import Ns, sys, cgi, string
import Cookie, os

#simple-search: looking for variable 'sear'
#goal: to reconstruct a web page, with search results

html_st = '''<html><head><title>Quotation exercise</title></head>
<body bgcolor=white>
<h2>Quotation searches aplenty!</h2>
<form action=simple-search.py>Perform another search here:
<input name=sear type=text size=25>
<input type=submit value=Search>
</form>
<br>
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


#quo_fmt = '''<tr><td>%s</td><td>%s</td><td>%s</td></tr>'''

#quo_head = '''<table>
#<tr><td><i>Topic</i></td><td><i>Quote</i></td><td><i>Author</i></td></tr>'''

html_end = '''</form><hr>
You can <a href="quotations.py">go home</a> again.
<address>
<a href="tom_hickerson@hotmail.com">tom_hickerson@hotmail.com</a>
</address></body></html>'''

nosuccess = '''<i>No matches were found.</i>'''

failstr = '''<tr>
<td><i>FAIL</i></td>
<td><i>FAIL</i></td>
<td><i>FAIL</i></td>
<td><i>FAIL</i></td></tr>'''

#error = ''

nosear = '''No string is listed for searching
              <form><input type=button value=Back 
              onclick="window.history.back()"></form>'''

def quoteprn(num,cat,quo,auth):
    print quo_fmt % (num,cat,quo,auth,)

def list_sql(slist):
    if slist:
       return string.join(string.split(slist,' '),',')
    else:
       return '2'

def process():
    error = ''
    results = ''
    conn = Ns.GetConn()

    C = Cookie.Cookie()

    try:
       C.load(os.environ["HTTP_COOKIE"])
    except KeyError:
       pass

    C["banned"] = C["banned"].value

    killed = C["banned"].value

    C["banned"]["path"] = "/"
    print 'Content-type: text/html\n'
    print html_st

    form = cgi.FieldStorage()

    try:
       dbase = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
    except:
       print 'can\'t get handle -- do you have a database installed??'

    if form.has_key('sear'):
       subj = string.replace(form['sear'].value,"'","\\\'")
    else:
       error = nosear

    if not error:
       sql = 'SELECT quotation_id,category,quote,author_name FROM quotations WHERE ((upper(quote) LIKE (upper(\'%%%s%%\'))) or (upper(category) LIKE (upper(\'%%%s%%\'))) or (upper(author_name) LIKE (upper(\'%%%s%%\')))) and quotation_id NOT IN (%s);' % (subj,subj,subj,list_sql(killed))
       try:
          results = dbase.Select(sql)
       except:
          print failstr
       
       tag1 = 0

       if (results):
          print quo_head
          while dbase.GetRow(results) == Ns.OK:
             quoteprn(results[0],results[1],results[2],results[3],)
             tag1 = 1

       if tag1==0:
          error = nosuccess

    print '</table>'
    print error
    print html_end

    del dbase

if __name__ == '__main__':
   process()


