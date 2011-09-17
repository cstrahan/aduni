import Ns, sys, cgi, string

errmsg = '''Content-type: text/html\n
<html><head><title>Error!</title></head>
<body bgcolor=white>
<h3>Error!</h3>
Your quote did not insert correctly.<br>
<i>%s</i>
<form><input type=button value=Back 
onclick="window.history.back()"></form>
</body></html>'''

okmsg = '''Content-type: text/html\n
<html><head><title>Success!</title></head>
<body bgcolor=white>
<h3>Success!</h3>
<br>
Your quote is now inserted.  Click
<a href="quotations.py">here</a> to see the new table.
<br>
<hr>
</body></html>'''

def showError(msg):
    print errmsg % (msg)

def confstr(str):
    string.replace(str,"'","Q")
    string.replace(str,'"','Q')
    return string.strip(str)

def showSuccess():
    print okmsg

def processInput():
    error = ''
    form = cgi.FieldStorage()

    if form.has_key('author'):
       who = string.replace(form['author'].value,"'","\\\'")
    else:
       error = 'No author is listed<br>'

    if form.has_key('quote'):
       what = string.replace(form['quote'].value,"'","\\\'")
    else:
       error = error + 'No quote was included<br>'

    if form.has_key('new_cat'):
       why = string.replace(form['new_cat'].value,"'","\\\'")    
    elif form['category'].value:
       why = string.replace(form['category'].value,"'","\\\'")
    else:
       why = 'Test'

    if not error:
       conn = Ns.GetConn()
       try:
         dbase = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
       except:
          print 'can\'t get handle -- do you have a database installed??'
       sql = 'INSERT INTO quotations (quotation_id,insertion_date,author_name,category,quote) values ((select nextval(\'q_id\')),current_date,\'%s\',\'%s\',\'%s\');' % (who, why, what)
       try:
          dbase.DML(sql)
          showSuccess()
       except:
          showError('Database entry error')
       del dbase       
    else:
       showError(error)

if __name__ == '__main__':
   processInput()

    

