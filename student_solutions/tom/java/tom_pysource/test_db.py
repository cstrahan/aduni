####################################################################################################################
### front.py
### Bryon Gill 4/22/01

import Ns
import string
import re

conn = Ns.GetConn()

try:
     db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
     conn.ReturnRedirect('error_page')


select_string = "select now() as one, now() as two"
return_html = ''
results = db.Select(select_string)
while (db.GetRow(results) != Ns.END_DATA):
        return_html = return_html + "<li>%s,%s</li>" % ( results.IGet('one') , results.IGet('two') )


html = '''<html><head><title>hi</title></HEAD></BODY><ol>%s</ol></body></html>''' % (return_html)


conn.ReturnHtml(200, html)


















