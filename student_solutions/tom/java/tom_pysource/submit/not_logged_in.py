### Blake Asdorian, 4/25/01
### /project/submit/not_logged_in.py
### This page is the redirect for people who try to access the content pages without first logging in

import Ns
import displayify

html = '''
<TR>
<TD ALIGN="CENTER" BGCOLOR="PINK"><H3>You must log in before you can post content to the site.</H3></TD>
</TR>
'''

conn = Ns.GetConn()
try:
    db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
    html = displayify.displayify(html,db)
    del db
except:
    pass

conn.ReturnHtml(200, html)
