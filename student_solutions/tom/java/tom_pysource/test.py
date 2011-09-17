import Ns
import ss_cookie

conn = Ns.GetConn()

cookie = ss_cookie.get_cookie()
    
html = '''
<HTML>
<HEAD><TITLE>User Registration</TITLE></HEAD>
<BODY>
<H3>This is a test page</H3>
<HR>
%s
</BODY>
</HTML>
''' % (cookie)

conn.ReturnHtml(200, html)
