import Ns
import zz

####### admin_login, by Tom Hickerson, 04/26/01
### assigns a supervisor cookie if person enters an email and the administrator
### password--currently hard-coded into the system as "ukraine"

conn = Ns.GetConn()

html_login = '''
<html><body bgcolor='white'>
<h3>Administrator Login</h3>
<hr>
<form action="admin_login.py">
<table>
<tr><td>Login Email:</td><td><input type=text name=who size=20></td></tr>
<tr><td>Login Passcode:</td><td><input type=password name=what size=20></td></tr>
</table>
<input type=submit>
</form>
</body></html>'''

query_exists = 0
mail = 'bauschar@bc.edu'
passwd = 'ukraine'

try:
	query=conn.GetQuery()
	query_exists = 1
except RuntimeError:
	query_exists = 0

if (query_exists == 1):
	who = query.Get('who')
	what = query.Get('what')
	if ((who==mail) and (what==passwd)):
		zz.set_cookie('bosphorous','straight',1)
		conn.ReturnRedirect('http://10.11.0.129/project/admin/admin2.py')
	else:
		conn.ReturnRedirect('http://10.11.0.129/project/front.py')
else:
	conn.ReturnHtml(200,html_login)

