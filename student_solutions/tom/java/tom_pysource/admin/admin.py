import Ns
import string
import sys, os

############Admin.py v1.1 by Tom Hickerson, 4/18/01
#### serves the form for the administrator to show data from tables
#### note: programming style differs from Blake's, whereas Blake 
#### constructs the whole page in one quote, I serve it in chunks,
#### which I just use print in the place of Ns.ServeHtml(200...)
#### Not sure which is better.

##############set up all the chunks of html code here

header = 'Content-type: text/html\n'

html_start = '''<html><body bgcolor=white>
<h3>Administration Page</h3>
<br>
<hr>
New Users -- %s new users have signed up since your last login<br>
<form action=admin2.py>
<input type=submit name=accept value="Accept Selected">
<input type=submit name=delete value="Invalidate Selected">
<input type=submit name=acceptall value="Accept All">
<table><tr>
<td></td>
<td>First names</td>
<td>Last name</td>
<td>Email</td>
<td>School</td></tr>'''

html_users = '''<tr>
<td><input type=checkbox value="%s" name=selected></td>
<td>%s</td>
<td>%s</td>
<td>%s</td>
<td>null</td>
</tr>'''

html_users_end = '''</table>
<input type=submit name=accept value="Accept Selected">
<input type=submit name=delete value="Invalidate Selected">
<input type=submit name=acceptall value="Accept All">
</form>'''

html_post_start = '''New Postings--%s new posts have been made since your last
login<br>
<form action=admin2.py>
<input type=submit name=accept_post value="Accept Selected">
<input type=submit name=delete_post value="Hide Selected">
<input type=submit name=acceptall_post value="Accept All">
<table>
<tr>
<td></td>
<td>Subject</td>
<td>Posted by</td>
<td>To group</td>
</tr>'''

html_posts = '''<tr>
<td><input type=checkbox value="%s" name=selected_post></td>
<td>%s</td>
<td>%s</td>
<td>%s</td>
<td>null</td>
</tr>'''

html_post_end = '''</table>
<input type=submit name=accept_post value="Accept Selected">
<input type=submit name=delete_post value="Hide Selected">
<input type=submit name=acceptall_post value="Accept All">
</form>'''

########begin the connection and set up sql strings here

conn = Ns.GetConn()

adminsql = 'SELECT last_login from users where user_id=1;'
newusersql = 'SELECT user_id,first_names,last_name,email FROM users;'
countsql = 'SELECT COUNT(*) from users;'

########setup several dummy values, error and dbase

error = ''
dbase= ''

########begin to fetch info through sql Selects

try:
   dbase = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
   error = 'can\'t get handle -- do you have a database installed??'

try:
	results = dbase.Select(adminsql)
	cutoff_date = results[0]

except:
	error = 'Unable to get cutoff date '

usercount = 0

try:
	results3 = dbase.Select(countsql)
	usercount = results3[0]
except:
	error = error + 'Unable to get number of new users'

#################begin the actual html page

print header
print html_start % (usercount)

try:
	results2 = dbase.Select(newusersql)
	while dbase.GetRow(results2) == Ns.OK:
	   print html_users % (results2[0],results2[1],results2[2],results2[3],)	
except:
	error = error + 'Unable to get user list'

print error
print html_users_end
print '<hr>'
print cutoff_date


print'</body></html>'



