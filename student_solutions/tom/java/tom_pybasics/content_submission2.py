import Ns, Cookie

##################content_submission2.py, by Tom Hickerson 04/24/01
### the second page, which will display the content being posted to
### the database together with a button for the user to return to the
### main page(or team page)

conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

### build html code, sql statements

html_form = '''
<html><body bgcolor=white>
<h2>Submitted to Content</h2>
<br>
%s
<a href="../front.py">Back to main page</a>
</body></html>'''

err_html_form = '''<html><body bgcolor=white>
<h2>Error in transaction</h2>
<br>
%s
<form>
<input type=button value=Back onclick="window.history.back()">
</form>'''

caselist_form = '''<table>
<tr><td>Title:</td><td>%s</td></tr>
<tr><td>Posted by:</td><td>%s</td></tr>
<tr><td>Message:</td><td>%s</td></tr>
<tr><td>Side: %s</td><td></td></tr>
</table>'''

comment_form = '''<table>
<tr><td>Title:</td><td>%s</td></tr>
<tr><td>Posted by:</td><td>%s</td></tr>
<tr><td>Message:</td><td>%s</td></tr>
<tr><td>Side: %s</td><td>Reply to: %s</td></tr>
</table>'''

user_line = '%s %s (%s) from %s'

check_user_sql = '''
SELECT 
   p.first_names,
   p.last_name,
   p.email,
   s.school_name,
   p.person_id 
FROM persons p, schools s, person_school_map psm 
WHERE psm.person_id = p.person_id 
and psm.school_id = s.school_id 
and p.email = (\'%s\');'''

insert_caselist_sql = 'INSERT into content (content_type,title,date_posted,team_id,posted_by,message,side) values (\'caselist\',\'%s\',current_timestamp,%s,%s,\'%s\',\'%s\');'

insert_comment_sql = 'INSERT into content (content_type,title,date_posted,team_id,posted_by,message,side,reference_id) values (\'comment\',\'%s\',current_timestamp,%s,%s,\'%s\',\'%s\',%s);'

reply_to_sql = 'SELECT c.title,t.team_name,c.mod_total FROM content c, teams t WHERE c.list_id = %s and c.team_id = t.team_id;'

html = ''
query_exists = 0
error_code = ''
submit = ''
form = ''
line = ''
posted_by = ''

try:
	query = conn.GetQuery()
	query_exists = 1
except RuntimeError:
	query_exists = 0

if (query_exists == 1):
	emailreq = query.Get('posted_email')
	userreq = check_user_sql % (emailreq)
	try:
		request = db.Select(userreq)
	except:
		error_code = 'A valid email was not entered.  Please try again.'
	while db.GetRow(request) == Ns.OK:
		line = line + user_line % (request[0],request[1],request[2],request[3])
		posted_by = posted_by + (request[4])
	#html = html + line
	### line added to see what was up with this request, was prone to errors

	message = query.Get('message')
	side = query.Get('side')
	title = query.Get('title')
	team_id = query.Get('team_id')
	#Ns.Log(Ns.Notice,request[4])
		
if not(error_code):
	if (query.Get('content_type') == 'caselist'):
		submit = insert_caselist_sql % (title,team_id,posted_by,message,side)
		form = caselist_form % (title,line,message,side)

	if (query.Get('content_type') == 'comment'):
		reference_id = query.Get('reference_id')
		submit = insert_comment_sql % (title,team_id,posted_by,message,side,reference_id)
		request_reply = reply_to_sql % (reference_id)
		reply_to = db.Select(request_reply)
		reply_line = '%s from %s (%s)' % (reply_to[0],reply_to[1],reply_to[2])
		form = comment_form % (title,line,message,side,reply_line)

	try:
		db.DML(submit)
	except:
		error_code = 'We have encountered a problem with the database.'

if (error_code):
	html = html + err_html_form % (error_code)
else:
	html = html + html_form % (form)

conn.ReturnHtml(200, html)