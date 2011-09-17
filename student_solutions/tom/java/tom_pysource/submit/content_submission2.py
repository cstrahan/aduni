import Ns, Cookie
import re
import displayify

##################content_submission2.py, by Tom Hickerson 04/24/01
### the second page, which will display the content being posted to
### the database together with a button for the user to return to the
### main page(or team page)

conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

### try to retrieve query information 


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
<tr><td>Side:</td><td>%s</td></tr>
</table>'''

comment_form = '''<table>
<tr><td>Title:</td><td>%s</td></tr>
<tr><td>Posted by:</td><td>%s</td></tr>
<tr><td>Message:</td><td>%s</td></tr>
<tr><td>Side:</td><td>%s</td></tr>
<tr><td>Reply to:</td><td>%s</td></tr>
</table>'''

user_line = '%s %s (%s) from %s'

### using a union to create an 'outer join'

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
and p.person_id = (\'%s\');'''

#  UNION ALL
#  SELECT
#     first_names,
#     last_name,
#     email,
#     state,
#     person_id
#  FROM persons
#  WHERE email = (\'%s\');'''

insert_caselist_sql = '''
INSERT 
	into content 
	(content_type,title,date_posted,team_id,person_id,message,side) 
	values 
	(\'caselist\',\'%s\',current_timestamp,%s,%s,\'%s\',\'%s\');'''

insert_comment_sql = '''
INSERT 
	into content 
	(content_type,title,date_posted,team_id,person_id,message,side,reference_id) 
	values 
	(\'comment\',\'%s\',current_timestamp,%s,%s,\'%s\',\'%s\',%s);'''

person_content_sql = '''INSERT into person_school_map (content_id,person_id) VALUES (%s,%s)'''

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
	person_id = query.Get('person_id')
        userreq = check_user_sql % (person_id)
	try:
		request = db.Select(userreq)
	except:
		error_code = 'A valid email was not entered.  Please try again.'
	while (db.GetRow(request) == Ns.OK):
		line = user_line % (request.Get('first_names'),request.Get('last_name'),request.Get('email'),request.Get('school_name'))

        ### html = html + line
	### line added to see what was up with this request, was prone to errors

	message = query.Get('message')
	side = query.Get('side')
	title = query.Get('title')
	team_id = query.Get('team_id')
	#Ns.Log(Ns.Notice,request[4])
	gtlt_check = re.compile(r'(<|>)',re.DOTALL)
	message = gtlt_check.sub('',message)		
	title = gtlt_check.sub('',title)		

if not(error_code):
	if (query.Get('content_type') == 'caselist'):
		submit = insert_caselist_sql % (re.escape(title),team_id,re.escape(person_id),re.escape(message),side)
		form = caselist_form % (title,line,message,side)

	if (query.Get('content_type') == 'comment'):
		reference_id = query.Get('reference_id')
		submit = insert_comment_sql % (re.escape(title),team_id,person_id,re.escape(message),side,reference_id)
		request_reply = reply_to_sql % (reference_id)
		reply_to = db.Select(request_reply)
		reply_line = '%s from %s (%s)' % (reply_to[0],reply_to[1],reply_to[2])
		form = comment_form % (title,line,message,side,reply_line)

        ########### this should be an atomic transaction properly written in pl/sql
	try:
		db.DML(submit)
        except:
		error_code = 'We have encountered a problem with the database.  Please go back to your submission page and make sure you entered a valid email address.  If you still have problems, contact the administrator.'

if (error_code):
	html = html + err_html_form % (error_code)
else:
	html = html + html_form % (form)

html = displayify.displayify(html,db)
conn.ReturnHtml(200, html)





















