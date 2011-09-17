import Ns, Cookie
import re
import displayify

#########content_submission4.py by Tom Hickerson 04/29/01
### the new content_submission2.py, which will act as a confirm 
### page and will show either Back or Submit buttons.  We will construct a
### html form rife with hidden variables and two buttons, and that should
### be that, no sql??  possible.

conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

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

html_form = '''
<html><body bgcolor=white>
<h2>Please Confirm Submission</h2>
<br>
%s
<form action="content_submission3.py" method=post>
<input type=hidden name=title value="%s">
<input type=hidden name=team_id value=%s>
<input type=hidden name=person_id value=%s>
<input type=hidden name=message value="%s">
<input type=hidden name=side value=%s>
%s
<input type=hidden name=content_type value=\'%s\'>
<input type=button value=Back onclick="window.history.back()">
<input type=submit value=Submit>
</form>
</body>
</html>'''

ref_to_line = '<input type=hidden name=reference_id value=%s>'

reply_to_sql = 'SELECT c.title,t.team_name,c.mod_total FROM content c, teams t WHERE c.list_id = %s and c.team_id = t.team_id;'

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

err_html_form = '''<html><body bgcolor=white>
<h2>Error in transaction</h2>
<br>
%s
<form>
<input type=button value=Back onclick="window.history.back()">
</form>
</body>
</html>'''

html = ''
query_exists = 0
error_code = ''
line = ''

try:
	query = conn.GetQuery()
	query_exists = 1
except RuntimeError:
	query_exists = 0

if (query_exists == 1):
	person_id = query.Get('person_id')
	if (person_id == ''):
		error_code = 'A valid email has not been entered.  Please go back and enter one.'
	else:
		
        	userreq = check_user_sql % (person_id)
		try:
			request = db.Select(userreq)
		except:
			error_code = 'A problem with the database was encountered.  Please go back and try again.'
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
			form = caselist_form % (title,line,message,side)
			html = html_form % (form,title,team_id,person_id,message,side,'','caselist')

		if (query.Get('content_type') == 'comment'):
			reference_id = query.Get('reference_id')
			request_reply = reply_to_sql % (reference_id)
			reply_to = db.Select(request_reply)
			ref_line = ref_to_line % (reference_id)
			reply_line = '%s from %s (%s)' % (reply_to[0],reply_to[1],reply_to[2])
			form = comment_form % (title,line,message,side,reply_line)
			html = html_form % (form,title,team_id,person_id,message,side,ref_line,'comment')
	else:
		html=err_html_form % (error_code)

	html = displayify.displayify(html,db)
	conn.ReturnHtml(200,html)
else:
	conn.ReturnRedirect('http://10.11.0.129/project/front.py')

del db
