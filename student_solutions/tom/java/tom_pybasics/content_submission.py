import Ns, os, string, Cookie

######################content_submission.py, by Tom Hickerson, 04/24/01
### the goal: create a content submission page, where content can
### be submitted as a caselist (if only the team_id and content_type
### fields are present in a query) or a comment (if list_id is present
### too.  Content_type might not be required, but we'll see...

conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

#### build chunks of html code, then sql statements

html_form = '''<html><head><title>Content Submission Page
</head></title><body bgcolor=white>
<h2>%s Submission</h2>
<br>
<form action="content_submission2.py">
<table>
<tr><td>Title:</td><td><input type=text name=title size=50 value="%s"></td></tr>
<tr><td>Body of the text:</td><td><textarea name=message cols=50 rows=8></textarea></td></tr>
<tr><td>Your Email Address:</td><td><input type=text name=posted_email size=50></td></tr>
<tr><td>%s</td><td></td></tr>
<tr><td><input type=submit name=submit value="Submit Comment"></td><td></td></tr>
</table>
</form>
</body>
</html>
'''

### html_will need % (caselist/comment,title(if any),rest_of_form)

caselist_form = '''Side:
<select name=side><option value="affirmative">Affirmative</option>
<option value="negative">Negative</option>
</select>
<input type=hidden name=content_type value="caselist">
<input type=hidden name=team_id value=%s>
'''

comment_form = '''
<input type=hidden name=side value=%s>
<input type=hidden name=content_type value="comment">
<input type=hidden name=reference_id value=%s>
<input type=hidden name=team_id value=%s>'''

### caselist_form

insert_caselist_sql = 'INSERT into content (content_type,title,date_posted,team_id,posted_by,message,side) values (\'caselist\',\'%s\',current_timestamp,%s,%s,\'%s\',\'%s\');'

insert_comment_sql = 'INSERT into content (content_type,title,date_posted,team_id,posted_by,message,side,reference_id) values (\'comment\',\'%s\',current_timestamp,%s,%s,\'%s\',\'%s\',%s);'

retrieve_caselist_data_sql = 'SELECT (title,team_id,side) FROM content WHERE list_id = %s;'

retrieve_title_sql = 'SELECT (title,side) FROM content WHERE list_id = %s;'

### insert sql statements, might have to be ported to csub2

### posted_by field will not require a cookie, we will check email instead

query_exists = 0

try:
	query = conn.GetQuery()
	query_exists = 1
except RuntimeError:
	query_exists = 0

### if list_id is in the query, post a comment, else post a caselist

if (query_exists == 1):
	if (query.Get('list_id')):
		value = query.Get('list_id')
		teamvalue = query.Get('team_id')
		search = retrieve_title_sql % (value)
		results = db.Select(search)	
		end_form = comment_form % (results[1],value,teamvalue)
		html = html_form % ('Comment',results[0],end_form)

	elif (query.Get('team_id')):
		teamvalue = query.Get('team_id')
		end_form = caselist_form % (teamvalue)
		html = html_form % ('Caselist','',end_form)
else:
	html = html_form % ('Caselist Default','',caselist_form % (1))

conn.ReturnHtml(200,html)
	

