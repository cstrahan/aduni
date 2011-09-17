import Ns, os, string, Cookie
import displayify
import zz
######################content_submission.py, by Tom Hickerson, 04/24/01
### the goal: create a content submission page, where content can
### be submitted as a caselist (if only the team_id and content_type
### fields are present in a query) or a comment (if list_id is present
### too.  Content_type might not be required, but we'll see...

### modified gt-check, lt-check on 04/29/01, tbh

conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

############### check for cookie, if none is found, redirect to login page.
############### if one is found, generate who-am-i list based on school affiliated emails

school_id = zz.get_cookie('valid_code')

if not (school_id):
        conn.ReturnRedirect('http://10.11.0.129/project/submit/not_logged_in.py')
        
############################ find if there is a query

#### build chunks of html code, then sql statements

html_form = '''<html><head><title>Content Submission Page
</head></title><body bgcolor=white>
<h2>%s Submission</h2>
<br>
<form method="post" action="content_submission4.py">
<table>
%s
<tr><td>Title:</td><td><input type=text name=title size=50 value="%s"></td></tr>
<tr><td>Body of the text:</td><td><textarea name=message cols=50 rows=8 wrap=virtual></textarea></td></tr>
<tr><td>Your Email Address:</td><td>%s</td></tr>
<tr>%s</tr>
<tr><td colspan=2 align="CENTER"><input type=submit name=submit value="Submit Comment"></td><td></td></tr>
</table>
</form>
</body>
</html>
'''

#################### build the school email list ##################
email_sql = '''SELECT person_id,email from persons WHERE person_id IN (SELECT person_id from person_school_map WHERE school_id = %s)''' % (school_id)

email_req = db.Select(email_sql)

email_list = ''
while (db.GetRow(email_req) != Ns.END_DATA):
        email_list = email_list + '''<OPTION VALUE="%s">%s</OPTION>''' % (email_req.IGet('person_id'),email_req.IGet('email'))

if (email_list != ''):
        email_list = '<SELECT NAME=person_id><OPTION VALUE=""></OPTION>' + email_list + '</SELECT>'

#  ###################### build school list #############
#  school_sql = '''SELECT school_name,school_id from schools'''
#  school_req = db.Select(school_sql)
#  school_list = ''
#  while (db.GetRow(school_req) != Ns.END_DATA):
#          school_list = school_list + '''<OPTION VALUE="%s">%s</OPTION>''' % (school_req.IGet('school_id'),school_req.IGet('school_name'))
#  if (school_list != ''):
#          school_list = '<SELECT NAME=school_id><OPTION VALUE=""></OPTION>' + school_list + '</SELECT>'
####################### define the two form versions
#  version_1 = '''Team who ran these arguments: <select name=team_id>%s</select></FORM><FORM METHOD="POST" ACTION="content_submission.py"><INPUT TYPE=SUBMIT NAME="new_team" VALUE="new_team">'''
#  version_2 = '''Team who ran these arguments: %s <input type=text name=team_initials size=3>''' 


team_list = ''

team_line = '<option value="%s">%s %s</option>'

### html_will need % (caselist/comment,title(if any),rest_of_form)

caselist_form = '''<td>Side:
<select name=side><option value="affirmative">Affirmative</option>
<option value="negative">Negative</option>
</select></td>
<input type=hidden name=content_type value="caselist">
<input type=hidden name=team_id value=%s>
'''


team_select = '''<TR><TD colspan="2">Team who ran these arguments: <select name=team_id>
%s</select> </TD></TR><TR><TD colspan="2"><A HREF="/project/registration/team_registration.py">Team not listed? Register them here.</A></TD></TR>'''


default_form = '''<td>Side:
<select name=side><option value="affirmative">Affirmative</option>
<option value="negative">Negative</option>
</select></td><input type=hidden name=content_type value="caselist">'''


comment_form = '''<td></td><td></td>
<input type=hidden name=side value=%s>
<input type=hidden name=content_type value="comment">
<input type=hidden name=reference_id value=%s>
<input type=hidden name=team_id value=%s>'''

### caselist_form

insert_caselist_sql = '''
INSERT 	into content 
	(content_type,title,date_posted,team_id,posted_by,message,side) 
values 	(\'caselist\',\'%s\',current_timestamp,%s,%s,\'%s\',\'%s\');'''

insert_comment_sql = '''
INSERT 	into content 
	(content_type,title,date_posted,team_id,posted_by,message,side,reference_id) 
values 	(\'comment\',\'%s\',current_timestamp,%s,%s,\'%s\',\'%s\',%s);'''

retrieve_caselist_data_sql = 'SELECT (title,team_id,side) FROM content WHERE list_id = %s;'

retrieve_title_sql = 'SELECT title,side FROM content WHERE list_id = %s;'

team_listing_sql = '''
SELECT
	t.team_id,
	s.school_name,
	t.team_name
FROM
	teams t, schools s
WHERE
	t.school_id = s.school_id;'''


just_one_team_listing_sql = '''
SELECT
	t.team_id,
	s.school_name,
	t.team_name
FROM
	teams t, schools s
WHERE
	t.school_id = s.school_id
        t.team_id =%s;'''

### union not necessary???
#UNION ALL
#SELECT
	#team_id,
	#NULL,
	#team_name
#FROM
	#teams;'''
### insert sql statements, might have to be ported to csub2
### posted_by field will not require a cookie, we will check email instead

### if list_id is in the query, post a comment, else post a caselist
html = ''
query_exists = 0

try:
	query = conn.GetQuery()
	query_exists = 1
except RuntimeError:
	query_exists = 0

###if it's a comment:
if (query_exists == 1):
	if (query.Get('list_id')):
		value = query.Get('list_id')
		teamvalue = query.Get('team_id')
                search = retrieve_title_sql % (value)
                results = db.Select(search)
                db.GetRow(results)
                end_form = comment_form % (results.Get('side'),value,teamvalue)
                html = html_form % ("", 'Comment',results.Get('title'),email_list,end_form)
###if it's a caselist entry linked from a particular team page:
                ###NOTE: THIS SEEMS TO BE BROKEN AT THE MOMENT!
        elif (query.Get('team_submit_code')):
                team_id = query.Get('team_submit_code')
                try:
                        one_teamlist = db.Select(just_one_team_listing_sql % team_id)
                        while db.GetRow(one_teamlist) == Ns.OK:
                                team_list = team_list + team_line % (teamlist[0],teamlist[1],teamlist[2])
                except:
                        conn.ReturnRedirect('/project/front.py')
                html = html_form % ("Caselist",team_select % team_list, '',email_list,default_form)
###if it's a new submission without a default team selected (e.g., from the front page)
else:
	try:
		teamlist = db.Select(team_listing_sql)
		while db.GetRow(teamlist) == Ns.OK:
			team_list = team_list + team_line % (teamlist[0],teamlist[1],teamlist[2])
        except:
		team_list='<option value="1">Default Team</option>'
                
	html = html_form % ('Caselist Default', team_select % team_list, '',email_list,default_form)

html = displayify.displayify(html,db)
conn.ReturnHtml(200,html)
	

















