import Ns, os, sys
import string, re

#################team-admin.py, by Tom Hickerson, 4/19/01
### Administrator update/validate/invalidate for the teams database.
###
###

conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

#### create blocks of html code, sql commands

html_form = '''<html><head><title>
Administrating TEAMS Database</title></head>
<body bgcolor=white>
<h3>Administrating Teams</h3>
%s
%s
%s
<hr>
</body>
</html>'''

validate_form = '''The following teams have been %s:
<table>
<tr><td>Id #</td><td>Team Name</td><td>School Name</td><td>Date Created</td>
<td>Member One</td><td></td><td>Member Two</td><td></td></tr>
%s
</table>
'''

validate_line = '''<tr>
<td>%s</td><td>%s</td><td>%s</td><td>%s</td>
<td>%s</td><td>%s</td><td>%s</td><td>%s</td>
</tr>'''

validate_list = ''

validate_button = '''<form method=post action="admin2.py">
<input type=submit value="Back to Admin">
</form>'''

update_form = '''You asked to update the following:
<form method=post action="team-admin.py">
%s
%s
</form>
'''

update_line = '''<table>
<tr>Date Created:%s<br>ID #: %s <br>School Name: %s<br>
Update this record <input type=checkbox name=selected value="%s"></tr>

<tr><td>Team Name:</td>
<td><input type=text size=20 name=team_name value="%s"></td>
<td>Current Status: %s<select name=status>
<option value="approved" selected>Approved</option>
<option value="not approved">Not Approved</option>
<option value="not reviewed">Not Reviewed</option>
</select>
</td></tr>

<tr><td>Member One--First, Last Name:</td>
<td><input type=text size=20 name=member_one_first_name value="%s"></td>
<td><input type=text size=20 name=member_one_last_name value="%s"></td>
</tr>

<tr><td>Member Two--First, Last Name:</td>
<td><input type=text size=20 name=member_two_first_name value="%s"></td>
<td><input type=text size=20 name=member_two_last_name value="%s"></td>
</tr>

</table>
<hr>
'''

commit_form = '''The following entries have been changed:
%s
<table>
%s
</table>
'''

commit_line = '''<tr><td>Team Name:</td>
<td>%s</td><td>Current Status:%s</td>
</tr>
<tr><td>Member One:</td>
<td>%s</td>
<td>%s</td></tr>
<tr><td>Member Two:</td><td>%s</td><td>%s</td></tr>
<hr>
'''

commit_list = ''

update_list = ''

update_button = '''<input type=submit name=commit value="Submit changes and return to Admin">'''

### rationale for the three esses: message, form, buttons

### sql strings: we'll need one for getting all non-reviewed persons so as to
### change them to reviewed

validate_sql = 'SELECT t.team_id,t.team_name,s.school_name,t.date_created,t.member_one_first_name,t.member_one_last_name,t.member_two_first_name,t.member_two_last_name FROM teams p, schools s WHERE t.school_id = s.school_id and t.status = \'not reviewed\';'

invalidate_sql = 'SELECT t.team_id,t.team_name,s.school_name,t.date_created,t.member_one_first_name,t.member_one_last_name,t.member_two_first_name,t.member_two_last_name FROM teams p, schools s WHERE t.school_id = s.school_id'

inv_where_sql = ' and p.person_id = (%s)'

update_sql = 'SELECT t.date_created,t.team_id,s.school_name,t.team_name,t.status,t.member_one_first_name,t.member_one_last_name,t.member_two_first_name,t.member_two_last_name FROM teams t,schools s WHERE t.school_id = s.school_id'

update_changes_sql = 'UPDATE teams SET team_name = \'%s\',status=\'%s\',member_one_first_name=\'%s\',member_one_last_name=\'%s\',member_two_first_name=\'%s\',member_two_last_name=\'%s\' WHERE team_id = \'%s\''

validate_changes_sql = 'UPDATE teams SET status = \'approved\''

invalidate_changes_sql = 'UPDATE teams SET status = \'not approved\''

where_sql = ' WHERE team_id in (%s)'

and_where_sql = ' and t.team_id in (%s)'

### get form data and start sql processes
html = 'hello'
form = ''
sql_list = ''
query_exists = 0
xyz=''
error=''

select_list = '0'

try:
	query = conn.GetQuery()
	query_exists = 1
except RuntimeError:
	query_exists = 0

#def update_records():

if (query_exists == 1):
	if (query.Get('commit')):
		query_size = query.Size()
		for item in range(query_size):
			if (query.Key(item) == 'selected'):
				commit_sql = update_changes_sql % (query.Value(item+1),query.Value(item+2),query.Value(item+3),query.Value(item+4),query.Value(item+5),query.Value(item+6),query.Value(item+7),query.Value(item+8),query.Value(item+9),query.Value(item+10),query.Value(item+11),query.Value(item+12),query.Value(item+13),query.Value(item))
				try:
					db.DML(commit_sql)
				except:
					error = error + 'There was a database problem updating the file of %s %s' % (query.Value(item+1),query.Value(item+3))
				commit_list = commit_list + commit_line % (query.Value(item+1),query.Value(item+2),query.Value(item+3),query.Value(item+4),query.Value(item+5),query.Value(item+6),query.Value(item+7),query.Value(item+8),query.Value(item+9),query.Value(item+10),query.Value(item+11),query.Value(item+12),query.Value(item+13))			

			
		commitf = commit_form % (error,commit_list)
		html = html_form % ('',commitf,validate_button)

	if (query.Get('update')):
		query_size = query.Size()
		for num in range(query_size):
			if (query.Key(num)=='selected'):
				select_list = select_list + ',' + (query.Value(num))
		search = update_sql + and_where_sql % (select_list)
		uresults = db.Select(search)
		while db.GetRow(uresults) == Ns.OK:
			update_list = update_list + update_line % (uresults[0],uresults[1],uresults[1],uresults[2],uresults[3],uresults[4],uresults[5],uresults[6],uresults[7],uresults[8],uresults[9],uresults[10],uresults[11],uresults[12],uresults[13],uresults[14])
			sql_list = sql_list + (uresults[0]) + ','
		form = update_form % (update_list,update_button)
		sql_list = sql_list + '1'
		#db.DML()
		html = html_form % ('',form,update_button)
	
	if (query.Get('invalid')):
		query_size = query.Size()
		for num in range(query_size):
			if (query.Key(num)=='selected'):
				select_list = select_list + ',' + (query.Value(num))
		search = invalidate_sql + (and_where_sql % (select_list))
		iresults = db.Select(search)
		while db.GetRow(iresults) == Ns.OK:
			validate_list = validate_list + validate_line % (iresults[0],iresults[1],iresults[2],iresults[3],iresults[4])
			sql_list = sql_list + (iresults[0]) + ','
		form = validate_form % ('invalidated',validate_list)
		sql_list = sql_list + '1'
		db.DML(invalidate_changes_sql + where_sql % (sql_list))
		html = html_form % ('',form,validate_button)
	
	if (query.Get('validate')):
		search = validate_sql
		vresults = db.Select(search)
		while db.GetRow(vresults) == Ns.OK:
			validate_list = validate_list + validate_line % (vresults[0],vresults[1],vresults[2],vresults[3],vresults[4])
			sql_list = sql_list + (vresults[0]) + ','
		form = validate_form % ('validated',validate_list)
		sql_list = sql_list + '1'
		html = html_form % ('',form,validate_button)
		db.DML(validate_changes_sql + where_sql % (sql_list))
else:
	conn.ReturnRedirect(200,'http://10.11.0.117:8000/basics/admin2.py')

conn.ReturnHtml(200,html)
