import Ns, os, sys
import string, re

#################person_admin.py, by Tom Hickerson, 4/19/01
### Administrator update/validate/invalidate for the persons database.
###
###

#################the page below has four receiving functions:
### update,
### commit,
### validate, and
### invalidate.
### Validate and invalidate return a screen that say the following
### selected users will be validated/invalidated.  Update pulls up an
### entire screen that allows the admin to change users stats, and
### then returns the page to commit, which updates the table and
### returns a page showing what has been changed, or any errors.

conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

#### create blocks of html code, sql commands

html_form = '''<html><head><title>
Administrating PERSONS Database</title></head>
<body bgcolor=white>
<h3>Administrating Persons</h3>
%s
%s
%s
<hr>
</body>
</html>'''

validate_form = '''The following people have been %s:
<table>
<tr><td>Id #</td><td>Name</td><td></td><td>Email</td><td>School</td></tr>
%s
</table>
'''

validate_line = '''<tr>
<td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>
</tr>'''

validate_list = ''

validate_button = '''<form method=post action="admin2.py">
<input type=submit value="Back to Admin">
</form>'''

update_form = '''You asked to update the following:
<form method=post action="person_admin.py">
%s
%s
</form>
'''

update_line = '''<table>
<tr>Date Created:%s<br>ID #: %s Update this record <input type=checkbox name=selected value="%s"></tr>

<tr><td>First Name:</td>
<td><input type=text size=20 name=first_names value="%s"></td>
<td>Current Access: %s<select name=access_level>
<option value="basic" selected>Basic</option>
<option value="admin">Admin</option></select>
</td></tr>

<tr><td>Last Name:</td>
<td><input type=text size=20 name=last_name value="%s"></td>
<td>Current Status: %s<select name=status>
<option value="approved" selected>Approved</option>
<option value="not approved">Not Approved</option>
<option value="not reviewed">Not Reviewed</option>
</select>
</td></tr>

<tr><td>Url:</td>
<td><input type=text size=40 name=url value="%s"></td>
<td>Email:  <input type=text size=30 name=email value="%s"></td></tr>

<tr><td>Street Address:</td>
<td><input type=text size=40 name=street_address value="%s"></td>
<td></td></tr>

<tr><td>City/State/Zip:</td>
<td><input type=text size=16 name=city value="%s"> 
<input type=text size=4 name=state value="%s"> 
<input type=text size=10 name=zipcode value="%s"> </td></tr>

<tr><td>Office/Mobile/Fax:</td>
<td><input type=text size=16 name=office_phone value="%s"> 
<input type=text size=16 name=mobile_phone value="%s"> 
<input type=text size=16 name=fax value="%s"> </td></tr>
</table>
<hr>
'''

commit_form = '''The following entries have been changed:
%s
<table>
%s
</table>
'''

commit_line = '''<tr><td>First Name:</td>
<td>%s</td><td>Current Access:%s</td>
</tr>
<tr><td>Last Name:</td>
<td>%s</td>
<td>Current Status:%s</td></tr>
<tr><td>Url:</td><td>%s</td><td>Email:%s</td></tr>
<tr><td>Street Address:</td><td>%s</td><td></td></tr>
<tr><td>City:%s</td><td>State:%s</td><td>Zip:%s</td></tr>
<tr><td>Office Phone:%s</td><td>Mobile Phone:%s</td><td>Fax:%s</td></tr>'''

commit_list = ''

update_list = ''

update_button = '''<input type=submit name=commit value="Submit changes and return to Admin">'''

### rationale for the three esses: message, form, buttons

### sql strings: we'll need one for getting all non-reviewed persons so as to
### change them to reviewed

validate_sql = 'SELECT p.person_id,p.first_names,p.last_name,p.email,s.school_name FROM persons p, person_school_map psm, schools s WHERE p.person_id = psm.person_id and psm.school_id = s.school_id and p.status = \'not reviewed\';'

invalidate_sql = 'SELECT p.person_id,p.first_names,p.last_name,p.email,s.school_name FROM persons p, person_school_map psm, schools s WHERE p.person_id = psm.person_id and psm.school_id = s.school_id'

inv_where_sql = ' and p.person_id = (%s)'

### old update sql string: will we allow schools to be edited from this page???
#update_sql = 'SELECT p.date_registered,p.person_id,p.first_names,p.access_level,p.last_name,p.status,p.url,p.email,p.street_address,p.city,p.state,p.zipcode,p.office_phone,p.mobile_phone,p.fax FROM persons p, person_school_map psm, schools s WHERE p.person_id = psm.person_id and psm.school_id = s.school_id and p.person_id = (%s);'

update_sql = 'SELECT date_registered,person_id,first_names,access_level,last_name,status,url,email,street_address,city,state,zipcode,office_phone,mobile_phone,fax FROM persons'

update_changes_sql = 'UPDATE persons SET first_names = \'%s\',access_level = \'%s\',last_name=\'%s\',status=\'%s\',url=\'%s\',email=\'%s\',street_address=\'%s\',city=\'%s\',state=\'%s\',zipcode=\'%s\',office_phone=\'%s\',mobile_phone=\'%s\',fax=\'%s\' WHERE person_id = \'%s\''

validate_changes_sql = 'UPDATE persons SET status = \'approved\''

invalidate_changes_sql = 'UPDATE persons SET status = \'not approved\''

where_sql = ' WHERE person_id in (%s)'

and_where_sql = ' and p.person_id in (%s)'

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
		search = update_sql + where_sql % (select_list)
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









