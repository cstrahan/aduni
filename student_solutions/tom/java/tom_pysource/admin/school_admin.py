import Ns

####################school_admin.py, by Tom Hickerson 04/23/01

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

### errors addressed, 4/26 tbh

conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

#### create blocks of html code, sql commands

html_form = '''<html><head><title>
Administrating SCHOOLS Database</title></head>
<body bgcolor=white>
<h3>Administrating Schools</h3>
%s
%s
%s
<hr>
</body>
</html>'''

validate_form = '''The following schools have been %s:
<table>
<tr><td>Id #</td><td>Name</td><td>NDT Dist.</td><td>CEDA Region</td>
<td>ADA Member</td></tr>
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
<form method=post action="school_admin.py">
%s
%s
</form>
'''

update_line = '''<table>
<tr bgcolor='EEFFFA'>Date Created:%s<br>ID #: %s Update this record <input type=checkbox name=selected value="%s" checked></tr>

<tr bgcolor='white'><td>Name:</td>
<td><input type=text size=20 name=school_name value="%s"></td>
</tr>

<tr bgcolor='EEFFFA'><td>NDT District:</td>
<td><input type=text size=20 name=ndt_district value="%s"></td></tr>

<tr bgcolor='white'><td>CEDA Region:</td>
<td><input type=text size=20 name=ceda_region value="%s"></td></tr>

<tr bgcolor='EEFFFA'><td>ADA Member: %s</td>
<td><select name=ada_member><option value=true selected>True</option>
<option value=false>False</option></select></td></tr>

<tr bgcolor='white'>
<td>Current Status: %s</td><td><select name=status>
<option value="approved" selected>Approved</option>
<option value="not approved">Not Approved</option>
<option value="not reviewed">Not Reviewed</option>
</select>
</td></tr>

<tr bgcolor='EEFFFA'><td>Password:</td>
<td><input type=text size=20 name=school_password value="%s"></td></tr>

</table>
<hr>
'''

commit_form = '''The following entries have been changed:
%s
<table>
%s
</table>
'''

commit_line = '''<tr bgcolor='EEFFFA'><td><b>Name: %s</b></td>
<td></td><td>NDT District: %s</td>
</tr>
<tr bgcolor='white'><td>CEDA Region: %s</td>
<td>ADA Member: %s</td>
<td>Status: %s</td></tr>
<tr bgcolor='EEFFFA'><td>Password: %s</td><td></td><td></td></tr>
'''

commit_list = ''

update_list = ''

update_button = '''<input type=submit name=commit value="Submit changes and return to Admin">'''

### rationale for the three esses: message, form, buttons

### sql strings: we'll need one for getting all non-reviewed persons so as to
### change them to reviewed

validate_sql = '''
SELECT 
	school_id, 
	school_name, 
	ndt_district, 
	ceda_region, 
	ada_member 
FROM schools 
WHERE status=\'not reviewed\';'''

invalidate_sql = 'SELECT school_id, school_name, ndt_district, ceda_region, ada_member FROM schools'

update_sql = '''
SELECT 
	date_registered,
	school_id,
	school_name,
	ndt_district,
	ceda_region,
	ada_member,
	status,
	school_password 
FROM schools'''

update_changes_sql = '''
UPDATE 
	schools 
SET school_name = \'%s\',
	ndt_district = \'%s\',
	ceda_region=\'%s\',
	ada_member=\'%s\',
	status=\'%s\',
	school_password=\'%s\' 
WHERE school_id = \'%s\''''

validate_changes_sql = 'UPDATE schools SET status = \'approved\''

invalidate_changes_sql = 'UPDATE schools SET status = \'not approved\''

where_sql = ' WHERE school_id in (%s)'

and_where_sql = ' and school_id in (%s)'

### get form data and start sql processes

html = 'hello'
form = ''
sql_list = ''
query_exists = 0
#xyz=''
error=''

select_list = ''

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
				commit_sql = update_changes_sql % (query.Value(item+1),query.Value(item+2),query.Value(item+3),query.Value(item+4),query.Value(item+5),query.Value(item+6),query.Value(item))
				try:
					db.DML(commit_sql)
				except:
					error = error + 'There was a database problem updating the file named %s' % (query.Value(item+1))
				commit_list = commit_list + commit_line % (query.Value(item+1),query.Value(item+2),query.Value(item+3),query.Value(item+4),query.Value(item+5),query.Value(item+6))

		commitf = commit_form % (error,commit_list)
		html = html_form % ('',commitf,validate_button)

	if (query.Get('update')):
		query_size = query.Size()
		for num in range(query_size):
			if (query.Key(num)=='selected'):
				select_list = select_list + (query.Value(num)) + ','
		search = update_sql + where_sql % (select_list[:-1])
		uresults = db.Select(search)
		while db.GetRow(uresults) == Ns.OK:
			update_list = update_list + update_line % (uresults[0],uresults[1],uresults[1],uresults[2],uresults[3],uresults[4],uresults[5],uresults[6],uresults[7])
			#sql_list = sql_list + (uresults[1]) + ','
		form = update_form % (update_list,update_button)
		#sql_list = sql_list + '1'
		
		html = html_form % ('',form,'')
	
	if (query.Get('invalid')):
		query_size = query.Size()
		for num in range(query_size):
			if (query.Key(num)=='selected'):
				select_list = select_list + (query.Value(num)) + ','
		search = invalidate_sql + (where_sql % (select_list[:-1]))
		iresults = db.Select(search)
		while db.GetRow(iresults) == Ns.OK:
			validate_list = validate_list + validate_line % (iresults[0],iresults[1],iresults[2],iresults[3],iresults[4])
			sql_list = sql_list + (iresults[0]) + ','
		form = validate_form % ('invalidated',validate_list)
		#sql_list = sql_list + '1'
		db.DML(invalidate_changes_sql + where_sql % (sql_list[:-1]))
		html = html_form % ('',form,validate_button)
	
	if (query.Get('validate')):
		search = validate_sql
		vresults = db.Select(search)
		while db.GetRow(vresults) == Ns.OK:
			validate_list = validate_list + validate_line % (vresults[0],vresults[1],vresults[2],vresults[3],vresults[4])
			sql_list = sql_list + (vresults[0]) + ','
		form = validate_form % ('validated',validate_list)
		#sql_list = sql_list + '1'
		html = html_form % ('',form,validate_button)
		db.DML(validate_changes_sql + where_sql % (sql_list[:-1]))
else:
	conn.ReturnRedirect('http://10.11.0.129/project/admin/admin2.py')

del db
conn.ReturnHtml(200,html)

