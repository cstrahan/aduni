import Ns, os, sys
import string, re

#################person-admin.py, by Tom Hickerson, 4/19/01
### Administrator update/validate/invalidate for the persons database.
###
###

conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

#### create blocks of html code, sql commands

html_form = '''<html><head><title>
Administrating PERSONS Database</head></title>
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
<tr><td>Id #</td><td>Name</td><td>Email</td><td>School</td></tr>
%s
</table>
'''

validate_line = '''<tr>
<td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>
</tr>'''

validate_list = ''

validate_button = '''<form>
<input type=button value="Back to Admin" onclick="admin2.py">
</form>'''

update_form = '''You asked to update the following:
<form action="person-admin.py">
%s
%s
</form>
'''

update_line = '''<table>
<tr><td>Date Created:%s</td></tr>

<tr><td>ID #:%s</td>
<td>First Name:<input type=text size=20 name=first_names value="%s"></td>
<td>Access<select name=access_level>
<option value="basic" selected>Basic</option>
<option value="admin">Admin</option></select>
</td></tr>

<tr><td></td>
<td>Last Name:<input type=text size=20 name=last_name value="%s"></td>
<td>Status<select name=status>
<option value="approved" selected>Approved</option>
<option value="not approved">Not Approved</option>
<option value="not reviewed">Not Reviewed</option>
</select>
</td></tr>

<tr><td></td>
<td>Url:<input type=text size=20 name=url value="%s"></td>
<td>Email:<input type=text size=20 name=email value="%s"></td></tr>

<tr><td></td>
<td>Street:<input type=text size=20 name=street_address value="%s"></td>
<td></td></tr>

<tr><td></td>
<td>City:<input type=text size=16 name=city value="%s"> 
State:<input type=text size=4 name=state value="%s"> 
Zipcode: <input type=text size=10 name=zipcode value="%s"> </td></tr>

<tr><td></td>
<td>Office Phone:<input type=text size=16 name=office_phone value="%s"> 
Mobile:<input type=text size=16 name=mobile_phone value="%s"> 
Fax: <input type=text size=16 name=fax value="%s"> </td></tr>
</table>
<hr>
'''

update_list = ''

update_button = '''<input type=submit value="Submit all changes">'''

### rationale for the three esses: message, form, buttons

### sql strings: we'll need one for getting all non-reviewed persons so as to
### change them to reviewed

validate_sql = 'SELECT p.person_id,p.first_names,p.last_name,p.email,s.school_name FROM persons p, person_school_map psm, schools s WHERE p.person_id = psm.person_id and psm.school_id = s.school_id and p.status = \'not reviewed\';'

invalidate_sql = 'SELECT p.person_id,p.first_names,p.last_name,p.email,s.school_name FROM persons p, person_school_map psm, schools s WHERE p.person_id = psm.person_id and psm.school_id = s.school_id'

inv_where_sql = ' and p.person_id = (%s)'

### old update sql string: will we allow schools to be edited from this page???
#update_sql = 'SELECT p.date_registered,p.person_id,p.first_names,p.access_level,p.last_name,p.status,p.url,p.email,p.street_address,p.city,p.state,p.zipcode,p.office_phone,p.mobile_phone,p.fax FROM persons p, person_school_map psm, schools s WHERE p.person_id = psm.person_id and psm.school_id = s.school_id and p.person_id = (%s);'

update_sql = 'SELECT date_registered,person_id,first_names,access_level,last_name,status,url,email,street_address,city,state,zipcode,office_phone,mobile_phone,fax FROM persons'

update_changes_sql = 'UPDATE persons SET first_names = \'%s\',access_level = \'%s\',last_name=\'%s\',status=%s,url==\'%s\',email==\'%s\',street_address==\'%s\',city==\'%s\',state==\'%s\',zipcode==\'%s\',office_phone=\'%s\',mobile_phone=\'%s\',fax=\'%s\''

validate_changes_sql = 'UPDATE persons SET status = "approved"'

invalidate_changes_sql = 'UPDATE persons SET status = "not approved"'

where_sql = ' WHERE person_id = (%s)'

### get form data and start sql processes
html = ''
form = ''
query_exists = 0
submit_type = 0

try:
	query = conn.GetQuery()
	query_exists = 1
except RunTimeError:
	query_exists = 0

if (query_exists == 1):
	if (query.Get('update')=="Update+Selected"):
		html = html_form % ('','','')
	if (query.Get('invalid')=="Invalidate+Selected"):
		html = html_form % ('','','')
	if (query.Get('validate')=="Accept+All"):
		search = validate_sql
		vresults = db.Select(search)
		while db.GetRow(vresults) == Ns.OK:
			validate_list = validate_list + validate_line % (vresults[0],vresults[1],vresults[2],vresults[3],vresults[4])
		form = validate_form % ('validated',validate_list)
		html = html_form % ('',validate_form,validate_button)
else:
	conn.ReturnRedirect(200,admin2.py)

conn.ReturnHtml(200,html)
