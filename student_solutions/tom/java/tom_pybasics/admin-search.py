import Ns
import os, sys, string, re


####################admin-search.py, by Tom Hickerson 4/19/01
### search designated databases, offer to search again, offer to update/invalidate
### selected records, using the following programs: person_admin, posting_admin,
### school_admin, team_admin, referring back to admin2.py as well.


conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

###set html code, sql strings, other dummy variables
###define sql strings###

person_sql = 'SELECT person_id,email,first_names,last_name from persons where upper(email) like upper(\'%%%s%%\') or upper(first_names) like upper(\'%%%s%%\') or upper(last_name) like upper(\'%%%s%%\');'

school_sql = 'SELECT school_id,school_name,ndt_district,ceda_region,ada_member from schools where upper(school_name) like upper(\'%%%s%%\');'

team_sql = 'SELECT team_id,team_name,member_one_first_name,member_one_last_name,member_two_first_name,member_two_last_name from teams where ((upper(team_name) like upper(\'%%%s%%\')) or (upper(member_one_first_name) like upper(\'%%%s%%\')) or (upper(member_one_last_name) like upper(\'%%%s%%\')) or (upper(member_two_first_name) like upper(\'%%%s%%\')) or (upper(member_two_last_name) like upper(\'%%%s%%\')));'

content_sql = 'SELECT list_id,content_type,title,date_posted,team_id,posted_by from content where ((upper(title) like upper(\'%%%s%%\')) or (upper(message) like upper(\'%%%s%%\')));'

html_person_start = '''<tr>
<td></td>
<td>Email</td>
<td>First Names</td>
<td>Last Name</td>
</tr>'''

html_school_start = '''<tr>
<td></td>
<td>Name</td>
<td>District</td>
<td>Region</td>
<td>ADA</td>
</tr>'''

html_team_start = '''<tr>
<td></td>
<td>Team Name</td>
<td>Team Member</td><td>Team Member</td>
</tr>'''

html_content_start ='''<tr>
<td></td>
<td>Type</td>
<td>Title</td>
<td>Date</td>
<td>Team</td>
<td>Poster</td>
</tr>'''

html_person = '''<tr>
<td><input type=checkbox value="%s" name=selected></td>
<td>%s</td>
<td>%s</td>
<td>%s</td>
</tr>'''

html_school = '''<tr>
<td><input type=checkbox value="%s" name=selected></td>
<td>%s</td>
<td>%s</td>
<td>%s</td>
<td>%s</td>
</tr>'''

html_team = '''<tr>
<td><input type=checkbox value="%s" name=selected></td>
<td>%s</td>
<td>%s</td><td>%s</td><td>%s</td>
<td>%s</td>
</tr>'''

html_content = '''<tr>
<td><input type=checkbox value="%s" name=selected></td>
<td>%s</td>
<td>%s</td>
<td>%s</td>
<td>%s</td>
<td>%s</td>
</tr>'''

### html_form, the wrapper for the rest of the queries

html_form = '''<html><body bgcolor=white>
<h3>Administration Page</h3>
%s
<form action=admin-search.py>
<input type=text name=ad_search size=25>
<input type=submit value="Search again">
<select name=dbase>
<option value=persons>Persons
<option value=schools>Schools
<option value=teams>Teams
<option value=content>Content
</select>
</form>
<form action=%s>
<input type=submit name=update value="Update Selected">
<input type=submit name=invalid value="Invalidate Selected">
<table>
%s
</table>
</form>
<br>
<hr></body></html>'''

###get form data###

query_exists = 0
search = ''
html_list = ''
html = ''
tstring = '<h3>Results for : %s</h3>'

try:
	query = conn.GetQuery()
	query_exists = 1
except RuntimeError:
	query_exists = 0

if (query_exists == 1):
	db_val = query.Get('dbase')
	db_string = query.Get('ad_search')
	tstr = tstring % (db_string)

	if db_val == 'persons':
		search = person_sql % (db_string,db_string,db_string)
		presults = db.Select(search)
		html_list = html_person_start
		while db.GetRow(presults) == Ns.OK:
			html_list = html_list + (html_person % (presults[0],presults[1],presults[2],presults[3]))
		html = html_form % (tstr,'person_admin.py',html_list)

	elif db_val == 'schools':
		search = school_sql % (db_string)
		sresults = db.Select(search)
		html_list = html_school_start
		while db.GetRow(sresults) == Ns.OK:
			html_list = html_list + (html_school % (sresults[0],sresults[1],sresults[2],sresults[3],sresults[4]))
		html = html_form % (tstr,'school_admin.py',html_list)

	elif db_val == 'teams':
		search = team_sql % (db_string,db_string,db_string,db_string,db_string)
		tresults = db.Select(search)
		html_list = html_team_start
		while db.GetRow(tresults) == Ns.OK:
			html_list = html_list + (html_team % (tresults[0],tresults[1],tresults[2],tresults[3],tresults[4],tresults[5]))
		html = html_form % (tstr,'team_admin.py',html_list)

	elif db_val == 'content':
		search = content_sql % (db_string,db_string)
		cresults = db.Select(search)
		html_list = html_content_start
		while db.GetRow(cresults) == Ns.OK:
			html_list = html_list + (html_content % (cresults[0],cresults[1],cresults[2],cresults[3],cresults[4],cresults[5]))
		html = html_form % (tstr,'posting_admin.py',html_list)

else:
	html = html_form % ('Please enter a search string','admin_search.py','')

conn.ReturnHtml(200,html)











