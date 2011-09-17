import Ns
import string
import sys, os
import zz

############Admin.py v1.1 by Tom Hickerson, 4/18/01
#### serves the form for the administrator to show data from tables
#### note: programming style differs from Blake's, whereas Blake 
#### constructs the whole page in one quote, I serve it in chunks,
#### which I just use print in the place of Ns.ServeHtml(200...)
#### Not sure which is better.

#################Admin2.py v1.2 Tom Hickerson 4/19/01
### rewrite of code to accept new changes in the data model, trying to get
### most of the html in one chunk and using the Ns module exclusively
### now that I've rewritten it, the file looks a little long.  More
### than likely, we split the admin functionality up into different
### files:
### admin2.py -- this file, display and serve different progs from here
### person_admin.py -- invalidate/update user info
### admin_search.py -- db search function
### posting_admin.py -- invalidate/update posting info
### school_admin.py -- invalidate/update school info
### team_admin.py -- invalidate/update team info
### whew!  maybe I'll try to combine a few of these --tbh

########begin the connection and set up sql strings here

conn = Ns.GetConn()



persons_sql = '''
SELECT 
	p.person_id,
	p.first_names,
	p.last_name,
	p.email,
	s.school_name 
FROM 
	persons p, 
	person_school_map psm, 
	schools s 
WHERE 
	p.person_id = psm.person_id 
	and psm.school_id = s.school_id 
	and p.status = \'not reviewed\';'''

### union all has been taken out because of repeats, put it back in??
##UNION ALL
#SELECT
	#person_id,
	#first_names,
	#last_name,
	#email,
	#state
#FROM
	#persons
#WHERE
	#status = \'not reviewed\';'''

### simple_persons_sql was set up as a quick fix, need to remove later

simple_persons_sql = '''
SELECT
	person_id,
	first_names,
	last_name,
	email	
FROM 	persons
WHERE	status = \'not reviewed\';'''

postings_sql = '''
SELECT 
	c.list_id, 
	c.title, 
	t.team_name, 
	p.first_names, 
	p.last_name 
FROM 
	content c, 
	teams t, 
	persons p 
WHERE 
	c.team_id = t.team_id 
	and c.person_id = p.person_id 
	and c.status = \'not reviewed\';'''

schools_sql = '''
SELECT 
	school_id, 
	school_name, 
	ndt_district, 
	ceda_region, 
	ada_member,
	school_password 
FROM 
	schools 
WHERE status=\'not reviewed\';'''

teams_sql = '''
SELECT 
	t.team_id, 
	t.team_name, 
	t.date_created, 
	s.school_name 
from teams t, schools s 
WHERE t.school_id = s.school_id and t.status=\'not reviewed\';'''

########setup several dummy values, error, db. etc.

error = ''

html_users_list = ''
html_posts_list = ''
html_schools_list = ''
html_teams_list = ''


##############set up all the chunks of html code here

###header = 'Content-type: text/html\n'

html_admin = '''<html><body bgcolor=white>
<h3>Administration Page</h3>
<a href="http://10.11.0.129/project/registration/school_registration.py">
Create a new school</a> | <a href="http://10.11.0.129/project/registration/team_registration.py">Create a new team</a> | <a href="http://10.11.0.129/project/admin/admin_logout.py">Logout to Home Page</a>
<!--Links to change cookies, search form, etc.-->
<form action=admin-search.py>
<input type=text name=ad_search size=25>
<input type=submit value="Search">
<select name=dbase>
<option value=persons>Persons
<option value=schools>Schools
<option value=teams>Teams
<option value=content>Content
</select>
</form>
<hr>
New Users -- %s new users have signed up:<br>
<form action=person_admin.py>

<table><tr>
<td></td>
<td>First names</td>
<td>Last name</td>
<td>Email</td>
<td>School</td></tr>
%s
</table>
<input type=submit name=update value="Update Selected">
<input type=submit name=invalid value="Invalidate Selected">
<input type=submit name=validate value="Accept All">

<!--<input type=submit name=accept value="Accept Selected">
<input type=submit name=invalid value="Invalidate Selected">
<input type=submit name=acceptall value="Accept All">-->

</form>
<hr>
New Postings--%s new posts have been created:<br>
<form action=posting_admin.py>

<table>
<tr>
<td></td>
<td>Subject</td>
<td>To group</td>
<td>Posted by</td>
</tr>%s
</table>

<input type=submit name=update value="Update Selected">
<input type=submit name=invalid value="Invalidate Selected">
<input type=submit name=validate value="Accept All">

<!--<input type=submit name=accept_post value="Accept Selected">
<input type=submit name=delete_post value="Hide Selected">
<input type=submit name=acceptall_post value="Accept All">-->

</form>
<hr>
New Schools--%s new schools have been signed up:<br>
<form action=school_admin.py>

<table>
<tr>
<td></td>
<td>School</td>
<td>NDT Dist.</td>
<td>CEDA Reg.</td>
<td>ADA Mem.</td><td>Password</td>
</tr>%s
</table>

<input type=submit name=update value="Update School">
<input type=submit name=invalid value="Invalidate School">
<input type=submit name=validate value="Accept All">

</form>
<hr>
New Teams--%s new teams have signed up:<br>
<form action=team_admin.py>

<table>
<tr>
<td></td>
<td>Team</td>
<td>Date created</td>
<td>School</td>
</tr>%s
</table>

<input type=submit name=update value="Update Team">
<input type=submit name=invalid value="Invalidate Team">
<input type=submit name=validate value="Accept All">
</form>
<hr>
Other links and announcements will be placed here.
</body></html>
'''

html_users = '''<tr bgcolor=%s>
<td><input type=checkbox value="%s" name=selected></td>
<td>%s</td>
<td>%s</td>
<td>%s</td><td>%s</td>
</tr>'''

html_posts = '''<tr bgcolor=%s>
<td><input type=checkbox value="%s" name=selected></td>
<td>%s</td>
<td>%s</td>
<td>%s</td><td>%s</td>
</tr>'''

html_schools = '''<tr bgcolor=%s>
<td><input type=checkbox value="%s" name=selected></td>
<td>%s</td>
<td>%s</td>
<td>%s</td><td>%s</td>
<td>%s</td>
</tr>'''

html_teams = '''<tr bgcolor=%s>
<td><input type=checkbox value="%s" name=selected></td>
<td>%s</td>
<td>%s</td>
<td>%s</td>
</tr>'''

### check to see if user is a valid admin; if not, redirect

status_sql = '''
SELECT access_level 
FROM persons 
WHERE person_id = %s;'''

cookie = zz.get_cookie('bosphorous')

#db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
if (cookie):
	pass
	#search = status_sql % (cookie)
	#admin_res = db.Select(search)

	#while (db.GetRow(admin_res)==Ns.OK):
		#if (admin_res[0]=='basic'):
			#conn.ReturnRedirect('http://10.11.0.129/project/front.py')
else:
        
	conn.ReturnRedirect('http://10.11.0.129/project/admin/admin_login.py')
	
########begin to fetch info through sql Selects
p_count = 0
m_count = 0
sch_count = 0
t_count = 0
bg_color = 'white'
bg_color2 = 'EEFFFA'

#### procedure to try and not use so much repetitive code: doesn't quite work yet,
#### bg_color is still handled in try loops for now

def switch(color):
	if (color=='white'):
		color = 'silver'
	else:
		color = 'white'
	return color

### try loops to construct tables for the admin page
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

try:
	person_res = db.Select(persons_sql)
	while db.GetRow(person_res) == Ns.OK:
		p_count = p_count + 1
		html_users_list = html_users_list + html_users % (bg_color,person_res[0],person_res[1],person_res[2],person_res[3],person_res[4])
		if (bg_color=='white'):
			bg_color = bg_color2
		else:
			bg_color = 'white'
		#switch(bg_color)
except:
	error = 'Unable to get user info'

try:
	posting_res = db.Select(postings_sql)
	while db.GetRow(posting_res) == Ns.OK:
		m_count = m_count + 1
		html_posts_list = html_posts_list + html_posts % (bg_color,posting_res[0],posting_res[1],posting_res[2],posting_res[3],posting_res[4])
		if (bg_color=='white'):
			bg_color = bg_color2
		else:
			bg_color = 'white'
except:
	error = 'Unable to get posts info'

try:
	school_res = db.Select(schools_sql)
	while db.GetRow(school_res) == Ns.OK:
		sch_count = sch_count + 1
		html_schools_list = html_schools_list + html_schools % (bg_color,school_res[0],school_res[1],school_res[2],school_res[3],school_res[4],school_res[5])
		if (bg_color=='white'):
			bg_color = bg_color2
		else:
			bg_color = 'white'
except:
	error = 'Unable to get school info'

try:
	team_res = db.Select(teams_sql)
	while db.GetRow (team_res) == Ns.OK:
		t_count = t_count + 1
		html_teams_list = html_teams_list + html_teams % (bg_color,team_res[0],team_res[1],team_res[2],team_res[3])
		if (bg_color=='white'):
			bg_color = bg_color2
		else:
			bg_color = 'white'
except:
	error = 'Unable to get teams info'



#################begin the actual html page


del db
html = html_admin % (str(p_count),html_users_list,str(m_count),html_posts_list,str(sch_count),html_schools_list,str(t_count),html_teams_list)
conn.ReturnHtml(200, html)









