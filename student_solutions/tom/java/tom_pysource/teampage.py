####################################################################################################################
### teampage.py
### Bryon Gill 4/24/01

import Ns
from rowify import rowify 
from displayify import displayify

###This is necessary aolserver overhead
conn = Ns.GetConn()
query = conn.GetQuery()
team_var = query.Get('team_id')

###here are the strings for the queries we'll need to perform:
team_aff_query_string = """
SELECT DISTINCT
  teams.team_id,
  schools.school_name, 
  teams.team_name,
  content.list_id,
  content.title,
  content.message,
  content.date_posted,
  persons.email
FROM  content, teams, schools, persons
WHERE content.team_id = teams.team_id 
AND teams.school_id = schools.school_id 
AND content.content_type = 'caselist'
AND teams.team_id = '%s'
AND content.person_id = persons.person_id
AND content.status != 'not approved'
AND content.side = 'affirmative'
ORDER BY content.date_posted DESC""" % team_var

team_neg_query_string = """
SELECT DISTINCT
  teams.team_id,
  schools.school_name, 
  teams.team_name,
  content.list_id,
  content.title,
  content.message,
  content.date_posted,
  persons.email
FROM  content, teams, schools, persons
WHERE content.team_id = teams.team_id 
AND teams.school_id = schools.school_id 
AND content.content_type = 'caselist'
AND teams.team_id = '%s'
AND content.person_id = persons.person_id
AND content.status != 'not approved'
AND content.side = 'negative'
ORDER BY content.date_posted DESC""" % team_var

comment_query_string =  """
SELECT DISTINCT
  persons.email,
  content.list_id,
  content.title,
  content.message,
  content.date_posted,
  teams.team_id,
  teams.team_name,
  schools.school_name
FROM  content, teams, schools 
WHERE content.team_id = teams.team_id 
AND teams.school_id = schools.school_id 
AND content.content_type = 'comment'
AND teams.team_id = '%s'
AND persons.person_id = content.person_id
AND content.status != 'not approved'
ORDER BY content.date_posted DESC""" % team_var

###the schoolpage query will be the same minus the team_name constraint
###(if I ever implement it)
try:
     db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
     conn.ReturnRedirect('/project/front.py')
###take the query strings and hand them off to rowify to fill in the tables
this_team_cases =  rowify(db, team_aff_query_string)
this_team_neg = rowify(db, team_neg_query_string)
this_team_comments = rowify(db, comment_query_string)

###get the name of the team in question
team_name_query = 'SELECT schools.school_name, teams.team_name FROM schools, teams WHERE team_id = %s AND schools.school_id = teams.school_id' % team_var
team_select = db.Select(team_name_query)
if (db.GetRow(team_select) == Ns.OK):
     team_name = team_select['school_name'] + " " + team_select['team_name']

###here's the content that gets handed off to displayify
team_caselist_info =  '''
<tr>
            <th colspan=4 valign=bottom><h3>Team Page for ''' + team_name + ''' </h3></th></tr>
    <tr>
            <th width=30\%>
            Team
            </th>
            <Th width=60\% cols="80">
            Affirmative Case Info
            </Th>
            <Th width=5\%>
            Updated on
            </th>
    </tr>''' + this_team_cases + '''
    <tr>
            <th width=30\%>
            Team
            </th>
            <Th width=60\%>
            Negative Argument Info
            </Th>
            <Th width=5\%>
            Updated on
            </th>
    </tr>''' + this_team_neg + '''
    
    <tr>
            <th width=30\%>
            <p>---<p>
            </th>
            <Th width=60\%>
            <p>Comments<p><p>
            </Th>
            <Th width=5\%>
            <p>---<p>
            </th>
    </tr>
  ''' + this_team_comments 

###this is debugging stuff, uncomment to use
#Ns.Log(Ns.Notice,"------------- for query string:\n%s\n---------------------------team info: \n%s" % (team_query_string,team_caselist_info))


###here's the call to displayify which returns the html
html =  displayify(team_caselist_info, db)

###close up the connection in case it's hanging around.  This should be unnecessary.
del db

###here's the aolserver command to send the page to the browser
conn.ReturnHtml(200, html)































