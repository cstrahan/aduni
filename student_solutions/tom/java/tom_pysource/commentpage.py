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
comment_query_string = """
SELECT DISTINCT
  teams.team_id,
  schools.school_name, 
  teams.team_name, 
  content.title,
  content.message,
  content.date_posted
FROM  content, teams, schools 
WHERE content.team_id = teams.team_id 
AND teams.school_id = schools.school_id 
AND content.content_type = 'comment'
AND teams.team_id = '%s'
ORDER BY content.date_posted DESC""" % team_var
###the schoolpage query will be the same minus the team_name constraint

try:
     db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
     conn.ReturnRedirect('error_page')

content =  rowify(db, comment_query_string)
team_name_query = 'SELECT schools.school_name, teams.team_name FROM schools, teams WHERE team_id = %s' % team_var
team_select = db.Select(team_name_query)
if (db.GetRow(team_select) == Ns.OK):
     team_name = team_select['school_name'] + " " + team_select['team_name']
team_comment_info =  """
<tr>
            <th colspan=4 valign=bottom><h3>Team Page for """ + team_name + """ </h3></th></tr>
    <tr>
            <th width=30\%>
            Team
            </th>
            <Th width=60\%>
            Comment
            </Th>
            <Th width=5\%>
            Posted on
            </th>
    </tr>""" + content

###this is debugging stuff, uncomment to use
#Ns.Log(Ns.Notice,"------------- for query string:\n%s\n---------------------------team info: \n%s" % (team_query_string,team_caselist_info))

###todo:
#team_comments = sidebarify(db, comment_string)

###here's the page itself

html =  displayify(team_comment_info, db)

###close up the connection in case it's hanging around.  This should be unnecessary.
del db

###here's the web page that gets generated.
conn.ReturnHtml(200, html)































