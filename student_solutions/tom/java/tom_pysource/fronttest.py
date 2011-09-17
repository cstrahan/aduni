####################################################################################################################
### front.py
### Bryon Gill 4/22/01

import Ns, string
from rowify import rowify
from displayify import displayify

###This is necessary aolserver overhead
conn = Ns.GetConn()

###here are the strings for the queries we'll need to perform:
update_query_string = """
SELECT DISTINCT 
     schools.school_name, 
     teams.team_name, 
     content.title, 
     content.date_posted 
FROM 
     content, 
     teams,
     schools 
WHERE
     content.team_id = teams.team_id AND
     teams.school_id = schools.school_id AND
     content.content_type = 'caselist' 
ORDER BY content.date_posted DESC
LIMIT 3"""


whole_caselist_query_string = """
SELECT DISTINCT 
     schools.school_name,
     teams.team_name, 
     content.title, 
     content.date_posted 
FROM
     content, 
     teams,
     schools 
WHERE content.team_id = teams.team_id
AND teams.school_id = schools.school_id
AND content.content_type = 'caselist' 
ORDER BY
     schools.school_name,
     teams.team_name,
     content.title,
     content.date_posted"""

try:
     db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
     conn.ReturnRedirect('error_page')


#try:
#        whole_list = " "
#        results = db.Select(whole_caselist_query_string)
#        while (db.GetRow(results) == Ns.OK):
#                whole_list = whole_list + results['school_name']
#except:
#        conn.ReturnRedirect('error_page')




whole_list = rowify(db, whole_caselist_query_string)
most_recently_updated = rowify(db, update_query_string)


    ### TODO:
    ###     -keep the school in a TD that spans all the appropriate team columns
    ###     -also, make sure updates from last seven days are displayed in red

content = """
<tr>
            <th colspan=4 valign=bottom><h3>Most recent submissions</h3> <a href=/project/recent.py>(click here for more)</a></th></tr>
    <tr>
            <th width=30\%>
            Team
            </th>
            <Th width=60\%>
            Case
            </Th>
            <Th width=5\%>
            Updated on
            </th>
    </tr>""" + most_recently_updated +  '''
    <tr>
            <th colspan=4 valign="bottom">
            <h3>Full Caselist</h3>
            </th>
    </tr>''' + whole_list



html = displayify(content)

###close up the connection
del db

###here's the web page that gets generated.
conn.ReturnHtml(200, html)































