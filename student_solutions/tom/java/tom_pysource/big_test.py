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
     teams.team_id,
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
     teams.team_id,
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


whole_list = rowify(db, whole_caselist_query_string)
most_recently_updated = rowify(db, update_query_string)


content = """
<tr>
            <th colspan=4 valign=bottom align=left><h3><a href=/project/recent.py>Most recent submissions</a></h3></th></tr>
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
            <th colspan=4 valign="bottom" align=left>
            <h3>Full Caselist</h3>
            </th>
    </tr>''' + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list + whole_list 



html = displayify(content, conn)

conn.ReturnHtml(200, html)


    ### TODO:
    ###     -keep the school in a TD that spans all the appropriate team columns
    ###     -also, make sure updates from last seven days are displayed in red





























