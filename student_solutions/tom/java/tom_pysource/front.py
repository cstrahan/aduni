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
     teams.team_name,
     schools.school_name, 
     content.list_id,
     content.title, 
     content.date_posted
FROM 
     content, teams, schools, persons
WHERE
     content.team_id = teams.team_id AND
     teams.school_id = schools.school_id AND
     content.content_type = 'caselist' AND
     content.status != 'not approved'     
ORDER BY content.date_posted DESC
LIMIT 3"""



# (zb) convert this into a view
whole_caselist_query_string = """
SELECT DISTINCT
     teams.team_id,
     schools.school_name,
     teams.team_name,
     content.list_id,
     content.title,
     content.date_posted,
     UPPER(schools.school_name),
     UPPER(teams.team_name),
     UPPER(content.title)
FROM
     content, 
     teams,
     schools 
WHERE content.team_id = teams.team_id
AND teams.school_id = schools.school_id
AND content.content_type = 'caselist' 
AND content.side = 'affirmative'
AND content.status != 'not approved'     
ORDER BY
UPPER(schools.school_name), UPPER(teams.team_name), UPPER(content.title), content.date_posted
"""

##      CASE
##           WHEN content.date_posted > content.date_posted
##           THEN 'new'
##           ELSE 'old'
##      END
## FROM

try:
     db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
     conn.ReturnRedirect('/project/error_page.html')


whole_list = rowify(db, whole_caselist_query_string)
most_recently_updated = rowify(db, update_query_string)


content = """
<tr>
            <th colspan=4 valign=bottom align=left><h3>Most Recent Submissions <a href=/project/recent.py><font size=1>(last 7 days)</font></a></h3></th></tr>
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
            <h3>Full Caselist <font size=1>(printer-friendly:
            <a href=/project/print-opts.py?display=aff>aff only|</a>
            <a href=/project/print-opts.py?display=neg>neg only|</a>
            <a href=/project/print-opts.py?display=both>both</a>)</font></a></h3>
            </th>
    </tr>''' + whole_list

html = displayify(content, db)

conn.ReturnHtml(200, html)


    ### TODO:
    ###     -keep the school in a TD that spans all the appropriate team columns
    ###     -also, make sure updates from last seven days are displayed in red



































