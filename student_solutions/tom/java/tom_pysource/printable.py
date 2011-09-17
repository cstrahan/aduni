####################################################################################################################
### printable.py
### Bryon Gill 4/29/01
### This page generates the entire caselist, aff then neg, in a printer friendly format.

import Ns, string
from rowify import printify
from displayify import displayify

###This is necessary aolserver overhead
conn = Ns.GetConn()


###query to get all aff cases:
whole_aff_caselist_query_string = """
SELECT DISTINCT
     teams.team_id,
     schools.school_name,
     teams.team_name,
     content.list_id,
     content.title,
     content.message,
     content.date_posted
FROM
     content, 
     teams,
     schools 
WHERE content.team_id = teams.team_id
AND teams.school_id = schools.school_id
AND content.content_type = 'caselist' 
AND content.side = 'affirmative'
AND content.status != 'not approved'     
ORDER BY schools.school_name, teams.team_name, content.title, content.date_posted"""


whole_neg_caselist_query_string = """
SELECT DISTINCT
     teams.team_id,
     schools.school_name,
     teams.team_name,
     content.list_id,
     content.title,
     content.message,
     content.date_posted
FROM
     content, 
     teams,
     schools 
WHERE content.team_id = teams.team_id
AND teams.school_id = schools.school_id
AND content.content_type = 'caselist' 
AND content.side = 'negative'
AND content.status != 'not approved'     
ORDER BY schools.school_name, teams.team_name, content.title, content.date_posted"""

try:
     db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
     conn.ReturnRedirect('/project/error_page.html')


whole_aff_list = printify(db, whole_aff_caselist_query_string)
whole_neg_list = printify(db, whole_neg_caselist_query_string)

content = '''
<html>
<body bgcolor="white">
<table border="1" align="center" width="90%">
<tr>
            <th colspan=4 valign=bottom align=left><h3>secondarysources.com Affirmative Caselist</h3></th></tr>
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
    </tr>''' + whole_aff_list +  '''
    <tr>
            <th colspan=4 valign=bottom align=left><h3>secondarysources.com Negative Argument List</h3></th></tr>
    </tr>''' + whole_neg_list + '''
</table>
</html>'''

conn.ReturnHtml(200, content)



















