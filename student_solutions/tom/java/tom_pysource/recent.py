####################################################################################################################
### front.py
### Bryon Gill 4/22/01

import Ns
from rowify import rowify
from displayify import displayify

###This is necessary aolserver overhead
conn = Ns.GetConn()


###here are the strings for the queries we'll need to perform:
all_updates_query_string = """SELECT DISTINCT 
teams.team_id,
schools.school_name, 
teams.team_name,
content.list_id,
content.title, 
content.date_posted
FROM 
content, 
teams,
schools 
WHERE
content.team_id = teams.team_id 
AND 
teams.school_id = schools.school_id 
AND 
content.content_type = 'caselist'
AND
content.date_posted > now() - 7
ORDER BY
content.date_posted DESC"""


try:
     db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
     conn.ReturnRedirect('error_page')

most_recently_updated = rowify(db, all_updates_query_string)

    ### TODO:
    ###     -keep the school in a TD that spans all the appropriate team *rows*
    ###     -also, make sure updates from last seven days are displayed in red

html = displayify(most_recently_updated, db)

## '''<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">
##         <html> <head>
##         <title>Secondary Sources Caselist - Updates from last 7 days</title>
##         </head>
        
##         <body bgcolor=white>
        
##         <table width=100\% align="center">
        
##         <tr>
##                 <td width=10\% rowspan=10 valign=top>
##                 Someday,
##                 <p>
##                 There will be nothing but links
##                 <p>
##                 As far as the eye can see.
##                 <p>
##                 </td>

##                 <td width=80\% BGCOLOR=gray>
##                 <h1><center>Secondary Sources Caselist - Updates from last 7 days</center></h1>
##                 </td>
##                 <td width=10\% rowspan=10 valign=top>

##                 Post new caselist information<p>
##                 Change user preferences<p>
##                 <a href=http://debate.uvm.edu>Debate Central</a><p>

##                 </td>        
##         </tr>        
##         <tr align=left valign=top>        
##                 <td width=60\%>
##                 <table border>
##                         <tr>
##                                 <th colspan=4 valign=bottom><h3>Most recent submissions</h3> <a href=http://www.slashdot.org>(click for more...)</a></th></tr>
##                         <tr>
##                                 <th width=30\%>
##                                 Team
##                                 </th>
##                                 <Th width=60\%>
##                                 Case
##                                 </Th>
##                                 <Th width=5\%>
##                                 Updated on
##                                 </th>
##                         </tr>
##                                 ''' + most_recently_updated +  '''
##                 </table>
##                         </td>           
##                 </tr>
##                 <tr>
##                         <td></td>
##                         <td></td>
##                         <td></td>
##                         <td></td>
##                         <td></td>
##                 </tr>        
##         </table>
##         <hr>
##         <address></address>
##         </body> </html>'''

## ###close up the connection in case it's hanging around.  This should be unnecessary.
## del db

###here's the web page that gets generated.
conn.ReturnHtml(200, html)































