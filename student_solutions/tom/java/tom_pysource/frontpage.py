from mod_python import apache
from pg import DB
from mod_python import util

###instantiate a connection to the database
conn = DB('test')


### here's the function to generate the table
### it returns a string with the caselist entries broken into rows.
def rowify(listinfo):
        caselist = ""
        for each_row in listinfo:
            caselist = caselist + """<TR><TD align=center>""" + each_row[0] + """ """ + each_row[1] +   """</TD><TD align=center>""" + each_row[2] + """</TD><TD align=center>""" + each_row[3][5:10] + """</TD></TR>"""
        return caselist
###TODO FOR ROWIFY:
### -colorize the rows, alternating for easy reading- build this into the rowify function?
### -make the team names into links to team pages
### -test the dates, and print the ones that are less than 7 days old in red
### -strip all but the month and day from the timestamp


###here go the queries!
def update_query(connection):
    ### get the 3 most current rows
    recent_updates = connection.query("""SELECT DISTINCT 
    schools.school_name, 
    teams.team_name, 
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
    ORDER BY
    content.date_posted
    DESC
    LIMIT 	
    3; 
    """).getresult()
    return recent_updates

def caselist_query(connection):
    whole_caselist = connection.query("""SELECT DISTINCT 
    schools.school_name, 
    teams.team_name, 
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
    ORDER BY
    schools.school_name,
    teams.team_name,
    content.title,
    content.date_posted
    """).getresult()
    return whole_caselist

    ### TODO:
    ###     keep the school in a TD that spans all the
    ###     appropriateteam columns
    ### also, make sure updates from last seven days are displayed in red




most_recently_updated = rowify(update_query(conn))
whole_list = rowify(caselist_query(conn))
pagestring = '''<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">
        <html> <head>
        <title>Secondary Sources Caselist</title>
        </head>
        
        <body bgcolor=white>
        
        <table width=100\% align="center">
        
        <tr>
        <td width=10\% rowspan=10 valign=top>
        link1 and some more stuff
        <p>
        link2
        <p>
        link3
        <p>
        </td>
        
        <td width=80\% BGCOLOR=gray>
        <h1><center>Secondary Sources Caselist</center></h1>
        </td>
        <td width=10\% rowspan=10 valign=top>
        
        link1 to stuff from outside<p>
        link2 to stuff from outside<p>
        link3 to stuff from really, really far outside<p>
        </td>
        
        </tr>
        
        <tr align=left valign=top>
        
        <td width=60\%>
        <table border>
        <tr>
        <th colspan=4 valign=bottom><h3>Most recent submissions</h3> <a href=http://www.slashdot.org>(click for more...)</a></th></tr>
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
        </tr>''' + most_recently_updated +  '''<tr><th colspan=4 valign="bottom"><h3>Full Caselist</h3></th></tr>   ''' + whole_list  + '''</table>
        
        </td>   
        
        </tr>
        <tr>
        <td></td><td></td><td></td><td></td><td></td>        
        </table>
        <hr>
        <address></address>
        </body> </html>'''


def handler(req):
	req.content_type = "text/html"
	req.send_http_header()
        req.write(pagestring)
        return apache.OK

###here's the web page that gets generated.



































