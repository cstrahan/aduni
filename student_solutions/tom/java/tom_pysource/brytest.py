### Blake Asdorian, 4/18/01
### School Registration Page for SecondarySources.com

import Ns
import string
import re

conn = Ns.GetConn()

#  ####### retrieve query data
#  try:
#       query = conn.GetQuery()
#       query_exists = 1
#       school_name = string.strip(query.Get('school_name'))
#       team_name = string.strip(query.Get('team_name'))
#       member_one_first_name = string.strip(query.Get('member_one_first_name'))
#       member_one_last_name = string.strip(query.Get('member_one_last_name'))
#       member_two_first_name = string.strip(query.Get('member_two_first_name'))
#       member_two_last_name = string.strip(query.Get('member_two_last_name'))
     
#  except:
#       query_exists = 0
#       school_name =''
#       team_name = ''
#       member_one_first_name = '' 
#       member_one_last_name = ''
#       member_two_first_name = ''
#       member_two_last_name = ''
     
#  ###### do some error checking on the input
#  valid_input = 0

#  if (query_exists):
#       pass
#  ###################### generate the appropriate page, depending on input 

#  if (valid_input == 0):
#       pass

################# generate the school list for which to register the team
school_sql = 'SELECT school_id,school_name FROM schools'

try:
     db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
     conn.ReturnRedirect('error_page')

school_req = db.Select(school_sql)

school_list = ''
while (db.GetRow(school_req) != Ns.END_DATA):
        school_list = school_list + '<OPTION VALUE=%s>%s</OPTION>' % (school_req.IGet('school_id'),school_req.IGet('school_name'))

if (school_list != ''):
        school_list = '<B>School</B><SELECT NAME=school_list>' + school_list + '</SELECT>'





########## generate the html code for the registration page
html = '''
     <HTML>
     <head><title>Secondary Sources</title>
     </head>
     <body bgcolor=white>
     <HR>
     <H2>Team Registration</H2>
     <HR>
     <FORM METHOD=POST ACTION='team_reg.py'>
     %s
     </FORM>
     </HTML>
     ''' % (school_list)
     
#       <form METHOD=POST action=team_reg.py>
#       <TABLE CELLPADDING=5 CELLSPACING=0 BORDER=0>
#       <TR>
#       <TD>Team Name:
#       <TD><input name=team_name type=text size=5 VALUE=\"%s\">
#       <TD>%s
#       </TR>
#       <TR>
#       <TD>
#       <TD>
#       </TR>
#       <TR>
#       <TD>
#       <TD>
#       </TR>
#       <TR>
#       <TD>
#       <TD>
#       </TR>
#       </TABLE>
#       <HR>
#       %s
#       <INPUT TYPE=SUBMIT VALUE=Register>
#       </FORM>
#       </HTML>
#     ''' % (school_name, 'hello','hello')

#  elif (valid_input == 1):
#  ############################ if all fields contained valid input, insert data and return completion page w/link to (main page or school page)?
#       sql = '''insert into schools (school_name,date_registered,ndt_district,ceda_region,school_password) VALUES ('%s',CURRENT_TIMESTAMP,'%s','%s','%s')''' % (re.escape(school_name),re.escape(ndt_district),re.escape(ceda_region),re.escape(password_1))
#       try:
#            db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
#            db.DML(sql)
#       except:
#            conn.ReturnRedirect('error_page')
     
#       html = '''
#       <HTML>
#       <head><title>Secondary Sources</title>
#       </head>
#       <body bgcolor=white>
#       <HR>
#       <H2>School Registration</H2>
#       <HR>
#       <H4>You have successfully registered your school with SecondarySources.com</H4>
#       return to
#       <A HREF="main_page">main page.</A>
#       <HR>
#       </BODY>
#       </HTML>'''

#####################################################################
conn.ReturnHtml(200, html)


















