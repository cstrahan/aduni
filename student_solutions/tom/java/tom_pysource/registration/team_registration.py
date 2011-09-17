### Blake Asdorian, 4/18/01
### School Registration Page for SecondarySources.com

import Ns
import string
import re
import displayify

conn = Ns.GetConn()

######################################################################
def registered_email(email):
     try:
          db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
          set = db.Select('select person_id from persons where email=\'%s\'' % (re.escape(email)))
          db.GetRow(set)
          return set.Get('person_id')
     except:
          return None

####### retrieve query data
try:
     query = conn.GetQuery()
     query_exists = 1
     school_id = string.strip(query.Get('school_id'))
     team_name = string.strip(query.Get('team_name'))
     member_one_first_name = string.strip(query.Get('member_one_first_name'))
     member_one_last_name = string.strip(query.Get('member_one_last_name'))
     member_two_first_name = string.strip(query.Get('member_two_first_name'))
     member_two_last_name = string.strip(query.Get('member_two_last_name'))
     email_contact = string.strip(query.Get('email_contact'))
except:
     query_exists = 0
     school_id =''
     team_name = ''
     member_one_first_name = '' 
     member_one_last_name = ''
     member_two_first_name = ''
     member_two_last_name = ''
     email_contact = ''
     
###### do some error checking on the input
email_error = ''
team_error = ''
valid_input = 0
legal_email = re.compile(r'[a-zA-Z][\w-]*@\w+\.\w+')
person_id = registered_email(email_contact)

if (query_exists):
     valid_email = legal_email.match(email_contact)
     if (cmp(team_name,'') == 0):
          team_error = '<FONT SIZE=4 COLOR=RED> Please Enter Team Initials</FONT>'
     elif not (valid_email):
          email_error = '<FONT SIZE=4 COLOR=RED> Please Enter a Valid Email Address</FONT>'
     elif not (person_id):
          email_error = '<FONT SIZE=4 COLOR=RED> This is not a Registered Email...</FONT><FONT SIZE=4><A HREF=\"person_registration.py\">register your email here</A></FONT>'
     else:
          valid_input = 1
###################### generate the appropriate page, depending on input 

if (valid_input == 0):

################# generate the school list for which to register the team
     school_sql = 'SELECT school_id,school_name FROM schools'

     try:
          db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
          school_req = db.Select(school_sql)
     except:
          conn.ReturnRedirect('error_page')
          
               
     school_list = ''
     while (db.GetRow(school_req) != Ns.END_DATA):
          if (cmp(school_id,school_req.IGet('school_id')) == 0):
               selected = 'SELECTED'
          else:
               selected = ''
          school_list = school_list + '''<OPTION %s VALUE="%s">%s</OPTION>''' % (selected, school_req.IGet('school_id'),school_req.IGet('school_name'))

     del db
     
     if (school_list != ''):
          school_list = '<SELECT NAME=school_id>' + school_list + '</SELECT>'

########## generate the html code for the registration page
     html = '''
     <H3>Team Registration</H3>
     <HR>
     Mandatory Fields<FONT COLOR="RED"> *</FONT><BR>
     Team Initials are the first initials of last names in alphabetical order. (e.g. a team of Paul Martin and James Hamilton would be team "HM")
     <FORM METHOD=POST ACTION='team_registration.py'>
     <TABLE>
     <TR>
     <TD></TD>
     <TD ALIGN=CENTER><B>School</B></TD>
     <TD ALIGN=CENTER><B>Team Initials</B></TD>
     </TR>
     <TR>
     <TD></TD>
     <TD>%s<FONT COLOR="RED">*</FONT></TD>
     <TD ALIGN=CENTER><INPUT TYPE=TEXT SIZE=10 NAME=team_name VALUE="%s"><FONT COLOR="RED"> *</FONT></TD>
     <TD>%s</TD>
     </TR>
     <TR>
     <TD COLSPAN=3><HR></TD>
     </TR>
     <TR>
     <TD></TD>
     <TD ALIGN=CENTER><B>First Name</B></TD>
     <TD ALIGN=CENTER><B>Last Name</B></TD>
     </TR>
     <TR>
     <TD ALIGN=CENTER>Member One:</TD>
     <TD ALIGN=CENTER><INPUT TYPE=TEXT SIZE=20 NAME=member_one_first_name VALUE="%s"></TD>
     <TD ALIGN=CENTER><INPUT TYPE=TEXT SIZE=20 NAME=member_one_last_name VALUE="%s" ></TD>
     </TR>
     <TR>
     <TD ALIGN=CENTER>Member Two:</TD>
     <TD ALIGN=CENTER><INPUT TYPE=TEXT SIZE=20 NAME=member_two_first_name VALUE="%s" ></TD>
     <TD ALIGN=CENTER><INPUT TYPE=TEXT SIZE=20 NAME=member_two_last_name VALUE="%s"></TD>
     </TR>
     <TR>
     <TD COLSPAN=3><HR>
     </TR>
     <TR>
     <TD COLSPAN=3 ALIGN=CENTER>Enter your Registered Email
     <TD>%s
     </TR>
     <TR>
     <TD COLSPAN=3 ALIGN=CENTER><input name=email_contact type=text size=50 VALUE=\"%s\"><FONT COLOR="RED"> *</FONT>
     </TR>
     <TR>
     <TD COLSPAN=3 ALIGN=CENTER><INPUT TYPE=SUBMIT VALUE=Register>
     </TR>
     </TABLE>
     </FORM>
     ''' % (school_list,team_name,team_error,member_one_first_name,member_one_last_name,member_two_first_name,member_two_last_name,email_error,email_contact)
     
elif (valid_input == 1):
############################ if all fields contained valid input, insert data and return completion page w/link to (main page or school page)?
     sql = '''insert into teams (school_id,team_name,date_created,member_one_first_name,member_one_last_name,member_two_first_name,member_two_last_name) VALUES ('%s','%s',CURRENT_TIMESTAMP,'%s','%s','%s','%s')''' % (re.escape(school_id),re.escape(team_name),re.escape(member_one_first_name),re.escape(member_one_last_name),re.escape(member_two_first_name),re.escape(member_two_last_name))
     try:
          db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
          db.DML(sql)
          del db
     except:
          conn.ReturnRedirect('error_page')
     
     html = '''
     <HTML>
     <head><title>Secondary Sources</title>
     </head>
     <body bgcolor=white>
     <HR>
     <H2>School Registration</H2>
     <HR>
     <H4>You have successfully registered a team with SecondarySources.com</H4>
     return to
     <A HREF="/project/front.py">main page.</A>
     <HR>
     </BODY>
     </HTML>'''

#####################################################################

db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
html = displayify.displayify(html,db)
del db

conn.ReturnHtml(200, html)


















