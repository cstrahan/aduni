### Blake Asdorian, 4/18/01
### School Registration Page for SecondarySources.com

import Ns
import string
import re

conn = Ns.GetConn()

############# define the procedure to check for email against registered persons
#  def registered_email(email):
#       try:
#            db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
#            set = db.Select('select person_id from persons where email=\'%s\'' % (re.escape(email)))
#            db.GetRow(set)
#            del db
#            return set.Get('person_id')
#       except:
#            return None
#
####### retrieve query data
try:
     query = conn.GetQuery()
     query_exists = 1
     school_name = string.strip(query.Get('school_name'))
     password_1 = string.strip(query.Get('password_1'))
     password_2 = string.strip(query.Get('password_2'))
     ndt_district = string.strip(query.Get('ndt_district'))
     ceda_region = string.strip(query.Get('ceda_region'))
     ada_member = string.strip(query.Get('ada_member'))
     #email_contact = string.strip(query.Get('email_contact'))
except:
     query_exists = 0
     school_name =''
     password_1 =''
     password_2 =''
     ndt_district = ''
     ceda_region = ''
     ada_member = ''
     #email_contact = ''
     
###### do some error checking on the input
password_error = ''
school_error = ''
email_error = ''
valid_input = 0
#legal_email = re.compile(r'[a-zA-Z][\w-]*@\w+\.\w+')
#person_id = registered_email(email_contact)

if (query_exists):
     #valid_email = legal_email.match(email_contact)
     if (cmp(school_name,'') == 0):
          school_error = '<FONT SIZE=4 COLOR=RED>Please Enter a School Name</FONT>'
     #elif not (valid_email):
     #     email_error = '<FONT SIZE=4 COLOR=RED> Please Enter a Valid Email Address</FONT>'
     #elif not (person_id):
     #     email_error = '<FONT SIZE=4 COLOR=RED> This is not a Registered Email...</FONT><FONT SIZE=4><A HREF=\"person_registration.py\">register your email here</A></FONT>'
     elif (cmp(password_1,'') == 0):
          password_error = '<FONT SIZE=4 COLOR=RED>Please Choose a School Password</FONT>'
     elif (cmp(password_2,'') == 0):
          password_error = '<FONT SIZE=4 COLOR=RED>Please Re-Enter Your School Password</FONT>'
     elif (cmp(password_1,password_2) != 0):
          password_error = '<FONT SIZE=4 COLOR=RED>Your Passwords Do Not Match</FONT>'
     else:
          valid_input = 1
     
###################### generate the appropriate page, depending on input 

if (valid_input == 0):
############ if the input had problems, or if first visiting the registraion page #################
###### generate select elements html 
     ndt_district_list = ['--None--','District I','District II','District III','District IV','District V','District VI','District VII','District VIII','District IX','District X','District XI','District XII']
     ndt_district_html = ''
     for district in ndt_district_list:
          if (cmp(district,ndt_district) == 0):
               selected = 'SELECTED'
          else:
               selected = ''
          ndt_district_html = ndt_district_html + '<OPTION %s VALUE=\"%s\">%s</OPTION>' % (selected,district,district)
               
     ceda_region_list = ['--None--','Northwest','West','Southern California','Rocky Mountain','North Central','Mid-America','East Central','South Central','Southeast Central','Southeast','East']
     ceda_region_html = ''
     for region in ceda_region_list:
          if (cmp(region,ceda_region) == 0):
               selected = 'SELECTED'
          else:
               selected = ''
          ceda_region_html = ceda_region_html + '<OPTION %s VALUE=\"%s\">%s</OPTION>' % (selected, region,region)

     if (cmp(ada_member,'true') == 0):
          ada_sel = 'SELECTED'
     else:
          ada_sel = ''
          
########## generate the html code for the registration page
     html = '''
     <HTML>
     <head><title>Secondary Sources</title>
     </head>
     <body bgcolor=white>
     <HR>
     <H2>SecondarySources.com</H2>
     <H3>School Registration</H3>
     return to
     <A HREF="/project/admin/admin2.py">admin page.</A>
     <HR>
     <form METHOD=POST action=school_registration.py>
     <TABLE CELLPADDING=5 CELLSPACING=0 BORDER=0>
     <TR>
     <TD>School Name:
     <TD><input name=school_name type=text size=30 VALUE=\"%s\"><FONT COLOR="RED"> *</FONT>
     <TD>%s
     </TR>
     <TR>
     <TD>ndt_district:
     <TD><SELECT NAME=ndt_district>%s</SELECT>
     </TR>
     <TR>
     <TD>ceda_region:
     <TD><SELECT NAME=ceda_region>%s</SELECT>
     </TR>
     <TR>
     <TD>ada_member:
     <TD><SELECT NAME=ada_member><OPTION VALUE=false>No</OPTION><OPTION %s VALUE=true>Yes</OPTION></SELECT>
     </TR>
     </TABLE>
     <HR>
     %s
     <TABLE>
     <TR>
     <P>Each school has a password that allows its team members to login.  This is the password you will send to the school liason for team distribution.  If there is no school liason, choose a password that will be hard for random site visitors to guess.  You can always change the password later from the school admin page.
     <TD><B>Create New School Password</B><TD>:<TD><INPUT TYPE=PASSWORD NAME=password_1 SIZE=20 VALUE=\"%s\"><FONT COLOR="RED"> *</FONT>
     </TR>
     <TR>
     <TD><B>Re-Enter School Password</B><TD>:<TD><INPUT TYPE=PASSWORD NAME=password_2 SIZE=20 VALUE=\"%s\"><FONT COLOR="RED"> *</FONT>
     </TR>
     <TR>
     <TD COLSPAN=3 ALIGN=CENTER><INPUT TYPE=SUBMIT VALUE=Register>
     </TR>
     </TABLE>
     </FORM>
     </HTML>
     ''' % (school_name, school_error, ndt_district_html, ceda_region_html, ada_sel,password_error, password_1, password_2)
#(school_name, school_error, ndt_district_html, ceda_region_html, ada_sel,email_error,email_contact, password_error, password_1, password_2)

elif (valid_input == 1):
############################ if all fields contained valid input, insert data and return completion page w/link to (main page or school page)?
     sql = '''insert into schools (school_name,date_registered,ndt_district,ceda_region,school_password) VALUES ('%s',CURRENT_TIMESTAMP,'%s','%s','%s')''' % (re.escape(school_name),re.escape(ndt_district),re.escape(ceda_region),re.escape(password_1))
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
     <H4>You have successfully registered your school with SecondarySources.com</H4>
     return to
     <A HREF="/project/admin/admin2.py">admin page</A>
     <HR>
     </BODY>
     </HTML>'''

conn.ReturnHtml(200, html)


















