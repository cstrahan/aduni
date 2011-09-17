#####################################################
#contact_list.py
#Bryon Gill 4/26/01
#
#This page displays the contact information for users of the
#system.
#
import Ns
import zz
from displayify import displayify

conn = Ns.GetConn()


try:
     db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
     conn.ReturnRedirect('/project/error_page.html')

contact_query_string = """
SELECT
persons.person_id,
persons.email,
persons.first_names,
persons.last_name,
persons.street_address,
persons.city,
persons.state,
persons.zipcode,
persons.office_phone,
persons.mobile_phone,
persons.fax
FROM persons
WHERE persons.status != 'not approved'
ORDER BY
UPPER(persons.last_name), UPPER(persons.first_names)
"""
content = '''<TR><TD colspan=3 ALIGN="CENTER" WIDTH=100\%><H3>Contact List</H3></TD></TR>'''
list = db.Select(contact_query_string)
color = ""
while (db.GetRow(list) == Ns.OK):
     if color == "":
          color = "CCFFFA"
     else:
          color = ""
     #optional fields
     if list.Get('street_address'):
          street = list.Get('street_address')
     else:
          street = '&nbsp;'
     if list.Get('street_address'):
          street = list.Get('street_address')
     else:
          street = '&nbsp;'
     if list.Get('city'):
          city = list.Get('city') + ', '
     else:
          city = '&nbsp;'
     if list.Get('state'):
          state = list.Get('state')
     else:
          state = '&nbsp;'
     if list.Get('zipcode'):
          zipcode = list.Get('zipcode')
     else:
          zipcode = '&nbsp;'
     if list.Get('office_phone'):
          office = 'office: %s<BR>' % list.Get('office_phone')
     else:
          office = '&nbsp;'
     if list.Get('mobile_phone'):
          mobile = 'mobile: %s<BR>' % list.Get('mobile_phone')
     else:
          mobile = '&nbsp;'
     if list.Get('fax'):
          fax = 'fax: %s' % list.Get('fax')
     else:
          fax = '&nbsp;'
     content = content + '''<TR BGCOLOR="%s"><TD rowspan=2 valign="TOP">%s, %s<BR><ADDRESS><A HREF="mailto:%s">%s</ADDRESS></TD><TD>%s<BR> %s %s %s</TD></TR>
                           <TR BGCOLOR ="%s"><TD>%s %s %s</TD></TR>''' % (color,
                                                                          list.Get('last_name'),
                                                                          list.Get('first_names'),
                                                                          list.Get('email'),
                                                                          list.Get('email'),
                                                                          street,
                                                                          city,
                                                                          state,
                                                                          zipcode,
                                                                          color,
                                                                          office,
                                                                          mobile,
                                                                          fax)
     

html = displayify(content,db)
del db

conn.ReturnHtml(200,html)







    



##This didn't work right, but it would be nice to show affiliation
##      person_in_question = list.Get('person_id')
##      school_query_string ="""
##      SELECT schools.school_name
##      FROM schools, person_school_map
##      WHERE person_school_map.person_id = %s
##      AND person_school_map.school_id = schools.school_id
##      AND schools.status != 'not approved'
##      """ % person_in_question
##     school_query = db2.Select(school_query_string)
##      while (db2.GetRow(school_query) == Ns.OK):
##            next_school = school_query.Get('school_name')
##            if next_school:
##                 school = school + next_school + "<BR>"

























