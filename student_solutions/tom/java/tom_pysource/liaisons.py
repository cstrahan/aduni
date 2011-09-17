#####################################################
#liaisons.py
#Bryon Gill 4/26/01
#
#This page displays the contact information for school liaisons
#
import Ns
import zz
from displayify import displayify

conn = Ns.GetConn()


try:
     db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
     conn.ReturnRedirect('/project/error_page.html')
## try:
##      db2 = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
## except:
##      conn.ReturnRedirect('/project/error_page.html')

contact_query_string = """
SELECT DISTINCT
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
FROM persons, person_school_map
WHERE person_school_map.website_contact = true
AND persons.person_id = person_school_map.person_id
ORDER BY
persons.last_name, persons.first_names
"""
content = '''<TR><TD colspan=3 ALIGN="CENTER" WIDTH=100\%><H3>School Liaison Contact List</H3></TD></TR>'''
list = db.Select(contact_query_string)
while (db.GetRow(list) == Ns.OK):
     school = ""
     ##todo: make some of this stuff conditional on the existence of the fields so the display looks nicer.
     content = content + """<TR><TD rowspan=3 valign="TOP">%s, %s</TD><TD>%s<BR>%s %s %s</TD><TD>%s</TD></TR>
                           <TR><TD>office: %s<BR>mobile: %s<BR>fax: %s</TD></TR>
                           <TR><TD><ADDRESS><A HREF="mailto:%s">%s</TD></TR>""" % (list.Get('last_name'),
                             list.Get('first_names'),
                             school,
                             list.Get('street_address'),
                             list.Get('city'),
                             list.Get('state'),
                             list.Get('zipcode'),
                             list.Get('office_phone'),
                             list.Get('mobile_phone'),
                             list.Get('fax'),
                             list.Get('email'),
                             list.Get('email'))
    

html = displayify(content,db)
del db

conn.ReturnHtml(200,html)







    




























