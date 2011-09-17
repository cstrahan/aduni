import Ns
import string
import re
conn = Ns.GetConn()
    
####### retrieve query data

query = conn.GetQuery()
query_exists = 1
school_name = string.strip(query.Get('school_name'))
password_1 = string.strip(query.Get('password_1'))
password_2 = string.strip(query.Get('password_2'))
ndt_district = string.strip(query.Get('ndt_district'))
ceda_region = string.strip(query.Get('ceda_region'))
ada_member = string.strip(query.Get('ada_member'))
email_contact = string.strip(query.Get('email_contact'))

html = '''
%s,%s,%s,%s,%s,%s,%s
''' % (school_name,password_1,password_2,ndt_district,ceda_region,ada_member,email_contact)

conn.ReturnHtml(200,html)
