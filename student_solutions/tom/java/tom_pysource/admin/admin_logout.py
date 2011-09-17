### Blake Asdorian 4/28/01
### /project/admin/admin_logout
########
### This page should set an expired cookie to logout the site administrator


import Ns
import zz

zz.set_cookie('bosphorous',None)

conn = Ns.GetConn()
conn.ReturnRedirect('http://10.11.0.129/project/front.py')
conn.ReturnOK()
