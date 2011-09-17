
import Ns, sys
import string

#Tom Hickerson, Problem set 1, Exercise 10-11, publishing data in XML
#April 12th, 2001

xmlstr = '''   <onequote>
      <quotation_id>%s</quotation_id>
      <insertion_date>%s</insertion_date>
      <author_name>%s</author_name>
      <category>%s</category>
      <quote>%s</quote>
   </onequote>

'''

failstr = '''   <onequote>
      FAIL
   </onquote>

'''

def xmlprint(num,dat,auth,cat,quo):
    print xmlstr % (num,dat,auth,cat,quo)

conn = Ns.GetConn()

print 'Content-type: application/xml\n'

print '<quotations>'

try:
   dbase = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))
except:
   print 'can\'t get handle -- do you have a database installed??'

sql = 'SELECT * FROM quotations;'

try:
   results = dbase.Select(sql)
   while dbase.GetRow(results) == Ns.OK:
      xmlprint(results[0],results[1],results[2],results[3],results[4],)
except:
   print failstr

print '</quotations>'

del dbase



