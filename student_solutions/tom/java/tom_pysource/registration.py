import Ns, sys, string, re, cgi
import Cookie

header = 'Content-type: text/html\n'

html_start = '''<html><head><title>Secondary Sources Login</title>
</head><body bgcolor=white>'''

html_end = '''<hr><address><a href="ssindex.html">Contact the administrator</a>
</address></body></html>'''

###User Registration: based in the data structure outlined in users.txt###
###Main goals are to generate an HTML form and make sure correct information
###is put into the database...

start_form = '''<form action=registration.py>
Your first name/names:
<input name=first_names type=text size=45><br>
Your last name:
<input name=last_name type=text size=45><br>
Your email:
<input name=email type=text size=45><br>
Please pick your school:<select name=school>
<option value=None selected>Pick a school'''

opt_item = '<option value=%s>%s'

end_form = '''</select>  If your school is not listed, enter it below:<br>
<input name=newschool type=text size=35>
<hr>
Please check all of the following that apply to you:<br>
<input type=radio name=is_jv value="t">I compete on the JV level
<input type=radio name=is_varsity value="t">I compete on the varsity level<br>
<input type=radio name=is_faculty value="t">I am a debate teacher or coach
<input type=radio name=is_student value="t">I am a student<br>
<input type=radio name=is_highschool value="t">I compete on the high school level
<input type=radio name=is_college value="t"I compete on the college level<br>
<input type=radio name=priv_email value="t"I do not want my email to be shown when I post a submission<br>
<input type=radio name=priv_name value="t"I do not want my name to be shown when I post a submission<br>
<hr>
If you have a personal website, please enter it below:<br>
<input name=url type=text size=60>
Please enter your passowrd to enter this site below:<br>
<input name=password type=text size=30>
Please re-enter your new password for verification:<br>
<input name=password_v type=text size=30>
Finally, if you wish for a signature to be displayed if/when you post a message, please enter it below:<br>
<textarea name=sig cols=40 rows=4></textarea>
<br>
<input type=submit value="Submit This Form">
<input type=reset value="Reset This Form">
</form>'''



error_form = '''<h4>Error</h4>Please correct the following errors that occured 
with your application:<br><font color=red>%s</font>'''

sqlschools = 'SELECT DISTINCT school FROM users'

sqlinsert = 'INSERT INTO users () VALUES ()'
###to be modified later

conn = Ns.GetConn()
form = cgi.FieldStorage()
try:
   	dbase = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))	
except:
   	print 'can\'t get handle -- do you have a database installed??'

def showForm():
	print start_form
	try:
		res_school = dbase.Select(sqlschools)
        	while dbase.GetRow(res_school) == Ns.OK:
			print opt_item % (res_school[0],res_school[0],)
	except:
		print opt_item % ('Null','Null',)
	print end_form

def showError(err):
	print header
	print html_start
	print error_form % (err)
	showForm()
	print html_end

def processInput():
	error = ''
        err = ''
        
	



