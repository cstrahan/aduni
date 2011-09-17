#####################################################
#displayify.py
#Bryon Gill 4/24/01
#
#This file defines a function which returns a string of html for the content display pages
# it takes three arguments; content, which generates rows for the content table, left_sidebar and right_sidebar.
# default sidebar values include generic links.
import Ns
import zz

conn = Ns.GetConn()

def displayify(content, db):

    am_I_logged_in = zz.valid_user_p(db)
    
    html = '''<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">
        <html> <head>
        <title>Secondary Sources Caselist</title>
        </head>
        <body bgcolor=white>
        <table width=100\% align="center" cellspacing=0>
        <tr>
                <td width = 15% BGCOLOR=gray>
                2001-2002 Season
                </td>
        
                <td width=70\% BGCOLOR=gray>
                <h1><center>Secondary Sources</center></h1>
                </td>
                <td width = 15\% BGCOLOR=gray>
                &nbsp;
                </td>
              </td>        
        </tr>
        </table>
        <table width=100\% align="center">
        <tr>
                <td width=15\% rowspan=10 valign=top>
                <!-- this is the left sidebar-->
                ''' + login_dialog(am_I_logged_in) + '''
                <li><a href=/project/front.py>Home</a><p>
                <li><a href=/project/contact_list.py>Community Contact List</a><p>
                <li><a href=/project/admin/contact.html>Contact Us</a><p>

                <h3>Links</h3><p>
                <li><a href=http://www.debate.net>Debate.net<p>
                <li><a href=http://debate.uvm.edu>Debate Central</a><p>                
                <li><a href=http://www.oneparadigm.comx>Paradigm Research</a><p>
                <li><a href=http://www.lexis-nexis.com>Lexis-Nexis</a><p>
                </td>
                <td width=85\%>
                <h1><center></center></h1>
                </td>
              </td>        
        </tr>
        <tr align=left valign=top>        
                <td width=60\%>
                <table cellpadding=5>
                        ''' + content + '''
                </table>
                        </td>           
                </tr>
                <tr>
                        <td></td>
                        <td></td>
                        <td></td>
                        <td></td>
                        <td></td>
                </tr>        
        </table>
        <hr>
        <address></address>
        </body> </html>'''
    return html

###this function returns the appropriate login or logout box.
def login_dialog(cookie_info):
    login_html = '''<FORM ACTION="/project/front.py" METHOD="post">Login
                    <a href=/project/registration/person_registration.py>(new user)</a><br>
                    Email:<br><INPUT TYPE="TEXT" NAME="email" SIZE=20><br>
                    Password:<br><INPUT TYPE="password" NAME="password" size=20><br>
                    <INPUT TYPE="SUBMIT" VALUE="Log me in"></FORM><br><h3>Navigate</h3>'''

        
    logged_in_html = '''Welcome!<p><FORM ACTION="/project/front.py" METHOD="post"> <INPUT TYPE="submit" NAME="logout" VALUE="Log me out"></FORM><BR>
                        <h3>Navigate</h3>
                        <li><a href=/project/submit/content_submission.py>Submit New Caselist Info</a><p>'''
                        
                        
    if cookie_info:
        return logged_in_html
    else:
        return login_html




##TODO:
##           These are links that need to be implemented appropriately.
##                 <li><a href=/project/registration/team_registration.py>Register a new team</a><p>
##                 <li><a href=/project/registration/school_registration.py>Register your school</a><p> 
##                 <li><a href=/project/insert-link>Contact List</a><p>





    





    
##legacy code: delete when the new stuff works! ############################################################################################################
##     try:
##         ###three interesting cases exist:
##         ###1. if there's post information
##         ###check for post information (login info)
##         query = conn.GetQuery()
##         if query.has_key('logout'):
##             ss_cookie.cancel_cookie()        
##         ###if we find login info, great, update the cookie
##         if query.has_key('password'):
##             check_password(query['password'])  
##             ss_cookie.set_cookie('asdf')
##             am_I_logged_in = 1

##         ###2. if there's no post information but there is a cookie, they are logged in already
##         ###   (eventually we'll authenticate the cookie somehow,but for now, cookie=logged in already)
##     except:
##         if ss_cookie.get_cookie(): ###our test is to see if the get_cookie function returns a non-null value
##             am_I_logged_in = 1
##         ###3. Finally, if there's no cookie and no post information, then they have to log in manually.
##         else:
##             am_I_logged_in = 0
## ################################################################################################################




























