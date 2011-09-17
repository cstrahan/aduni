# get-pwd-2.tcl - if email submitted in get-pwd.tcl exists in users table
# mail password to user.
# Written by Shyam Visweswaran at ADUni, 27 April 2001

set_the_usual_form_variables
set email [string trim $email]

if { [string equal $email ""] } {

    ns_return 200 text/plain "You forgot to type your email address. Go back and type it in."
    return

} else {

    if { [db_0or1row get_password "select password from users where upper(email) = upper(:email)"] == 0 } {

	set page_content "
	[html_page_header 0 "Forgotten password" "<a href=\"login.tcl\">Login</a> > Forgotten password"]
	
	<!-- stuff for body of page -->
	
	<table width=90%>
	<tr><td>

	We could not find your email address below in our system. Retype your email
	address if you have made a mistake.
	<p>
	<form method=post action=get-pwd-2>
	<input type=text name=email size=40 value=$email>
	&nbsp; &nbsp <input type=submit value=Submit>
	</form>
	
	</td></tr>
	</table>
	[html_page_footer 0]        
	"
	ns_return 200 text/html $page_content
	return

    } else {

	set email_body "You had requested your password from the Partners in Education website.
	Your password is '$password'."

	ns_sendmail "$email" "PartnersInEducation@mgh.org" "Mail from Partners in Education" $email_body
	
	set page_content "
	[html_page_header 0 "Password mailed" "<a href=\"login.tcl\">Login</a> > Password mailed"]
	
	<!-- stuff for body of page -->
	
	<table width=90%>
	<tr><td>
	
	Your password has been emailed to you at <i>$email</i>
	
	</td></tr>
	</table>
	[html_page_footer 0]    	
	"
	ns_return 200 text/html $page_content
	return
    }
}









