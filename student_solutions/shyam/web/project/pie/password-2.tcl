# password-2.tcl - updates member's password
# target: password.tcl; parameters: pwd_id, old_pwd, new_pwd1, new_pwd2
# Written by Shyam Visweswaran at ADUni, 27 April 2001

set_the_usual_form_variables
set old_pwd [string trim $old_pwd]
set new_pwd1 [string trim $new_pwd1]
set new_pwd2 [string trim $new_pwd2]
set error_text ""

if { [string equal $old_pwd ""] } {
    append error_text "<li><font color=red>You forgot to type your old password.</font>"
} elseif { [string equal $new_pwd2 ""] } {
    append error_text "<li><font color=red>You forgot to type your new password.</font>"
} elseif { [string equal $new_pwd2 ""] } {
    append error_text "<li><font color=red>You forgot to retype your new password.</font>"
} elseif { ![string equal $new_pwd1 $new_pwd2] } {
    append error_text "<li><font color=red>Your new passwords do not match.</font>"
} elseif { [string length $new_pwd1] < 5 } {
    append error_text "<li><font color=red>Your new password should be at least 5 characters long. Please choose a longer one.</font>"
} elseif { [db_0or1row get_password "select password from users where user_id = :pwd_id"] == 1 } {
    if { ![string equal $password $old_pwd] } {
	append error_text "<li><font color=red>Your old password does not match with our records. Please retype your old password.</font>" 
    }
}   

if { [string equal $error_text ""] } {
    db_dml update_password "update users set password = :new_pwd1 where user_id = :pwd_id"
    
    set page_content "
    [html_head "Change password"]
    [html_body_start]
    [html_body_top "Change password" "<a href=\"home.tcl\">Home</a> > Change password"]
    
    <!-- stuff for body of page -->
    
    <table width=90%>
    <tr><td>
    
    Your password has been successfully updated.
    
    </td></tr>
    </table>
    
    [html_body_bottom]
    [html_body_end]
    [html_foot]
    "
    ns_return 200 text/html $page_content
}

set page_content "
[html_head "Change password"]
[html_body_start]
[html_body_top "Change password" "<a href=\"home.tcl\">Home</a> > Change password"]

<!-- stuff for body of page -->

<blockquote>
<table width=90%>
<tr><td>

<ul>
$error_text
</ul>

<form method=post action=password-2.tcl>
<input type=hidden name=pwd_id value=$pwd_id>    
Please type your old password
<br>
<input type=password name=old_pwd size=30>
<p>

Now type your new password (should be at least 5 characters long)
<br>
<input type=password name=new_pwd1 size=30>
<p>

And retype your new password
<br>
<input type=password name=new_pwd2 size=30>
<p>

<input type=submit value=Submit>
</form>

</td></tr>
</table>
</blockquote>

[html_body_bottom]
[html_body_end]
[html_foot]
"

ns_return 200 text/html $page_content




