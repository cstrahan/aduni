set email [ns_queryget wml_email]
set pwd [ns_queryget wml_pwd]

set sql_user "select user_id, last_visit from users
 where upper(email) = upper(:email) and password = :pwd
 and status != 'banned'"

if { [db_0or1row get_user_info $sql_user] == 0 } {
    set page_content "
    <wml>
    <card>
    <p>Your Email and Password do not match.</p>
    </card>
    </wml>
    "
    ns_return 200 text/vnd.wap.wml $page_content
    return
}


set i_groups {} ;# the distinct groups viewer is mapped to

set viewer_id $user_id

# We don't ask for distinct goup_id, since a doctor can be leader & member of a practice!

set sql_query "select m.group_id, m.role, g.short_name
  from user_group_map m, groups g
  where m.user_id = :viewer_id and m.group_id = g.group_id"

db_foreach get_users $sql_query {
    if { [lsearch $i_groups $group_id] == -1 } { ;# distinct groups
	lappend i_groups $group_id
    }
}


set page_content "
<wml>
    <card>
"

set n_active 1 ;# preset number of active groups at 1

foreach i_group $i_groups {

    set sql_query "select g.group_id, g.status, 
     g.short_name, g.pretty_name, g.meeting_time, g.meeting_note  
     from groups g, user_group_map m where
     g.group_id = :i_group and g.group_id = m.group_id
     and m.user_id = :viewer_id 
     and (g.status = 'active')"

    if { [db_0or1row get_groups $sql_query] == 1 } {
	;# <= 1
	append page_content "<p>$n_active. $pretty_name $short_name <br/> - $meeting_time<br/> - $meeting_note</p>"
        incr n_active
    }
} ;# foreach

if { $n_active == 1 } {
    append page_content "You don't seem to be in any active groups.<br/>"
}
# end of Groups

append page_content "
</card>
</wml>"

db_release_unused_handles

ns_return 200 text/vnd.wap.wml $page_content
