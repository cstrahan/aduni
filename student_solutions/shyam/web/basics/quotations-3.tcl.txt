# quotations-3.tcl - sets and modifies cookies, writes them to the client
# and then redirects to quotations.tcl
# Written by Shyam Visweswaran at ADUni

if { [ns_queryexists "q_cookie"] && [ns_queryget "q_cookie"] == "set" } {
    set headers [ns_conn outputheaders]
    ns_set put $headers Set-Cookie "kill_id=; path=/; expires=Fri, 01-Jan-2010 01:00:00 GMT"
} elseif { [ns_queryexists "q_cookie"] && [ns_queryget "q_cookie"] == "expire" } {
    set headers [ns_conn outputheaders]
    ns_set put $headers Set-Cookie "kill_id=; path=/; expires=Fri, 01-Jan-1990 01:00:00 GMT"
} else {
    set headers [ns_conn outputheaders]
    set kill_id [ns_queryget "kill_id"]
    ns_set put $headers Set-Cookie "kill_id=$kill_id; path=/; expires=Fri, 01-Jan-2010 01:00:00 GMT"    
}

# after writing out the cookie to the client, redirect to quotations.tcl

ns_returnredirect "quotations.tcl"


