# Description: form-variable handling procs

proc DoubleApos {string} {
    regsub -all ' "$string" '' result
    return $result
}

# this one does both the regular and the QQ
proc set_the_usual_form_variables {{error_if_not_found_p 1}} {
    if { [ns_getform] == "" } {
	if $error_if_not_found_p {
	    uplevel { 
		ns_returnerror 500 "Missing form data"
		return
	    }
	} else {
	    return
	}
    }
    uplevel {
	set form [ns_getform] 
	set form_size [ns_set size $form]
	set form_counter_i 0
	while {$form_counter_i<$form_size} {
	    set [ns_set key $form $form_counter_i] [ns_set value $form $form_counter_i]
	    set QQ[ns_set key $form $form_counter_i] [DoubleApos [string trim [ns_set value $form $form_counter_i]]]
	    incr form_counter_i
	}
    }
}

proc_doc ad_return_complaint {exception_count exception_text} "Return a page complaining about the user's input (as opposed to an error in our software, for which ad_return_error is more appropriate)" {
    # there was an error in the user input 
    if { $exception_count == 1 } {
	set problem_string "a problem"
	set please_correct "it"
    } else {
	set problem_string "some problems"
	set please_correct "them"
    }
	    
    ns_return 200 text/html "
<html>
<head>
<title>
Problem with Your Input
</title>
</head>

<body>
<body bgcolor=#ffffff text=#000000>
    
<h2>Problem with Your Input</h2>

to <a href=/>[ns_info hostname]</a>

<hr>

We had $problem_string processing your entry:
	
<ul> 
	
$exception_text
	
</ul>
	
Please back up using your browser, correct $please_correct, and
resubmit your entry.
	
<p>
	
Thank you.
	
<hr>
<a href=\"http://photo.net/philg/\"><address>philg@mit.edu</address></a>
</body>
</html>
"
}

proc_doc ad_call_proc_if_exists { proc args } {

Calls a procedure with particular arguments, only if the procedure is defined.

} {
    if { [llength [info procs $proc]] == 1 } {
	eval $proc $args
    }
}








