;; Compile the files: you need to be running scheme with the 
;; "-band compiler.com" command line option in order to have 
;; compilation tools loaded.
;; The compiled code will run an order of magnitude faster.
(cf "search.scm")
(cf "rule-utils.scm")
(cf "forward.scm")
(cf "match.scm")
(cf "schedule.scm")

