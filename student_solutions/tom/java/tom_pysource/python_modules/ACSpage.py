### $Header: /cvsroot/PyWX/PyWX/ACSpage.py,v 1.2 2000/05/23 22:24:59 titus Exp $
###
### See included file COPYRIGHT for copyright and license information.
###

from ns_python import *

class ACSpage:
    def __init__(self, title):
        self.title = title

    def stuffDict(self, d = {}):                   # d should be dict
        d['header'] = ns_tcl.ad_header(self.title)
        d['footer'] = ns_tcl.ad_footer()
        return d
