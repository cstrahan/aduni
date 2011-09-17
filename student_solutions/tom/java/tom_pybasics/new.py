#! /usr/bin/python

#try all the imports

#import extendpath
import os
#import cgi
import pwd
import socket
import urlparse
from time import *

print "Content-type: text/html\n\n"

print "<html><head><title>"
print "Hey ADUNI two!!"

print"</title></head>"
print"<body bgcolor=white text=black><h3>Hey ADUNI!!</h3>"
print strftime("%Y-%m-%d %H.%M %Z", localtime(time()))
print"<br>"
print"<hr>"
print"<address>"
print"<a href='mailto:tom_hickerson@hotmail.com'>tom_hickerson@hotmail.com</a>"
print"</address>"
print"</body>"
print"</html>"   