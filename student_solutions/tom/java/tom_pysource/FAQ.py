########################
##FAQ.py
##Bryon Gill 4/28/01

import Ns
from displayify import displayify
conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

content = """
<h1>Frequently Asked Questions</h1>

<li>What is the purpose of this site?<br>
    The Secondary Sources web site exists to provide a central location for
    the gathering of caselist information for college-level policy debate.<p>

<li>How can I obtain posting priveleges?
    Only registered users from schools who participate in NDT/CEDA debate
    may post information to the list.  Each school has a liaison
    responsible for distributing these passwords to trusted users.  To find
    out who the liaison for your institution is, see the <a
    href=/project/liaisons.py>liaison listings</a> .  If your school is not
    listed there, contact the <a href=mailto:admin@secondarysources.com>site administrator</a>.<p>
    
<li>Can I use a WAP-enabled mobile phone to access your site?<BR>
    Why, we're so glad you asked!  The answer is yes- you can now check the latest
    updates to SecondarySources.com from the comfort of your own cell phone, whether
    you're stuck in traffic or roaming the library stacks ten minutes before closing
    time.  No more need for clunky computers- WAP is here!<p>

<li>How can I change my user information?<br>
    If there is a problem with your user information, please contact the <a href=mailto:admin@secondarysources.com>site administrator</a>.<p>

<li>Do I have to give my email address?<BR>
    You can read the entire site without ever logging in, but if you'd like to post to the site, 
    the answer is yes.  If you have concerns about giving out your normal email address, we suggest obtaining
    an email address from one of the free web-based email services such as <a href=http://mail.yahoo.com>Yahoo</a>.<p>

<li>I am not a debater.  What is this site all about?<BR>
    Well, this site was designed for a specific purpose of interest mainly to the academic debate community.  If you are interested in
    learning more about debate, we suggest you try the <a href=http://debate.uvm.edu/learn2.html>Debate Central learning pages</a>.
    If you are just interested in learning about this year's debate topic, you may find some useful information in the links section on
    our <a href=/project/front.py>home page</a>.
<p>
<li>I am a high school/ld/parliamentary/other debater.  Where do I submit my caselist information?<BR>
    At this time secondarysources.com caters exclusively to the college policy debate community, but we may
    expand our service in the future.  If you have questions or suggestions, you may contact the <a href=mailto:admin@secondarysources.com>site administrator</a>.<p>
    """
html = displayify(content,db)
    
conn.ReturnHtml(200, html)









