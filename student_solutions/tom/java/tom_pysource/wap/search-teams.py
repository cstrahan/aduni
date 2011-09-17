import Ns
import zz

conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

##### search-teams.py by Tom Hickerson 4/28/01
### search the wap database for teams, return the five
### last updated posts related to the search string.

wml_form = '''<?xml version="1.0"?> 
<!DOCTYPE wml PUBLIC "-//WAPFORUM//DTD WML 1.1//EN" "http://www.wapforum.org/DTD/wml_1.1.xml">
  
<wml>
<head>
<meta http-equiv="Cache-Control" content="max-age=0"/>
</head>

<template>
<do type="prev" label="Back">
<prev/>
</do>
<do type="options" label="Sec. Sources home">
<go href="index.py"/>
</do>
</template>

<card>
<p>Secondary Sources.com</p>
<p></p>
%s
</card>
</wml>'''

wap_form_contents = ''

insert_content = '''<do type="accept" label="Search">
<go href="search-teams.py" method="post">
<postfield name="textstring" value="$textstring"/></go>
</do>
<p>Search Teams: <input name="textstring"/>
</p>'''

result_content = '''<p>Team results</p><p>%s</p>'''

result_body = ''

result_line = '''
<p><anchor>%s %s<go href="one-post.py" method="post">
<postfield name="list_id" value="%s"/></go></anchor>
</p>
<p>%s</p>'''

wap_teams_sql = '''
SELECT
	s.school_name,
	t.team_name,
	c.list_id,
	c.title
FROM
	schools s, teams t, content c
WHERE
	(s.school_id = t.school_id
	and c.team_id = t.team_id)
	and (upper(t.team_name) like upper(\'%%%s%%\')
	or upper(s.school_name) like upper(\'%%%s%%\'))
ORDER BY c.date_posted
LIMIT 5;'''

wml = ''
str = ''

try:
	query = conn.GetQuery()
	str = query.Get('textstring')
except:
	str = ''	


if (str):
	wap_sql = wap_teams_sql % (str,str)
	try:
		results = db.Select(wap_sql)
		while db.GetRow(results) == Ns.OK:
			result_body = result_body + result_line % (results[0],results[1],results[2],results[3])
	except:
		result_body = insert_content
else:
	result_body = insert_content

wml = wml_form % (result_body)

conn.ReturnData(200,wml,'text/vnd.wap.wml')
del db

