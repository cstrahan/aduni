import Ns


conn = Ns.GetConn()

wml_form ='''<?xml version="1.0"?>
<!DOCTYPE wml PUBLIC "-//WAPFORUM//DTD WML 1.1//EN" "http://www.wapforum.org/DTD/wml_1.1.xml">
	<wml>
	<head>
	<meta http-equiv="Cache-Control" content="max-age=0"/>
	</head>
<!--this is the first card-->
	<card>
	 <do type="accept" label="Answer">
		<go href="#card2"/>
	 </do>
	 <p>What is your name?
		<input name="Name"/></p>
	</card>
<!--this is the second card-->
<card id="card2">
	<do type="accept" label="Answer">
		<go href="#card3"/>
	</do>
	<p>What is your favorite color?
	<select name="Favorite">
	  <option value="red">Red</option>
	  <option value="blue">No, Blue!</option>
	</select></p>
</card>

<!--this is the third card>-->
<card id="card3">
	<p>Name: $(Name)<br/>
	Color: $(Favorite)</p>
<p>We're not in the 1970s anymore.</p>
</card>
</wml>'''



conn.ReturnData(200,wml_form,'text/vnd.wap.wml')
#print header
#print wml_form