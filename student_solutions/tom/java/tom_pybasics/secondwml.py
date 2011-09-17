import Ns

conn = Ns.GetConn()

wml = '''<?xml version="1.0"?>
<!DOCTYPE wml PUBLIC "-//WAPFORUM//DTD WML 1.1//EN" "http://www.wapforum.org/DTD/wml_1.1.xml">
<wml>
	<head>
	<meta http-equiv="Cache-Control" content="max-age=0"/>
	</head>
	<card>
	 <do type="accept" label="Answer">
		<go href="http://10.11.0.117:8000/basics/firstwml.py" method="post"/>
		<!-- <postfield name="Items" value=$(Items)"/>
		</go>-->
	 </do>
	 <p>What is your email?
		<input name="Email"/></p>
	<p>What is your password?
		<input name="Password"/></p>

	</card>
	<card id="card2">
	
	</card>'''


<?xml version="1.0"?> 
<!DOCTYPE wml PUBLIC "-//WAPFORUM//DTD WML 1.1//EN" "http://www.wapforum.org/DTD/wml_1.1.xml">
    

<wml>

    <head>
    <meta http-equiv="Cache-Control" content="max-age=0"/>
    </head>
    <template>
    <do type="prev" label="Back">
    <prev/>
    </do>
    <do type="options" label="photo.net home">
    <go href="/wap/index.tcl"/>
    </do>
    </template>
  <card>
    <p>Top matches for "palm"</p>
    <p>
<a href="view-one.tcl?neighbor_to_neighbor_id=11726">Associated Camera Repair, Portland, Oregon : Incorrect Camera Meter Calibration</a><br/>
<a href="view-one.tcl?neighbor_to_neighbor_id=16846">Eckerd Photo Lab : Ruined one negative &#38; lost another!!!</a><br/>
<a href="view-one.tcl?neighbor_to_neighbor_id=29706">60 Minute Photo Lab, West Palm Beach, Fl : Dust Damage</a><br/>
<a href="view-one.tcl?neighbor_to_neighbor_id=19166">Sexton, John : John Sexton, "The Expressive Black and White Print"</a><br/>
<a href="view-one.tcl?neighbor_to_neighbor_id=53760">Joe Palma : Totally flaked/reneged on a done deal</a><br/>
<a href="view-one.tcl?neighbor_to_neighbor_id=84246">Jim Palmer : Excellent selling experience</a><br/>
<a href="view-one.tcl?neighbor_to_neighbor_id=100325">Yashica T4super : Magnify that waist level finder for less than  $$5.00</a><br/>
<a href="view-one.tcl?neighbor_to_neighbor_id=107413">Arthur Morris - Birds As Art : A review of a one-day Instructional Photo Tour (IP</a><br/>
<a href="view-one.tcl?neighbor_to_neighbor_id=107414">Arthur Morris - Birds As Art : A Review of a One-Day Instructional Photo Tour (IP</a><br/>
<a href="view-one.tcl?neighbor_to_neighbor_id=111324">Canon cameras : Gripes about the EOS-3</a><br/>

    </p>
  </card>
</wml>
