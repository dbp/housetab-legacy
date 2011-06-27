<apply template="base">

  <bind tag="heading">
    <link rel="stylesheet" type="text/css" href="/css/nav_left.css" />
  </bind>

  <bind tag="top">
    <tutorial step="1">
      <apply template="tutorial/1"></apply>
    </tutorial>
	  <tutorial step="2">
      <apply template="tutorial/2"></apply>
    </tutorial>
	  
	  <h1>USER SETTINGS</h1>
		<div id="totalshares">
			<img src="/img/Glyphs/glyph_above.png" />
			<h3>total shares</h3>
			<p>NNN</p>
		</div>
		
    <apply template="people/add"></apply>
  </bind>


  <bind tag="above">
		<apply template="people/settings"></apply>
  </bind>

  <bind tag="navbar">
    <div id="navbarcontents1">
			<div id="sel_settings"></div>
			<a href="/entries" id="entries"></a>
			<a href="/about" id="about"></a>
			<a href="/logout" id="logout"></a>
		</div>
		<div id="navbarcontents2">
			<div id="subnav">
				<div id="subnav_settings"></div>
				<div id="subnav1"><p><a href="#top">See User Settings</a></p></div>
				<div id="subnav2"><p><a href="#below">See Account History</a> | <a href="#below">Delete Account</a></p></div> 
			</div>
			<div id="subnav3"><p>Update Account Settings</p></div>
			<form action="">
				<p>Old Password:</p><input type="text" name="fname" class="setform" value="" />
				<p>New Password:</p><input type="text" name="lname" class="setform" value="" />
				<p>Contact Email:</p><input type="text" name="lname" class="setform" value="" />
				<input type="image" src="/img/Icons/submit_b.png" name="lname" value="" />
			</form>
		</div>
		
  </bind>
  
  <bind tag="below">
    <div id="belowtop">
			<div id="bt1">
			</div>
			
			<div id="bt2">
				<h1>LIST OF CHANGES</h1>
				<img src="/img/Glyphs/glyph_below.png" />
				<h3>for the account of</h3>
				<p><accountName/></p>
			</div>
			
			<div id="bt3">
				<h3>today's date is</h3>
				<p><currentDateLong/></p>
			</div>
		</div>

    <apply template="history/all"></apply>

  </bind>
</apply>