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
	  <tutorial step="3">
      <apply template="tutorial/3"></apply>
    </tutorial>
    <tutorialOff>
      <apply template="tutorial/off"></apply>      
    </tutorialOff>  
	  
	  <h1>USER SETTINGS</h1>
    <apply template="people/total_shares"></apply>
		
    <apply template="people/add"></apply>
  </bind>


  <bind tag="above">
		<apply template="people/settings"></apply>
    <apply template="/tutorial/3_bubble"></apply>
  </bind>

  <bind tag="navbar">
    <apply template="settings/navbar1"></apply>
		<div id="navbarcontents2">
			<div id="subnav">
				<div id="subnav_settings"></div>
				<div id="subnav1"><p><a href="#top">See User Settings</a> | </p>
				  <more-box>
				    Delete Account
				    <more>
				      Are you SURE you want to delete your account? This CANNOT be undone, and there will be NO way or recovering any of your information. <form method="POST" action="/account/delete"><button type="submit">Yes</button></form> <button class="close">No</button>
				    </more>
				  </more-box>
				  </div>
          <apply template="settings/subnav2"></apply>
			</div>
			<div id="subnav3"><p>Update Account Settings</p></div>
      <apply template="/account/change_settings"></apply>
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