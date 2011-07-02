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
        <div id="subnav2b"><more-box class="delete-box">
			    Delete Account
			    <more>
			      Are you SURE you want to delete your account? This CANNOT be undone, and there will be NO way or recovering any of your information. <form method="POST" action="/account/delete"><button type="submit">Yes</button></form> <button class="close">No</button>
			    </more>
			  </more-box>
			  </div>
        <div id="subnav_settings"></div>
        <div id="subnav1"><p><a href="#top">See User Settings</a></p></div>
        <div id="subnav2"><p><a href="#below">History of Changes</a></p></div> 
        <apply template="settings/subnav1b"></apply>
			</div>
			<div id="subnav3"><p>Update Account Settings</p></div>
      <apply template="/account/change_settings"></apply>
		</div>
		
  </bind>
  
  <bind tag="below">
    <div id="belowtop">
			<div id="bt1b">
			</div>
			
			<div id="bt2">
				<h1>LIST OF CHANGES</h1>
				<img src="/img/Glyphs/glyph_below.png" />
				<h3>for the account of</h3>
				<p><accountName/></p>
			</div>
			
			<div id="bt3">
				<h3>today's date</h3>
				<p><currentDateLong/></p>
          <more-box class="delete-history delete-box">
  				<historyOn>
            Delete All History and Stop Recording.
          </historyOn>
          <historyOff>
            Turn on History Recording
          </historyOff>
            <more>
              Are you sure you want to do this? 
              <historyOn>You cannot undo this.</historyOn>
              <br><br>
              <historyOn>
                <a-async href="/history/deactivate">Yes</a-async>
              </historyOn>
              <historyOff>
                <a-async href="/history/activate">Yes</a-async>
              </historyOff>
              <button class="close">Cancel</button>
            </more>
          </more-box>
			</div>
		</div>

    <apply template="history/all"></apply>

  </bind>
</apply>