<apply template="base">

  <bind tag="top">
    <tutorial step="1">
      <apply template="tutorial/1"></apply>
    </tutorial>
	  <tutorial step="2">
      <apply template="tutorial/2"></apply>
    </tutorial>
	  
    
    <h1>ADD A NEW PERSON</h1>
  	<img src="/img/glyph_up.png" />
    <apply template="people/add"></apply>
  </bind>


  <bind tag="above">
		<apply template="people/settings"></apply>
  </bind>

  <bind tag="navbar">
    <div id="navbarcontents1">
  		<div id="settings"></div>
  		<div id="arch"></div>
  		<div id="about"></div>
  	</div>
  	<div id="navbarcontents2">
  	  <div id="subnav1"><p><a href="#top">See the Users</a></p></div>
  		<div id="entries"></div>
  		<div id="subnav2"><p><a href="#below">See the History of Actions</a></p></div> 
  		<div id="subnav3"><p>BLAH.</p></div>
  	</div>
  </bind>
  
  <bind tag="below">
    <div id="belowtop">
			<div id="mod1">
			</div>
			
			<div id="mod2">
				<h1>LIST OF CHANGES</h1>
				<img src="/img/glyph_down.png" />
				<h3>for the account of</h3>
				<p><accountName/></p>
			</div>
			
			<div id="mod3">
				<h3>today's date is</h3>
				<p><currentDateLong/></p>
			</div>
		</div>

    <apply template="history/all"></apply>

  </bind>
</apply>