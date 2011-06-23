<apply template="base">

  <bind tag="navbar">
    <div id="navbarcontents1">
  		<div id="settings"></div>
  		<div id="arch"></div>
  		<div id="about"></div>
  	</div>
  	<div id="navbarcontents2">
  	  <div id="subnav1"></div>
  		<div id="entries"></div>
  		<div id="subnav2"><p><a href="#below">See the History of Actions</a></p></div> 
  		<div id="subnav3"><p>Add a New Entry Below</p></div>
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
			</div>
		</div>

    <apply template="history/all"></apply>

  </bind>
</apply>