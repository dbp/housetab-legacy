<apply template="base">


  <bind tag="heading">
    <link rel="stylesheet" type="text/css" href="/css/nav_mid.css" />
  </bind>

  <bind tag="top">
    
    <h1>BALANCE OF PAYMENTS</h1>
		<img src="/img/Glyphs/glyph_above.png" />
  	<h3>total spent</h3>
  	<p><totalSpent/></p>
  </bind>
  
  <bind tag="above">
		<apply template="people/result"></apply>
  </bind>

  <bind tag="navbar">
    <div id="navbarcontents1">
  		<a href="/settings" id="settings"></a>
			<div id="sel_entries"></div>
  		<a href="/about" id="about"></a>
  		<a href="/logout" id="logout"></a>
  	</div>
  	<div id="navbarcontents2">
			<div id="subnav">
				<div id="subnav1"><p><a href="#top">See the Balance of Payments</a></p></div>
				<div id="subnav_entries"></div>
				<div id="subnav2"><p><a href="#below">See the List of Purchases</a></p></div> 
			</div>
  		<div id="subnav3"><p>Add a New Entry Below</p></div>
  		<apply template="entries/add"></apply>
  	</div>
  </bind>
  
  <bind tag="below">
    <div id="belowtop">
			<div id="bt1">
			</div>
			
			<div id="bt2">
				<h1>LIST OF PURCHASES</h1>
				<img src="/img/Glyphs/glyph_below.png" />
				<h3>for the account of</h3>
				<p><accountName/></p>
			</div>
			
			<div id="bt3">
				<h3>today's date is</h3>
				<p><currentDateLong/></p>
			</div>
		</div>

    <apply template="entries/all"></apply>

  </bind>
</apply>