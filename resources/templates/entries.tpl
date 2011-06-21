<apply template="base">

  <bind tag="top">
    <h1>BALANCE OF PAYMENTS</h1>
  	<img src="/img/glyph_up.png" />
  	<h3>total spent</h3>
  	<p>$15,201.45</p>
  	<apply template="people/add"></apply>
  </bind>
  
  <bind tag="above">
    <div id="payments-table">
		  <result>
		    <div class="person $(personClasses)">
		      <h2><personName/> (<a href="/people/edit/$(personId)">edit</a>)</h2>
          <p>spent $<personSpent/></p>
          <p><personShare/>%</p>
          <p class="balance">$<personOwes/></p>
		    </div>
      </result>
    </div>		
  </bind>

  <bind tag="navbar">
    <div id="navbarcontents1">
  		<div id="settings"></div>
  		<div id="arch"></div>
  		<div id="about"></div>
  	</div>
  	<div id="navbarcontents2">
  		<div id="subnav1"><p><a href="#top">See the Balance of Payments</a></p></div>
  		<div id="entries"></div>
  		<div id="subnav2"><p><a href="#below">See the List of Purchases</a></p></div> 
  		<div id="subnav3"><p>Add a New Entry Below</p></div>
  		<apply template="entries/add"></apply>
  	</div>
  </bind>
  
  <bind tag="below">
    <div id="belowtop">
			<div id="mod1">
			</div>
			
			<div id="mod2">
				<h1>LIST OF PURCHASES</h1>
				<img src="/img/glyph_down.png" />
				<h3>for the account of</h3>
				<p>DurrutiColumn</p>
			</div>
			
			<div id="mod3">
				<h3>today's date is</h3>
				<p>16 January 2011</p>
			</div>
		</div>

    <apply template="entries/all"></apply>

  </bind>
</apply>