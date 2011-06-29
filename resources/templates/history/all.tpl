<div-async name="purchases"  id="history">	

  <historyOn>
    <more-box class="delete-history">
      Delete ALL History and Stop Recording It.
      <more>
        Are you sure you want to do this? You cannot undo this (though you can turn the recording back on, but only for new actions).
        <br><br>
        <a-async href="/history/deactivate">Yes</a-async>
      </more>
    </more-box>
  </historyOn>
  <historyOff>
    <a-async href="/history/activate">Turn on History Recording</a-async>
  </historyOff>
  
  <div class="clearfix"/>

  <div class="heading">
		<div class="col0 col">Time</div>
		<div class="col1 col">Category</div>
  	<div class="col2 col"></div>
  	<div class="col3 col">Buyer</div>
  	<div class="col4 col">Purchase</div>
  	<div class="col5 col">Cost</div>
  	<div class="col6 col">Date</div>
  	<div class="col7 col">Users</div>
  </div>
  	
  	<apply template="page"></apply>
    
    <div class="clearfix"/>
</div-async>