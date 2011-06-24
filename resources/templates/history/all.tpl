<div-async name="purchases" id="purchases">	

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
  		<div class="col1">Time</div>
  		<div class="col2"></div>
  		<div class="col3">Buyer</div>
  		<div class="col4">Purchase</div>
  		<div class="col5">Cost</div>
  		<div class="col6">Date</div>
  		<div class="col7">Users</div>
    </div>
  	
  	<apply template="page"></apply>
    
    <div class="clearfix"/>
</div-async>