<div-async name="change-settings">
<form-async action="/settings/update" method="POST" class="settings-form">
  
<div class="field">  
  <p>Current Password:</p>
  <div class="errors"><current-errors><error/> </current-errors></div>
  <input type="password" name="current" class="setform" value="" />
</div>  

<div class="field">
  <p>New:</p>
  <div class="errors"><password-errors><error/> </password-errors></div>
  <input type="password" name="password" class="setform" value="" />
</div>  

<div class="field">
  <p>Confirm:</p>
  <div class="errors"><confirm-errors><error/> </confirm-errors></div>
  <input type="password" name="confirm" class="setform" value="" />
</div>  

<div class="field">
  <p>Contact Email:</p>
  <div class="errors"><email-errors><error/> </email-errors></div>
	<input type="text" name="email" class="setform" value="$(email-value)" />
</div>
	
	<input type="image" src="/img/Icons/submit_b.png" name="" value="" />
</form-async>
</div-async>