<apply template="base">

  <bind tag="heading">
    <link rel="stylesheet" type="text/css" href="/css/nav_mid.css" />
  </bind>

  <bind tag="above">
    
    	<div id="copy">
    	<img src="/img/Other/logo.png" />
    		<p>A simple and powerful way to track shared expenses.</p>
    		<p>A way to never worry who owes whom for groceries or a meal out.</p>
    		<p>A cost-sharing method that allows flexibility, because different people have different abilities to pay.</p>
    	</div>
    	<div id="marx">
    		<p>&#8220The form of wood, for instance, is altered, by making a table out of it. Yet, for all that, the table continues to be that common, everyday thing, wood. But, so soon as it steps forth as a commodity, it is changed into something transcendent. It not only stands with its feet on the ground, but, in relation to all other commodities, it stands on its head, and evolves out of its wooden brain grotesque ideas, far more wonderful than table-turning ever was.&#8221
    		</p>
    	</div>
  </bind>
  
  <bind tag="navbar">
  	<div id="navbarcontents_lo2">
  	  <div id="navbarcontents1_lo">
  	  	<a href="/signup" id="signup"></a>
  	  	<a href="/about" id="about_lo"></a>
  	  </div>
  	  <div-async name="login-box" id="navbarcontents2_lo">
  	  	<div id="subnav">
  	  	</div>
  	  	<div id="subnav3"><p>Log in Below</p></div>
  	  	<form method="POST" id="loginform">
  	  		<p>Account:</p><input type="text" name="accountName" class="logform" value="" />
  	  		<show notblank="$(failure)">
  	  		  <div class="errors">Incorrect username or password.</div>
          </show>
  	  		<p>Password:</p><input type="password" name="password" class="logform" value="" />
  	  		<button type="submit" class="b"/>    	  	
  	  	</form>
  	  	<p class="inline"><a-async href="/account/forgot">forgot password?</a-async></p>
  	  </div-async>
    </div>
  </bind>


</apply>