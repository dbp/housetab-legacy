<apply template="base">

  <bind tag="heading">
    <link rel="stylesheet" type="text/css" href="/css/nav_mid.css" />
  </bind>

  <bind tag="above">
    	<div id="copy">
    	<img src="/img/Other/logo.png" />
    		<p>An application to track expenses among a group of friends that is simple and powerful</p>
    		<p>A way to never have to worry about who owes who money for groceries or a meal out.</p>
    		<p>A cost sharing method that allows flexibility, because different people have different abilities to pay.</p>
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
  	  <div id="navbarcontents2_lo">
  	  	<div id="subnav">
  	  	</div>
  	  	<div id="subnav3"><p>Log in Below</p></div>
  	  	<form method="POST" id="loginform">
  	  		<p>Account:</p><input type="text" name="accountName" class="logform" value="" />
  	  		<p>Password:</p><input type="password" name="password" class="logform" value="" />
  	  		<button type="submit" id="logform_submit"/>
  	  	</form>
  	  </div>
    </div>
  </bind>


</apply>