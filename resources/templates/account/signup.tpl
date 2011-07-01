<apply template="base">

  <bind tag="heading">
    <link rel="stylesheet" type="text/css" href="/css/nav_left.css" />
  </bind>


  <bind tag="above">

  </bind>

  <bind tag="navbar">
  		<div id="navbarcontents1">
  			<div id="sel_signup"></div>
  			<a href="/login" id="login"></a>
  			<a href="/about" id="about"></a>
  		</div>
    <apply template="/account/signup_form"></apply>
  </bind>
  
</apply>