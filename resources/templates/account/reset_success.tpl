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



        <div-async name="reset-box" id="navbarcontents2">
        	<div id="subnav">
            <!-- <div id="subnav_signup"></div> -->
        	</div>
        	<div id="subnav3"><p>Successfully Reset Password</p></div>
        	<form><p>Please click on the login link above to log in.</p></form>
        </div-async>


    </bind>
</apply>

