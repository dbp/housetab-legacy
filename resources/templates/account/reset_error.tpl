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
            <div id="subnav_reset"></div>
        	</div>
        	<div id="subnav3"><p>Could Not Reset Password</p></div>
        	<form><p>The reset was not successful. Check that the link was correct, or contact <a href="mailto:info@housetab.org">info@housetab.org</a>.</p></form>
        </div-async>


    </bind>
</apply>