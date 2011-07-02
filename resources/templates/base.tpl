<html>
  <head>
    <title>HouseTab</title>
    <script type="text/javascript" charset="utf-8" src="/js/valentine.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/reqwest.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/qwery.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/heist-async.js"></script>
    
    <script type="text/javascript" charset="utf-8" src="/js/bean.min.js"></script>      
    <script type="text/javascript" charset="utf-8" src="/js/declarative.js"></script>
    
    <script type="text/javascript" charset="utf-8" src="/js/bonzo.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/housetab.js"></script>
    
    <!--     <script type="text/javascript" src="http://use.typekit.com/hbk2bjj.js"></script>
        <script type="text/javascript">try{Typekit.load();}catch(e){}</script> -->
    
    <link rel="stylesheet" type="text/css" href="/css/main.css" />
    <link rel="stylesheet" type="text/css" href="/css/history.css" />
    <link rel="stylesheet" type="text/css" href="/css/more-box.css" />
    <link rel="stylesheet" type="text/css" href="/css/box-field.css" />

    <link rel="stylesheet" type="text/css" href="/css/tutorial.css" />
    
    <heading/>
    
    </head>
  <body>
    

    <show notblank="$(top)">
      <div id="top">
      	<div id="topcontents">
          <top/>
        </div>
      </div>
    </show>

    <show notblank="$(above)">
      <div id="above">
        <ifLoggedIn>
      	<div id="abovecontents">
          <above/>
        </div>
        </ifLoggedIn>
        <ifGuest>
          <div id="abovecontents">
            <above/>
          </div>
        </ifGuest>
      </div>
      <div class="clearfix"/>
      
    </show>
    
    <show notblank="$(navbar)">
      <div id="navbar">
        <ifLoggedIn>
      	  <div id="navbarcontents">
            <navbar/>
          </div>
        </ifLoggedIn>
        <ifGuest>
          <div id="navbarcontents_lo1">
            <navbar/>
          </div>
        </ifGuest>
      </div>
    </show>
    
    <show notblank="$(below)">
      <div id="below">
      	<div id="belowcontents">
      
          <below/>
      
          <div id="bottom">
            <p>a project of <a href="http://positionstudios.com" target="_blank">position studios</a> | powered by <a href="http://snapframework.com" target="_blank">snap</a> |
              grab the  
              <a href="http://darcsden.com/position/housetab" target="_blank">source</a>
              | issues? <a href="http://darcsden.com/position/housetab/issues" target="_blank">file them here</a>
            </p>
            <br>
          </div>
          
          <div id="arrows">
          	<a href="#top"><div id="arrow_above"></div></a>
          	<a href="#navbar"><div id="arrow_mid"></div></a>
          	<a href="#below"><div id="arrow_below"></div></a>
          </div>
      	</div>
      </div>
    </show>
    
    <div id="preload" style="display: none;">
      <img src="/img/Glyphs/arrow_mid_mo.png"/>
      <img src="/img/Glyphs/arrow_above_mo.png"/>
      <img src="/img/Glyphs/arrow_below_mo.png"/>
      <img src="/img/Icons/submit_mo.png"/>
      <img src="/img/Icons/edit_mo.png"/>
      <img src="/img/Icons/delete_mo.png"/>
      <img src="/img/NavigationBar/nav_entries_mo.png"/>
      <img src="/img/NavigationBar/nav_login_mo.png"/>
      <img src="/img/NavigationBar/nav_about_mo.png"/>
      <img src="/img/NavigationBar/nav_logout_mo.png"/>
      <img src="/img/NavigationBar/nav_settings_mo.png"/>
      <img src="/img/NavigationBar/nav_signup_mo.png"/>
    </div>
    
  </body>
</html>