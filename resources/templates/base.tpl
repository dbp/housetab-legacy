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
    
    <link rel="stylesheet" type="text/css" href="/css/main.css" />
    <link rel="stylesheet" type="text/css" href="/css/history.css" />
    <link rel="stylesheet" type="text/css" href="/css/more-box.css" />
    <link rel="stylesheet" type="text/css" href="/css/box-field.css" />

    <link rel="stylesheet" type="text/css" href="/css/tutorial.css" />
    
    <heading/>
    
    </head>
  <body>
    

    <show blank="$(top)">
      <div id="top">
      	<div id="topcontents">
          <top/>
        </div>
      </div>
    </show>

    <show blank="$(above)">
      <div id="above">
        <ifLoggedIn>
      	<div id="abovecontents">
          <above/>
          <div class="clearfix"/>
        </div>
        </ifLoggedIn>
        <ifGuest>
          <div id="abovecontents">
            <above/>
            <div class="clearfix"/>
          </div>
        </ifGuest>
      </div>
    </show>
    
    <show blank="$(navbar)">
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
    
    <show blank="$(below)">
      <div id="below">
      	<div id="belowcontents">
      
          <below/>
      
          <div id="bottom">
            <p>a project of position studios | powered by <a href="http://snapframework.com" target="_blank">snap</a>. 
              grab the source: 
              <a href="http://darcsden.com/position/housetab" target="_blank">darcsden.com/position/housetab</a>
            </p>
          </div>
          
          <div id="arrows">
          	<a href="#top"><div id="arrow_up"></div></a>
          	<a href="#navbar"><div id="arrow_mid"></div></a>
          	<a href="#below"><div id="arrow_down"></div></a>
          </div>
      	</div>
      </div>
    </show>
  </body>
</html>