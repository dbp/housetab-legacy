<html>
  <head>
    <title>HouseTab</title>
    <script type="text/javascript" charset="utf-8" src="/js/valentine.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/reqwest.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/qwery.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/heist-async.js"></script>
    
    <script type="text/javascript" charset="utf-8" src="/js/bean.min.js"></script>      
    <script type="text/javascript" charset="utf-8" src="/js/declarative.js"></script>
    
    <script type="text/javascript" charset="utf-8" src="/js/bonzo.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/js/housetab.js"></script>
    
    <link rel="stylesheet" type="text/css" href="/css/main.css" />
    <link rel="stylesheet" type="text/css" href="/css/more-box.css" />

    <ifTutorial>
      <link rel="stylesheet" type="text/css" href="/css/tutorial.css" />
      <script type="text/javascript" charset="utf-8" src="/js/tutorial.js"></script>
    </ifTutorial>
    
    </head>
  <body>
    

    <div id="top">
    	<div id="topcontents">
    	  <ifTutorial>
          <apply template="tutorial/base"></apply>
        </ifTutorial>
        <top/>
      </div>
    </div>

    <div id="above">
    	<div id="abovecontents">
        <above/>
        <div class="clearfix"/>
      </div>
    </div>
    
    <div id="navbar">
    	<div id="navbarcontents">
        <navbar/>
      </div>
    </div>
    
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
  </body>
</html>