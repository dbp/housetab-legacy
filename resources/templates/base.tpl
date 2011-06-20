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
    
       <!-- <script type="text/javascript" src="/js/jquery-1.6.1.min.js"></script>
       <script type="text/javascript" src="/js/jquery.tokeninput.js"></script>
       
       <link rel="stylesheet" href="/css/token-input.css" type="text/css" media="screen" title="no title" charset="utf-8">
        -->
    <!-- <activate-async/> -->
    </head>
  <body>
    
    <activate-async/>
    
    <div id="navigations">
      <ifLoggedIn>
        <a href="/logout">logout</a> | <a href="entries">entries</a>
      </ifLoggedIn>
      <ifGuest>
         <a href="login">login</a> | <a href="signup">signup</a>
      </ifGuest>
    </div> <!-- #navigation -->
    
    <div id="notification">
      <notification />
    </div> <!-- #notification -->
    
    <content />
  </body>
</html>