<html>
  <head>
    <title>HouseTab</title>
    <!-- <script type="text/javascript" charset="utf-8" src="/valentine.min.js"></script>
    <script type="text/javascript" charset="utf-8" src="/reqwest.min.js"></script>
      <script type="text/javascript" charset="utf-8" src="/qwery.min.js"></script>
       <script type="text/javascript" charset="utf-8" src="/heist-async.min.js"></script> -->
    <activate-async/>
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