<apply template="base">


  <bind tag="heading">
    <link rel="stylesheet" type="text/css" href="/css/nav_mid.css" />
    <script type="text/javascript" src="/js/raphael-min.js"></script>
    <script type="text/javascript" src="/js/g.raphael-min.js"></script>
    <script type="text/javascript" src="/js/g.pie-min.js"></script>
    <category-data></category-data>
    <script type="text/javascript">
        window.onload = function() {
            // Creates canvas 640 × 480 at 10, 50
            var r = Raphael('category-chart');
            // Creates pie chart at with center at 320, 200,
            // radius 100 and data: [55, 20, 13, 32, 5, 1, 2]
            // Add legend to piechart.
            var pie = r.g.piechart(300, 240, 90, cat_data, {legend:cat_legend, legendpos: "west"});
r.text(320, 100, "Total By Category").attr({ font: "20px sans-serif" });
        }
    </script>
    
  </bind>

  <bind tag="top">
    <tutorial step="1">
      <apply template="tutorial/0"></apply>
    </tutorial>
    <tutorial step="4">
      <apply template="tutorial/4"></apply>
    </tutorial>
    <tutorial step="5">
      <apply template="tutorial/5"></apply>
    </tutorial>
    <tutorialOff>
      <apply template="tutorial/off"></apply>      
    </tutorialOff>  
	  
    
    <h1>BALANCE OF PAYMENTS</h1>
		<img src="/img/Glyphs/glyph_above.png" />
  	<h3>total spent</h3>
  	<p><totalSpent/></p>
  </bind>
  
  <bind tag="above">
		<apply template="people/result"></apply>
  </bind>

  <bind tag="navbar">
    <div id="navbarcontents1">
  		<a href="/settings#navbar" id="settings"></a>
			<div id="sel_entries"></div>
  		<a href="/about#navbar" id="about"></a>
  		<a href="/logout" id="logout"></a>
  	</div>
  	<div id="navbarcontents2">
			<div id="subnav">
				<div id="subnav1"><p><a href="#top">See the Balance of Payments</a></p></div>
				<div id="subnav_entries"></div>
				<div id="subnav2"><p><a href="#below">See the List of Purchases</a></p></div> 
			</div>
  		<div id="subnav3"><p>Add a New Entry Below</p></div>
  		<apply template="entries/add"></apply>
  	</div>
  </bind>
  
  <bind tag="below">
    <div id="belowtop">
			<div id="bt1">
			</div>
			
			<div id="bt2">
				<h1>CHARTS OF PURCHASES</h1>
				<img src="/img/Glyphs/glyph_below.png" />
				<h3>for the account of</h3>
				<p><accountName/></p>
			</div>
			
			<div id="bt3">
				<h3>today's date</h3>
				<p><currentDateLong/></p>
			</div>
		</div>

    <div id="category-chart"></div>

  </bind>
</apply>
