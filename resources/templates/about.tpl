<apply template="base">
 
 
  <bind tag="heading">
    <link rel="stylesheet" type="text/css" href="/css/nav_right.css" />
  </bind>
 
  <bind tag="top">
  	<div id="topcontents">
  		<h1>HOUSETAB</h1>
  		<img src="/img/Glyphs/glyph_above.png" />
  		<h3>a project of</h3>
  		<p>Position Studios</p>
  	</div>
  </bind>
 
  <bind tag="above">
  	<div id="abovecontents">
  		<p>Housetab.org is a site that allows groups of people to create collective 'house tabs', where they can record expenses that are shared by more than one person in the group, and flexibly make sure costs are being shared equitably. You can imagine the need for it if you live in a house (or organization) that buys groceries as a group, pay bills as a group, etc, and have to keep track of who owes who what.</p>
  	</div>
  </bind>
  
  <bind tag="navbar">
  		<div id="navbarcontents1">
  		  <ifLoggedIn>
  			  <a href="/settings" id="settings"></a>
  			  <a href="/entries" id="entries"></a>
  			  <div id="sel_about"></div>
  			  <a href="/logout" id="logout"></a>
        </ifLoggedIn>
        <ifGuest>
          <a href="/signup" id="signup"></a>
    			<a href="/login" id="login"></a>
    			<div id="sel_about"></div>
        </ifGuest>
  		</div>
  		<div id="navbarcontents2">
  		<div id="subnav">
  			<div id="subnav1"><p><a href="#top">What is HouseTab?</a></p></div>
  			<div id="subnav2"><p><a href="#below">Frequently Asked Questions</a></p></div> 
  			<div id="subnav_about"></div>
  		</div>
  			<div id="subnav3"></div>
  			<p id="contact">Contact us at <a href="mailto:info@housetab.org">info@housetab.org</a> | <a target="_blank" href="https://www.wepay.com/x1ggtpb">To donate click here.</a></p>
  		</div>
  </bind>
  
  <bind tag="below">
  		<div id="faq_head">
  			<h1>FREQUENTLY ASKED QUESTIONS</h1>
  			<img src="/img/Glyphs/glyph_below.png" />
  			<p><a href="#faq1">How does this work?</a> | <a href="#faq2">What about shares?</a> | <a href="#faq3">How are the totals calculated?</a> | <a href="#faq4">How do I pay someone back directly?</a> | <a href="#faq5">Isn't this really insecure?</a> | <a href="#faq6">I want to contribute!</a></p>
  		</div>

  		<div id="faq">
  		<h2 id="faq1"><img src="/img/Glyphs/acorn.png" />How does this work?</h2>
  			<p>The system focuses around the idea of people and entries. An entry is created marking some purchase or expense. It is noted who spent the money for it, how much it was, what it was, when it happened, and finally, who are the people who are using that expense. As a concrete example, if I went and bought groceries for the house and spent $85, I would record the entry as having been paid for by me, being groceries, purchased today, cost $85, and being split by all the people in the house.</p>

  			<p>A person has a name, and a series of shares. The shares are associated with dates. In the most simple example, when everyone is splitting everything evenly, in a house of five people, everyone could be marked down at 20 shares, as of some date before their first entry. The total of the shares does not have to add to 100, but it is often easier to reason about if it is.</p>

  		<h2 id="faq2"><img src="/img/Glyphs/acorn.png" />What about shares?</h2>
  			<p>Each entry is split among all the people using it based on their relative shares. We get the share of each person as of the date it was recorded. ie, if I have 20 on January 1st of this year, and then 25 as of March 1st, if an entry is recorded as having been purchased on February 15th, then I will be covering 20 out of whatever the total shares for all the people covering it was, whereas one from April 12th would have me covering 25 out of the total.</p>
  			<p>Now, if you’ve been diligent and made it so all the shares add up to 100, you may wonder why we bother with this funny language (shares instead of percents). If I have a housetab with four people on it, each covering 25 shares, if an item is only split among two, it adds those two shares (getting 50) and then calculates a relative percentage of 50% for each of the two people (as 25 is half of the sum of the two shares, 50). So while it seemed like each person was at 25 shares, or 25%, obviously if only two people are splitting something, they should not be covering only 25% of it respectively. </p>
  			<p>So if you want to get your shares to add to 100, that’s fine, but don’t worry about it if the numbers work out easier in some other way. On the settings page you can see the total shares, people’s current shares, and their current percentage (out of an expense shared by all). On the entries page, we just show the current percentage out of the total, as that will probably be the most relevant thing to see at a glance.</p>
  		
  		<h2 id="faq3"><img src="/img/Glyphs/acorn.png" />How are the totals calculated?</h2>
  			<p>To calculate totals, the program runs through all the entries, splitting them among people based on their relative percentages as of the specific dates they occurred on (this is because people’s shares can change over time, up to once per day, though that’d probably be excessive), coming up with a total that each person should have spent. This will most likely not be what they actually spent, so that is calculated next (by adding up all the entries that were paid for by that person), and the difference is what is displayed on the balance section of the page; positive means that you have spent more than you should of (so you are “up”), negative means you have spent less than you should have (so you are “down”). </p>

  			
  		<h2 id="faq4"><img src="/img/Glyphs/acorn.png" />How do I pay someone back directly?</h2>

  			<p>You can record one person directly paying another by putting the payer as the person who bought the item (the payback) and the payee as the ONLY splitter. We’ve included the category “Cash” for this purpose. A little less intuitively, but still easy, you can do the opposite as well - if someone does me a favor (like lets me use their car), I can give them some money for gas by putting them as the buyer and me as the only splitter, as I am acting as though they directly paid me (though the “payment” is pretty ephemeral).</p> 
  			<p>Both of these work not because of some special rules, but based on the same calculations described in the FAQ <a href="#faq3">“How are the totals calculated”</a> - the thing to keep in mind (if you are trying to convince yourself that it works) is that it the case of only one User, the shares are irrelevant (ie, 10 shares / 10 shares is 100% just like 30 shares / 30 shares is).</p>

  		<h2 id="faq5"><img src="/img/Glyphs/acorn.png" />Isn't this really insecure?</h2>

  			<p>This is a service only to be used by people who trust each other! It is secure in the context of only people who have the login for your account can get in and change things, but if people are logged in, they can do whatever they want! If you are engaged in collective expense sharing, at the bare minimum you should be trusting each other to not try to rip one another off.</p>
  			
  			<p>With that said, by default all actions are recorded, and you can see those on the settings page. This is designed to prevent accidents (ie, you accidentally deleted an entry, and wanted to see what it was), but you can use it as an audit trail of sorts. However, we give you the power to wipe out that entire record, because we don’t believe in not letting you opt out of those kinds of things. </p>
  			
  			<p>There is no way of temporarily disabling the records, so if you suspect that something is going on, and you look at the settings page, and notice that the record has been wiped, it might be good to sit down with the people you’re doing this with and figure out what is going on. A housetab is meant to save time and effort and increase flexibility, NOT enforce social/moral codes.</p>

  		<h2 id="faq6"><img src="/img/Glyphs/acorn.png" />I want to contribute, how can I help?</h2>

  			<p>This is open source software written in the purely functional programming language Haskell with the framework <a href="http://snapframework.com" target="_blank">Snap</a> and the <a href="http://mongodb.org">MongoDB</a> database - feel free to grab the <a href="http://darcsden.com/position/housetab">source</a>, make changes and send patches to <a href="mailto:daniel@positionstudios.com">daniel@positionstudios.com</a>, or the committer email address, and I'll integrate them in if they are useful. </p>

  			<p>If you are not a programmer, you can also support the project by <a href="https://www.wepay.com/donate/99127" targen="_blank">donating</a>.</p>

  		</div>

  </bind>

    
</apply>
