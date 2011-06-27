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
  			<p id="contact">Contact us at <a href="">info@housetab.org</a> | <a target="_blank" href="https://www.wepay.com/x1ggtpb">To donate click here.</a></p>
  		</div>
  </bind>
  
  <bind tag="below">
  		<div id="faq_head">
  			<h1>FREQUENTLY ASKED QUESTIONS</h1>
  			<img src="/img/Glyphs/glyph_below.png" />
  			<p><a href="#faq1">Where did the idea come from?</a> | <a href="#faq2">How does it work?</a> | <a href="#faq3">What is on the pages and how do I change stuff?</a> | <a href="#faq4">Is this really secure?</a> | <a href="#faq5">Can I change a person's name or letter?</a> | <a href="#faq6">I want to contribute!</a></p>
  		</div>

  		<div id="faq">
  		<h2 id="faq1"><img src="/img/Glyphs/acorn.png" />Where did the idea of 'housetab's come from?</h2>
  			<p>Housetab.org is a site that allows groups of people to create collective 'house tabs', where they can record expenses that are shared by more than one person in the group, and flexibly make sure costs are being shared equitably. You can imagine the need for it if you live in a house (or organization) that buys groceries as a group, pay bills as a group, etc, and have to keep track of who owes who what.</p>

  			<p>It's first iteration was as an online spreadsheet where entries were recorded under different people's names, and some (semi-complicated) spreadsheet equations came up with totals and highlighted who was "up" and who was "down". This was used for months, but eventually began stretched in terms of it's flexibility - some items were not shared among the entire group (for example, a meal that only two people ate), and we wanted to be able to have certain people have higher or lower percentages of the whole split (as some people are able to easily make more money than others, and some people have more unavoidable expenses).</p>

  			<p>So the next evolution was one where the spreadsheet was only the location of the data, and a script was used to process an exported CSV (comma separated value) file. This allowed much more complicated semantics of processing, but unfortunately took away one of the best features of the spreadsheet - that the totals could be viewed online by anyone at any time (as the programmer of the group, the script was created by me and lived on my computer). So to make it more convenient, an online version of the House Tab was born, which used, virtually unchanged, the earlier script as it's backend (now living in Lib.hs, if you want to check it out).</p>

  			<p>After fine tuning this and using it for a month or two, the final iteration happened - making it so that multiple accounts could be created - so that anyone could create a HouseTab and start using it to coordinate their own collective expenses.</p>
  		<h2 id="faq2"><img src="/img/Glyphs/acorn.png" />How does it work?</h2>
  			<p>The system focuses around the idea of people and entries. An entry is created marking some purchase or expense. It is noted who spent the money for it, how much it was, what it was, when it happened, and finally, who are the people who are splitting that expense. As a concrete example, if I went and bought groceries for the house and spent $85, I would record the entry as having been paid for by me, being groceries, purchased today, cost $85, and being split by all the people in the house.</p>
  			<p>A person has a name, an initial (this could often be the first letter of a name) to make entering the data quicker, and a series of percentages. The percentages are associated with dates. In the most simple example, when everyone is splitting everything evenly, in a house of five people, everyone would be marked down at 20%. If they never decided to change it, all they would have was the date when they opened the tab (or any time before that) and the number 20.</p>
  			<p>One point of flexibility comes from these percentages. Each entry has it's percentage calculated by getting the percentage of each person splitting it as of the date it was recorded. Ie, if I have 20% on January 1st of this year, and then 25% as of March 1st, if an entry is recorded as having been purchased on February 15th, then I will be covering 20% of it, whereas one from April 12th would have me covering 25%.</p>
  			<p>The reality is actually a little more complicated, but if you aren't interested in knowing exactly how, it should act just like you think it would. If you like math, you'll probably have noticed that if only two people are splitting something, then one is probably not covering 25% of it, but more like 50%. The way it works is it adds all the percentages of the people covering an entry up (as of the date of the entry), and then portions out parts of it based on what relative percentage one would be spending.</p>
  			<p>This means that if I have a housetab with four people on it, each covering 25%, then if an item is only split among two, it adds those two percentages (getting 50%) and then calculates a relative percentage of 50% for each of the two people (as 25% is half of the sum of the two percentages, 50%). This also exposes another slight complication, or simplification, depending on how you see it - the total percentages in a tab do not need to sum to 100% - because ALL splits are calculated relative to the local total. So if all of our percentages add to 150% that is fine - our numbers are just relative to that number. If you want to go further from that (and are mathematically inclined, like I am), these aren't percentages at all, they are just relative fractions. It is just usually easier to think about things in terms of percentages, and try to get them all to add to 100%, as it makes things easier to figure out.</p>
  			<p>So to calculate totals, the program runs through all the entries, splitting them among people based on their relative percentages as of the specific dates they occurred on, coming up with a total that each person should have spent. This will most likely not be what they actually spent, so that is calculated next (by adding up all the entries with the paid for set to that person), and the difference is what is displayed on the front page, color coded based on whether you have spent more than your calculated share or less (red means you are down, green means you are up, sorry to red-green color blind folks - it was the simplest/most clear way of communicating it I could think of).</p>
  			<p>One more point of flexibility - you can actually record one person directly paying another by putting the payer as the person who bought the item (the payback) and the payee as the ONLY splitter. The opposite is also true - if someone does me a favor (like lets me use their car), I can give them some money for gas by putting them as the buyer and me as the only splitter, as I am acting as though they directly paid me. It's a little complicated how this works, but should make sense (and you can check that it works - the payer gets more in debt/less up, the payee gets less in debt/more up.)</p>
  		<h2 id="faq3"><img src="/img/Glyphs/acorn.png" />What's on the pages, and how do I change stuff?</h2>
  			<p>Once you've got it all set up, you shouldn't ever have to move from the entries page - it has buttons to add, edit, or delete entries, and has totals at the top. But, at the beginning that top line is going to be important - the white plus (that turns blue when you mouseover) allows you to add a new person to the account. This should be pretty straightforward, and then they should show up on the list at the top of the page, next to the white plus, colored green because they don't owe anything.</p>

  			<p>Clicking on their box should bring you to a page where you can add percents and also see all the purchases that they have made. Add a percent, giving the date you want it to be effective as of (could be today, or if you are going to add in expenses before today, put the date back before the first expense). Then head back to the entries page via the menu at the top, and add more people or start recording expenses.</p>

  			<p>Note: when adding entries, you only use the initials of people to refer to them, and you cannot add an initial of someone who is not yet on the house tab. So add all the people first, and then start adding expenses (you could also record the expenses first without them listed in the splitters and add them in later, but that'd probably be a lot more work).</p>

  			<p>Under the settings tab at the top you can change the password on the account, or change the contact email (which is used in case you forget your password). You can also delete the account (permanently - this is not reversable), and you will notice a link that says "View export dump" and "Restore from dump".</p>

  			<p>This service is very simple, and it puts the data in your hands - we do not make backups of your data to protect you from deleting your information (though in the case of a server error we can restore what the system was like before the error - which is a different thing) - but we make it very easy for you to make backups yourself, in a way that should be quite easy to bring it into another program or other data format.</p>

  			<p>Clicking on the "View export dump" will yield a page that has all the data from the HouseTab (except your username and password, of course, as that is safely stored and hashed so even we can't see it), wrapped in a bunch of brackets and parenthesis and quotation markes. Copy and paste this into a document and save it. I have a folder where I have copies of this in text files named after the dates they were made - is, 2010.4.10.txt, 2010.4.20.txt, etc. I may try to make a url that is plain text to make it easier to script this backup procedure, but for now it is a copy and paste activity.</p>

  			<p>Note for programmers - this format is made such that the string can be directly Read by haskell code into a set of lists and tuples - look at Backup.hs if you want to see how you can parse this back into the native format of the program and as guidance to parsing it into any other format.</p>

  			<p>To restore from a dump, go to the "Restore from dump" page and paste in exactly what you copied from the export page. This is very dangerous and made available because we trust you to manage your own data - this gives you flexibility to take all your data with you, and bring it all back later (or if you want to install a non-shared version of HouseTab, the earlier, one account version is available, and you can paste in your export to that). But, if you paste in the wrong data you risk erasing your entire tab (there is some error checking to make it harder to do this, but don't trust it!).</p>

  			<p>On the front page, each person's name has the total amount they spent at the top, next to their name, and their current percent (this is as of the most recent entry in the tab) below the amount they are up or owe. The black square on the right, where you can add people with the white plus, has the total that has been spent above the plus and the total percent as of the most recent purchase.</p>

  			<p>This is so it is easy to figure out what your relative percentage is, as you are covering the number under your name out of the total number listed below the white plus. If you have calculated properly this should be 100%, but there is no need for it to be! Just know that if you have 25% under your name and it has the total split at 75%, you are actually covering 1/3, not 1/4. The reason why it doesn't matter is because relative to the other people you are still covering the same - because two other people who also had 25% would be covering the same 1/3 as you were.</p>
  		<h2 id="faq4"><img src="/img/Glyphs/acorn.png" />Isn't this really insecure?</h2>

  			<p>This is a service only to be used by people who trust each other! It is secure in the context of only people who have the login for your account can get in and change things, but if people are logged in, they can do whatever they want! If you are engaged in collective expense sharing, at the bare minimum you should be trusting each other to not try to rip one another off.</p>

  			<p>Of course, you can also make backups after you record entries, and could compare those later to see if someone is changing stuff (shouldn't be hard to do if you can do a little programming), but this is definitely not the design of the system. A housetab is meant to save time and effort and increase flexibility, NOT enforce social/moral codes.</p>

  		<h2 id="faq5"><img src="/img/Glyphs/acorn.png" />Can I change a person's name or letter?</h2>

  			<p>Once you create a person their details (letter and name) are fixed. This is because all the data depends on the actual letter, and so if you changed their letter suddenly all the entries with their letter listed would be no longer valid. If you really want to do it, make a backup export (via /youraccount/backup/export) and then manually change your name/letter every time it appears, and then re-import it (via /youraccount/backup/import). Alternately, you can create a new person with the new name/letter and then edit all the entries they are included in and change the letter there, and then delete the original person.</p>

  		<h2 id="faq6"><img src="/img/Glyphs/acorn.png" />I want to contribute, how can I help?</h2>

  			<p>This is open source software written in the purely functional programming language Haskell with the framework Snap - feel free to grab the source, make changes and send patches to daniel@positionstudios.com, or the committer email address, and I'll integrate them in if they are useful. Alternatively, you could grab the source, install it on your own server, and start your own site!</p>

  			<p>If you are not a programmer, you can also support the project by donating:</p>


  		</div>

  </bind>

    
</apply>
