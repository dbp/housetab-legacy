<div-async name="entry-$(index)" class="purchase">
<div class="clearfix"/>

	<div class="col1 col">
	  <bind tag="imgSrc">
      <catImage cat="$(entryCategory)" />
    </bind>
    <img src="$(imgSrc)"/>
  </div>
	<div class="col2 col">
    <a-async href="/entries/delete/$(index)" class="delete"></a-async>
	  <a-async href="/entries/edit/$(index)" class="edit"></a-async>
	  <div-async name="delete-$(index)" style="display: none;"></div-async>
	</div>
	<div class="col3 col"><lookupName id="$(entryBy)"/></div>
	<div class="col4 col"><entryWhat/></div>
	<div class="col5 col">$<entryAmount/></div>
	<div class="col6 col"><entryDate/></div>
	<div class="col7 col">
	  <entryFor>
      <lookupName id="$(value)"/>,
    </entryFor>
  </div>
</div-async>
