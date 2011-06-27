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
	<div class="col4 col">
	  <more-box>
      <take n="35" val="$(entryWhat)" />
      <more>
        <entryWhat/>
      </more>
	  </more-box>
  </div>
	<div class="col5 col"><entryAmount/></div>
	<div class="col6 col"><entryDate/></div>
	<div class="col7 col">
	  <more-box>
	    <entryForSummary/>
      <more>
        <entryFor>
          <lookupName id="$(value)"/><br>
        </entryFor>
      </more>
    </more-box>
  </div>
</div-async>
