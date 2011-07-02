<div-async name="entry-$(index)" class="purchase">
<div class="clearfix"/>

	<div class="col1 col">
	  <bind tag="imgSrc">
      <catImage cat="$(entryCategory)" />
    </bind>
    <img src="$(imgSrc)" alt="$(entryCategory)" title="$(entryCategory)"/>
  </div>
	<div class="col2 col">
	  <more-box class="delete-box">
	    <div class="delete"/>
	    <more>Are you sure you want to delete this entry?
	      <form-async action="/entries/delete/$(index)" method="POST">
          <button type="submit" title="">Delete</button>
        </form-async>
        <button class="close">Cancel</button>
      </more>
    </more-box>
    <!-- <a-async href="/entries/delete/$(index)" class="delete"></a-async> -->
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
