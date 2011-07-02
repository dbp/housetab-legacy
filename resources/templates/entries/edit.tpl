<div-async name="entry-$(id-value)" class="purchase entry-form" id="entry-$(id-value)">
  <form-async action="/entries/edit/$(id-value)" method="POST" > <!-- data-loading-div="#entry-$(id-value)" -->
  <input type="hidden" name="id" value="$(id-value)" />
  <div class="clearfix"/>

  <div class="col1 col category">
	  <div class="errors"><category-errors><error/> </category-errors></div>
	  <bind tag="category-display">
	    <catName cat="$(category-value)" />
    </bind>
	  <box-field name="category" value="$(category-value)" display="" display-class="$(category-value)" class="inp">
      <categories>
        <box-option value="$(cat)" id="$(cat)">
        <catName cat="$(cat)" />
        </box-option><br>
      </categories>
    </box-field>
	</div>
		
    <div class="col2 col">
      <a-async href="/entries/delete/$(index)" class="delete"></a-async>
      <a-async href="/entries/edit/$(index)" class="edit"></a-async>
      <div-async name="delete-$(index)" style="display: none;"></div-async>
    </div>
    
  	<div class="col3 col">
  	  <bind tag="by-display">
  	    <lookupName id="$(by-value)" />
      </bind>
      <box-field name="by" value="$(by-value)" display="$(by-display)" class="inp">
        <people>
          <box-option value="$(personId)">
            <personName/>
          </box-option>
        </people>
      </box-field>
  	</div>
  	<div class="col4 col">
  	  <div class="errors"><what-errors><error/> </what-errors></div>
      <input name="what" type="text" value="$(what-value)" class="inp" />
  	</div>
  	<div class="col5 col">
  	  <div class="errors"><ammount-errors><error/> </ammount-errors></div>
      <input name="ammount" type="text" value="$(ammount-value)" class="inp" />
    </div>
  	<div class="col6 col">
  	  <div class="errors"><date-errors><error/> </date-errors></div>
      <input name="date" type="text" value="$(date-value)" class="inp" />
    </div>
  	<div class="col7 col">
  	  <bind tag="peopleshow"><lookupPeopleShow value="$(for-value)"/></bind>
      <div class="errors"><for-errors><error/></for-errors></div>
      <box-field-multi name="for" value="$(for-value)" display="$(peopleshow)" class="inp">
      <people>
        <box-option value="$(personId)">
          <personName/>
        </box-option>
      </people>
      </box-field-multi>
    </div>
  <button type="submit" title=""/>
  </form-async>
</div-async>
