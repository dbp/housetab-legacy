<div-async name="entry-form" class="entry-form">
<tutorial step="4">
  <div id="tutorial-4">
  </div>
</tutorial>
  <form-async action="/entries/add" method="POST"> <!-- data-loading-div="#purchases" -->
  <input type="hidden" name="id" value="$(id-value)" />
  
  <div class="buyer enform1">
    <p>Buyer:</p>
    <div class="errors"><by-errors><error/> </by-errors></div>
    <bind tag="by-display">
	    <lookupName id="$(by-value)" />
    </bind>
    <box-field name="by" value="$(by-value)" display="$(by-display)" class="">
      <people>
        <box-option value="$(personId)">
          <personName/>
        </box-option>
      </people>
    </box-field>
  </div>
  
	<div class="purchase enform2">
	  <p>Purchase:</p>
    <div class="errors"><what-errors><error/> </what-errors></div>
    <input name="what" type="text" value="$(what-value)" class="" />
  </div>
  
	<div class="category enform3">
	  <p>Cat:</p>
	  <div class="errors"><category-errors><error/> </category-errors></div>
	  <bind tag="category-display">
	    <catName cat="$(category-value)" />
    </bind>
	  <box-field name="category" value="$(category-value)" display="$(category-display)" display-class="$(category-value)">
      <categories>
        <box-option value="$(cat)" id="$(cat)">
        <catName cat="$(cat)"/>
        </box-option>
      </categories>
    </box-field>
	</div>
	
	
	<div class="date enform4">
	  <p>Date:</p>
	  <div class="errors"><date-errors><error/> </date-errors></div>
    <input name="date" type="text" value="$(date-value)" class="" />
  </div>
	
	<div class="amount enform5">
	  <p>Cost:</p>
    <div class="errors"><ammount-errors><error/> </ammount-errors></div>
    <input name="ammount" type="text" value="$(ammount-value)" class="" />
  </div>
  
	<div class="users enform6">
	  <p>Users:</p>
    <div class="errors"><for-errors><error/></for-errors></div>
    <bind tag="peopleshow"><lookupPeopleShow value="$(for-value)"/></bind>
    <box-field-multi name="for" value="$(for-value)" display="$(peopleshow)" class="">
    <people>
      <box-option value="$(personId)">
        <personName/>
      </box-option><br>
    </people>
    </box-field-multi>
  </div>

	<button type="submit" class="enform_submit"/>
  </form-async>
</div-async>
