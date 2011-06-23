<div-async name="entry-form" class="entry-form">
  <form-async action="/entries/add" method="POST"> <!-- data-loading-div="#purchases" -->
  <input type="hidden" name="id" value="$(id-value)" />
  
  <div class="buyer">
    <p>Buyer:</p>
    <div class="errors"><by-errors><error/> </by-errors></div>
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
  
	<div class="purchase">
	  <p>Purchase:</p>
    <div class="errors"><what-errors><error/> </what-errors></div>
    <input name="what" type="text" value="$(what-value)" class="inp" />
  </div>
  
	<div class="category">
	  <p>Cat:</p>
	  <div class="errors"><category-errors><error/> </category-errors></div>
	  <bind tag="category-display">
	    <catName cat="$(category-value)" />
    </bind>
	  <box-field name="category" value="$(category-value)" display="$(category-display)" class="inp">
      <categories>
        <box-option value="$(cat)" id="$(cat)">
        <catName cat="$(cat)"/>
        </box-option>
      </categories>
    </box-field>
	</div>
	
	
	<div class="date">
	  <p>Date:</p>
	  <div class="errors"><date-errors><error/> </date-errors></div>
    <input name="date" type="text" value="$(date-value)" class="inp" />
  </div>
	
	<div class="amount">
	  <p>Cost:</p>
    <div class="errors"><ammount-errors><error/> </ammount-errors></div>
    <input name="ammount" type="text" value="$(ammount-value)" class="inp" />
  </div>
  
	<div class="users">
	  <p>Users:</p>
    <div class="errors"><for-errors><error/></for-errors></div>
    <box-field-multi name="for" value="$(for-value)" display="$(for-value)" class="inp">
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
