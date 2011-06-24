<div-async name="add-person" class="addPerson">
  <form-async action="/people/add" method="POST">
    <name-errors><error/> </name-errors>
    <label for="name">Add Person:</label>
    <input name="name" type="text" value="$(name-value)" />
    <button type="submit" title=""/>
  </form-async>
  
  <tutorial step="1">
    <div id="tutorial-1">
      1
    </div>
  </tutorial>
  <tutorial step="2">
    <div id="tutorial-2">
      2
    </div>
  </tutorial>
</div-async>

<tutorial step="2">
  <apply template="/tutorial/2"></apply>
</tutorial>