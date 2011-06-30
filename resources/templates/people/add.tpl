<div-async name="add-person" class="addPerson" id="adduser">
  <form-async action="/people/add" method="POST">
    <name-errors><error/> </name-errors>
    <h2><label for="name">Add a new user:</label></h2>
    <input name="name" type="text" value="$(name-value)" />
    <button type="submit" title="" class="addform_submit" />
  </form-async>
  
  <tutorial step="1">
    <div id="tutorial-1">
    </div>
  </tutorial>
  <tutorial step="2">
    <div id="tutorial-2" class="b">
    </div>
  </tutorial>
</div-async>

<tutorial step="2">
  <apply template="/tutorial/2"></apply>
</tutorial>