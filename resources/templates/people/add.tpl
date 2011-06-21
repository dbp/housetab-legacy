<div-async name="add-person" class="addPerson">
  <form-async action="/people/add" method="POST">
    <name-errors><error/> </name-errors>
    <label for="name">Name:</label>
    <input name="name" type="text" value="$(name-value)" />
    <button type="submit" title=""/>
  </form-async>
</div-async>
