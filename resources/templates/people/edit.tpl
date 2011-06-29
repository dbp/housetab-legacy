<div-async name="edit-person-$(id-value)">
  <form-async action="/people/edit/$(id-value)" method="POST">
    <name-errors><error/><br></name-errors>
    <input name="name" type="text" value="$(name-value)" />
        <button type="submit" title=""/>
  </form-async>
</div-async>
