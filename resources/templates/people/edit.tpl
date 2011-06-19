<div-async name="edit-person">
  <h3>Edit Person</h3>
  <form-async action="/people/edit/$(id-value)" method="POST">
    <table>
      <tr><td colspan="2"><name-errors><error/><br></name-errors></td></tr>
      <tr>
        <td class="label"><label for="name">Name:</label></td> 
        <td><input name="name" type="text" value="$(name-value)" />
        <button type="submit" title=""/></td></tr>
    </table>
  </form-async>
</div-async>
