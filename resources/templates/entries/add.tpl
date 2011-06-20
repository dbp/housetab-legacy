<div-async name="entry-form">
  <h3>Add Entry</h3>
  <people/>
  <form-async action="/entries/add" method="POST">
    <table>
      <tr><td colspan="2"><by-errors><error/><br></by-errors></td></tr>
      <tr>
        <input type="hidden" name="id" value="$(id-value)" />
        <td class="label"><label for="by">By:</label></td> 
        <td>
          
          <box-field name="by" value="$(by-value)">
            <people>
              <box-option value="$(personId)">
                <personName/>
              </box-option>
            </people>
          </box-field>
          
          </td>
      </tr>
      
      <tr><td colspan="2"><for-errors><error/><br></for-errors></td></tr>
      <tr><td class="label">
        <label for="for">For:</label></td> 
        <td>
          <box-field-multi name="for" value="$(for-value)">
            <people>
              <box-option value="$(personId)">
                <personName/>
              </box-option>
            </people>
          </box-field-multi>
        </td></tr>
        
        <tr><td colspan="2"><ammount-errors><error/><br></ammount-errors></td></tr>
        <tr><td class="label">
          <label for="ammount">Ammount:</label></td> 
          <td><input name="ammount" type="text" value="$(ammount-value)" /></td></tr>
          
          <tr><td colspan="2"><what-errors><error/><br></what-errors></td></tr>
          <tr><td class="label">
            <label for="what">What:</label></td> 
            <td><input name="what" type="text" value="$(what-value)" /></td></tr>
            
            <tr><td colspan="2"><date-errors><error/><br></date-errors></td></tr>
            <tr><td class="label">
              <label for="date">Date:</label></td> 
              <td><input name="date" type="text" value="$(date-value)" />
        <button type="submit" title=""/></td></tr>
    </table>
  </form-async>
</div-async>
