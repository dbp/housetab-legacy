<div-async name="settings-$(personId)-add-share">
  <form-async action="/people/$(personId)/shares/add" method="POST" class="add-share">
    <share-errors><error/><br></share-errors>
    <date-errors><error/><br></date-errors>
    <input name="share" type="text" value="$(share-value)"  class="value" title="value"/>
    as of<br>
    
    <input name="date" type="text" value="$(date-value)" class="date" title="date"/>
    <button type="submit" title=""/>
  </form-async>
  <div class="clearfix"/>
</div-async>