<div-async name="settings-$(personId)-shares" class="shares-box">
  <a-async href="/people/$(personId)/shares/hide">hide shares</a-async>

  <apply template="add"></apply>
  <personShares>
    <a-async href="/people/$(personId)/shares/delete/$(date)">x</a-async> <value/> as of <date/><br>
  </personShares>
</div-async>
