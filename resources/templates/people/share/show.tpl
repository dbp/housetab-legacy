<div-async name="settings-$(personId)-shares" class="shares-box showing">
  <a-async href="/people/$(personId)/shares/hide">hide shares</a-async>
  <h5>add new share</h5>
  <apply template="add"></apply>
  <h5>all shares</h5>
  <personShares>
    <div class="share"><a-async href="/people/$(personId)/shares/delete/$(date)">x</a-async> <value/> as of <date/></div>
  </personShares>
</div-async>
