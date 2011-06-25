<div-async name="settings-$(personId)">
  <show blank="$(personId)">
    <tutorial step="2">
      <div id="tutorial-2">
        2
      </div>
    </tutorial>
    <h2><personName/> <a href="/people/edit/$(personId)">\/ edit</a></h2>
    <a-async href="/people/$(personId)/share/add">\/ add a share</a-async><br>
    <hasShares>
      <a-async href="/people/$(personId)/share/show">\/ show shares</a-async><br>	        
    </hasShares>
  </show>
</div-async>
