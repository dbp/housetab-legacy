<div-async name="settings-$(personId)">
  <show blank="$(personId)">
    <tutorial step="2">
      <div id="tutorial-2">
        2
      </div>
    </tutorial>
    <h2>
      <apply template="name"></apply>
    </h2>
    <p>shares: <personCurrentShare/></p>
    <p><personCurrentPercent/>%</p>
    <apply template="share/hide"></apply>
  </show>
</div-async>