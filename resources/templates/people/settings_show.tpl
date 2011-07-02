<div-async name="settings-$(personId)" class="sub">
  <show notblank="$(personId)">
    <h2>
      <apply template="name"></apply>
    </h2>
    <p>shares: <personCurrentShare/></p>
    <p class="percent"><personCurrentPercent/>%</p>
    <apply template="share/hide"></apply>
  </show>
</div-async>
