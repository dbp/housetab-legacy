<div-async name="result-table" id="payments-table">
  <result>
    <div class="person $(personClasses)">
      <show blank="$(personId)">
	      <h2><personName/></h2>
        <p>spent <personSpent/></p>
        <p><personCurrentShare/>%</p>
        <p class="balance"><personOwes/></p>
      </show>
    </div>
  </result>
</div-async>