<div-async name="result-table" id="payments-table">
  <result>
    <div class="person $(personClasses)">
      <show blank="$(personId)">
        <tutorial step="2">
          <div id="tutorial-2">
            2
          </div>
        </tutorial>
	      <h2><personName/><!-- (<a href="/people/edit/$(personId)">edit</a>) --></h2>
        <p>spent <personSpent/></p>
        <p><personShare/>%</p>
        <p class="balance"><personOwes/></p>
      </show>
    </div>
  </result>
</div-async>