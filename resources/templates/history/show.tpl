<div class="purchase">
<div class="clearfix"/>

	<div class="col1 col">
	  <more-box>
      <date/>
      <show blank="$(date-old)">
         <more>
           <date-old/>
         </more>
       </show>
    </more-box>
  </div>
	<div class="col2 col">
	  <more-box>
	    <bind tag="imgSrc">
        <catImage cat="$(category)" />
      </bind>
      <img src="$(imgSrc)"/>
      <show blank="$(category-old)">
        <more>
          <bind tag="imgSrc">
            <catImage cat="$(category-old)" />
          </bind>
          <img src="$(imgSrc)"/>
        </more>
      </show>
    </more-box>
	</div>
	<div class="col3 col">
	  <more-box>
      <lookupName id="$(who)"/>
      <show blank="$(who-old)">
         <more>
           <lookupName id="$(who-old)"/>
         </more>
       </show>
    </more-box>
  </div>
	<div class="col4 col">
    <more-box>
      <what/>
      <show blank="$(what-old)">
         <more>
           <what-old/>
         </more>
       </show>
    </more-box>
  </div>
	<div class="col5 col">
	  <more-box>
      <ammount/>
      <show blank="$(ammount-old)">
         <more>
           <ammount-old/>
         </more>
       </show>
    </more-box>
  </div>
	<div class="col6 col">
	  <more-box>
      <when/>
      <show blank="$(when-old)">
         <more>
           <when-old/>
         </more>
       </show>
    </more-box>
	</div>
	<div class="col7 col">
	  <more-box>
	    <forSummary/>
      <more>
        <show blank="$(for-old)">
          Old:
        </show>
        <for-old>
          <lookupName id="$(value)"/><br>
        </for-old>
        <show blank="$(for-old)">
          New:
          <for>
            <lookupName id="$(value)"/><br>
          </for>
        </show>
      </more>
    </more-box>
  </div>
</div>
