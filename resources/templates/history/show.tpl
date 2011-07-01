<div class="purchase history-$(type)">
<div class="clearfix"/>

  <div class="col0 col">
    <date/>
  
  </div>

	<div class="col1 col">
	  <div class="icon $(type)" />
  </div>
	<div class="col2 col">
	  <show Add="$(type)" Delete="$(type)">
	    <bind tag="imgSrc">
        <catImage cat="$(category)" />
      </bind>
      <img src="$(imgSrc)"/>
    </show>
    <show Edit="$(type)">
      <bind tag="imgSrc">
        <catImage cat="$(category)" />
      </bind>
      <show blank="$(category-old)">
        <img src="$(imgSrc)"/>
      </show>
      <show notblank="$(category-old)">
	      <more-box>
          <img src="$(imgSrc)"/>
            <more>
              <bind tag="imgSrc">
                <catImage cat="$(category-old)" />
              </bind>
              <img src="$(imgSrc)"/>
            </more>
        </more-box>
      </show>
    </show>
	</div>
	<div class="col3 col $(who-class)">
	  <show Add="$(type)" Delete="$(type)">
	    <lookupName id="$(who)"/>
    </show>
    <show Edit="$(type)">
      <show blank="$(who-old)">
        <lookupName id="$(who)"/>
      </show>
      <show notblank="$(who-old)">
	      <more-box>
          <lookupName id="$(who)"/>
          <more>
            <lookupName id="$(who-old)"/>
          </more>
        </more-box>
      </show>
    </show>
  </div>
	<div class="col4 col $(what-class)">
	  <show Add="$(type)" Delete="$(type)">
	    <what/>
    </show>
    <show Edit="$(type)">
      <show blank="$(what-old)">
        <what/>
      </show>      
      <show notblank="$(what-old)">
        <more-box>
          <what/>
          <more>
            <what-old/>
          </more>
        </more-box>
      </show>
    </show>
  </div>
	<div class="col5 col $(ammount-class)">
	  <show Add="$(type)" Delete="$(type)">
	    <ammount/>
    </show>
    <show Edit="$(type)">
      <show blank="$(ammount-old)">
        <ammount/>
      </show>
      <show notblank="$(ammount-old)">
	      <more-box>
          <ammount/>
          <more>
            <ammount-old/>
          </more>
        </more-box>
      </show>
    </show>
  </div>
	<div class="col6 col $(when-class)">
	  <show Add="$(type)" Delete="$(type)">
	    <when/>
    </show>
    <show Edit="$(type)">
      <show blank="$(when-old)">
        <when/>
      </show>
      <show notblank="$(when-old)">
	      <more-box>
          <when/>
          <more>
            <when-old/>
          </more>
        </more-box>
      </show>
    </show>
	</div>
	<div class="col7 col $(for-class)">
	  <show Add="$(type)" Delete="$(type)">
      <more-box>
        <forSummary/>
	      <more>
          <for>
            <lookupName id="$(value)"/><br>
          </for>
        </more>
      </more-box>
    </show>
    <show Edit="$(type)">
      <more-box>
	      <forSummary/>
        <more>
          <show notblank="$(forSummary-old)">
            New:<br>
          </show>
          <for>
            <lookupName id="$(value)"/><br>
          </for>
          <show notblank="$(forSummary-old)">
            <br>
            Old:<br>
            <for-old>
              <lookupName id="$(value)"/><br>
            </for-old>
          </show>
        </more>
      </more-box>
    </show>
  </div>
</div>
