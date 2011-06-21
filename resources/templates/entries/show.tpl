<tr-async name="entry-$(index)">
	<td class="col1">
	  <bind tag="imgSrc">
      <catImage cat="$(entryCategory)" />
    </bind>
    <img src="$(imgSrc)"/>
	  </td>
	<td class="col2">
    <a-async href="/entries/delete/$(index)" class="delete"></a-async>
	  <a-async href="/entries/edit/$(index)" class="edit"></a-async>
	  <div-async name="delete-$(index)" style="display: none;"></div-async>
	</td>
	<td class="col3"><lookupName id="$(entryBy)"/></td>
	<td class="col4"><entryWhat/></td>
	<td class="col5">$<entryAmount/></td>
	<td class="col6"><entryDate/></td>
	<td class="col7">
	  <entryFor>
      <lookupName id="$(value)"/>,
    </entryFor>
  </td>
</tr-async>
