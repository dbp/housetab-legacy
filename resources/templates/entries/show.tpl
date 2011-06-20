<div-async name="entry-$(index)">
  <a-async href="/entries/edit/$(index)">edit</a-async>, 
  <a-async href="/entries/delete/$(index)">delete</a-async>:
  <div-async name="delete-$(index)" style="display: inline;"></div-async>
  By: <lookupName id="$(entryBy)"/>, 
  For: <entryFor>
        <lookupName id="$(value)"/>,
      </entryFor> 
  Amount: <entryAmount/>, 
  Date: <entryDate/>, 
  What: <entryWhat/>
</div-async>
