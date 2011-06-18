<apply template="base">

    <result>
      <a href="/people/edit/$(personLetter)">edit</a>: Name: <personName/>, Letter: <personLetter/>, Spent: <personSpent/>, Owes: <personOwes/><br>
    </result>
    <a-async href="/people/add">add a person</a-async><br><br>
    <a-async href="/entries/add">add an entry</a-async><br><br>
    
    <div-async name="add-entry"></div-async>
    <div-async name="add-person"></div-async>
    
    
    <entries>
      <a href="/entries/edit/$(index)">edit</a>, <a href="/entries/delete/$(index)">delete</a>: By: <entryBy/>, For: <entryFor/>, Amount: <entryAmount/>, Date: <entryDate/>, What: <entryWhat/><br>
    </entries>  

</apply>