<apply template="base">

    <result>
      <a href="/people/edit/$(personLetter)">edit</a>: Name: <personName/>, Letter: <personLetter/>, Spent: <personSpent/>, Owes: <personOwes/><br>
    </result>
    <a-async href="/people/add">add a person</a-async><br><br>
    <a-async href="/entries/add">add an entry</a-async><br><br>
    
    <div-async name="entry-form"></div-async>
    <div-async name="add-person"></div-async>
    
    
    <entries>
      <apply template="entries/show"></apply>
    </entries>  

</apply>