<html>
  <head>
    <title>Entries</title>
  </head>
  <body>
    <a href="/logout">logout</a><br>
    <result>
      <a href="/people/edit/$(personLetter)">edit</a>: Name: <personName/>, Letter: <personLetter/>, Spent: <personSpent/>, Owes: <personOwes/><br>
    </result>
    <a href="/people/add">add a person</a><br>
    <a href="/entries/add">add an entry</a><br><br>
    <entries>
      <a href="/entries/edit/$(index)">edit</a>, <a href="/entries/delete/$(index)">delete</a>: By: <entryBy/>, For: <entryFor/>, Amount: <entryAmount/>, Date: <entryDate/>, What: <entryWhat/><br>
    </entries>  
  </body>
</html>