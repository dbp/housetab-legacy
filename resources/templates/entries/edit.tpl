<div-async name="entry-$(id-value)">
  <form-async action="/entries/edit/$(id-value)" method="POST">
        <input type="hidden" name="id" value="$(id-value)" />
        <label for="by">By:</label> <by-errors><error/><br></by-errors>
        <input class="person" name="by" type="text" value="$(by-value)" />
        
        <label for="for">For:</label> <for-errors><error/><br></for-errors>
        <input class="person" name="for" type="text" value="$(for-value)" /> 
        
        <label for="ammount">Ammount:</label> <ammount-errors><error/><br></ammount-errors>
        <input name="ammount" type="text" value="$(ammount-value)" />
        
        <label for="what">What:</label> <what-errors><error/><br></what-errors>
        <input name="what" type="text" value="$(what-value)" />
        
        <label for="date">Date:</label> <date-errors><error/><br></date-errors>
        <input name="date" type="text" value="$(date-value)" />
        <button type="submit" title=""/>
  </form-async>
</div-async>

<script type="text/javascript">
  $("form .person").tokenInput("/people/list")
</script>
