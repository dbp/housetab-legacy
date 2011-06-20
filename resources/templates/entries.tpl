<apply template="base">

    <result>
      <a href="/people/edit/$(personId)">edit</a>: Name: <personName/>, Spent: <personSpent/>, Owes: <personOwes/><br>
    </result>
    <a-async href="/people/add">add a person</a-async><br><br>
    <a-async href="/entries/add">add an entry</a-async><br><br>
    
    <div-async name="entry-form"></div-async>
    <div-async name="add-person"></div-async>
    
    
    <entries>
      <apply template="entries/show"></apply>
    </entries>  

    

    <!-- <div class="fancyField">
       <input type="hidden" name="fieldName" value="" />
       <div class="display" style="width: 50px; height: 10px; border: 1px solid black;"></div>
       <div class="box" style="display: none;">
         <div class="option" data-value="024034270650764">
           Buenaventura
         </div>
         <div class="option" data-value="427648476857">
           Remeike
         </div>
       </div>
     </div> -->
 
</apply>