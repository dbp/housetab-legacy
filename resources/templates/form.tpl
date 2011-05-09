<html>
  <head>
    <title>Form Page</title>
    <script type="text/javascript" src="/jquery-1.6.min.js"></script>
    <script type="text/javascript">
    // Requires that JQuery also be in scope
    function findInputList(button) {
      var mainDiv = $(button).parent();
      while ( !mainDiv.hasClass('inputList') ) {
        mainDiv = $(mainDiv).parent();
      }
      return mainDiv;
    }

    function findItems(button) {
      return $('.inputListItem', findInputList(button));
    }

    function addItem(button) {
      var count = $(':hidden', findInputList(button))[0];
      var items = findItems(button);
      var item = $(items[items.length-1]);
      var newItem = item.clone(true);
      var i;

      // Increment counter
      $(count).val(parseInt($(count).val())+1);

      // We have to change the raw html because IE doesn't allow the
      // name field to be changed.
      newItem.html(newItem.html().replace(/fval\[(\d+\.)*(\d+)\.(\d+)\]/g,
        function(a, b, c, d) {
          var newC = parseInt(c)+1;
          return a.replace(/\d+\.\d+\]/, newC+'.'+d+']');
        }
      ));
      newItem.appendTo(item.parent());

      // Copy the values of all children that had the name attribute set.
      // The direct html insertion does not preserve the most current
      // values.  It only preserves default values, so if we want values
      // copied, we have to use an approach like this.
      var items2 = findItems(button);
      var newLast = $(items2[items2.length-1]);
      var c1 = $('[name]', item);
      var c2 = $('[name]', newLast);
      if ( c1.length == c2.length ) {
        for ( i = 0; i < c1.length; i++ ) {
          $(c2[i]).val($(c1[i]).val());
        }
      }
    }

    function removeItem(button) {
      var items = findItems(button);
      if ( items.length > 1 ) {
        var count = $(':hidden', findInputList(button))[0];
        var item = $(items[items.length-1]);
        item.remove();

        // Decrement counter
        $(count).val(parseInt($(count).val())-1);
      } else {
        alert('Cannot remove any more rows');
      }
    }
    
    </script>
  </head>
  <body>
    <form method="post">
    <formdata/>
    <input type="submit" value="submit"/>
    
    <p><a href="/entries">Return</a></p>
  </body>
</html>
