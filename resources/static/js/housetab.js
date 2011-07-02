bean.add(document, 'DOMContentLoaded', function () {
  // single select box-fields
  declare("click", ".box-field .display", true, function (elem) {
    m = bonzo(bonzo(elem).next());
    if (m.css("display") === "none") {
      m.css("display", "block");
    } else {
      m.css("display", "none");
    }
  });
  declare("click", ".box-field .box .option", true, function (elem) {
    bonzo(elem.parentNode).hide();
    d = bonzo(elem.parentNode).previous()[0];
    d.innerHTML = elem.innerHTML;
    d.setAttribute("class", (d.getAttribute("class") || "") + " " + (elem.getAttribute("id") || ""));
    bonzo(bonzo(d).previous()[0]).attr("value", elem.getAttribute("data-box-value"));
  });
  
  // multi-select box-fields
  declare("click", ".box-field-multi .display", true, function (elem) {
    // set up the selections, based on what is already selected
    h = bonzo(elem).previous()[0]; // the hidden input
    sels = h.getAttribute("value").split(",");
    v(bonzo(elem).next()[0].childNodes).each(function(n) {
      if (n.hasAttribute && n.hasAttribute("data-box-value")) {
        if (sels.indexOf(n.getAttribute("data-box-value")) != -1) {
          if (!bonzo(n).hasClass("selected")) {
            bonzo(n).addClass("selected");
          }
        } else {
          bonzo(n).removeClass("selected");
        }
      }
    });
    m = bonzo(bonzo(elem).next());
    if (m.css("display") === "none") {
      m.css("display", "block");
    } else {
      m.css("display", "none");
    }
  });
  // declare("click", ".box-field-multi .box .close", true, function (elem) {
  //   bonzo(elem.parentNode).hide();
  // });
  declare("click", ".box-field-multi .box .option", true, function (elem) {
    e = bonzo(elem);
    d = bonzo(elem.parentNode).previous()[0]; // display
    h = bonzo(d).previous()[0] // hidden value
    function get_name(value) {
      return v(elem.parentNode.childNodes).toArray().filter(function(i){return i.nodeType === 1 && i.getAttribute("data-box-value") === value})[0].innerHTML;
    };
    var num_people = v(elem.parentNode.childNodes).toArray().filter(function(i){return i.nodeName === "DIV" && bonzo(i).hasClass("option");}).length;
    function display(selected) {
      if (selected.length === num_people) {
        return "ALL USERS"
      } else if (selected.length <= 2) {
        return v(selected).map(function(i){return get_name(i);}).join(" & ");
      } else {
        return get_name(selected[0]) + " & " + (selected.length - 1) + " others.";
      }
    };
    if (e.hasClass("selected")) {
      // remove from list
      e.removeClass("selected");
      sels = h.getAttribute("value").split(",");
      // strip out the element, using valentine
      var newsels = v(sels).filter(function(i){return i!==elem.getAttribute("data-box-value")});
      h.setAttribute("value",newsels.join(","));
      d.innerHTML = display(newsels);
    } else {
      // add to list
      e.addClass("selected");
      val = h.getAttribute("value");
      if (val.length === 0) {
        sels = [];
      } else {
        sels = val.split(",");
      }
      var newsels = sels.concat([elem.getAttribute("data-box-value")]);
      h.setAttribute("value",newsels.join(","));
      d.innerHTML = display(newsels);
    }
  });
  
  // generic close boxs (num is how far up to go)
  declare("click", ".close", true, function (elem) {
      bonzo(elem.parentNode).hide();
    });
  declare("click", ".close2", true, function (elem) {
      bonzo(elem.parentNode.parentNode).hide();
    });
 // more box, ie, a div that when you click on it, it pops up a box with more content in it.
 declare("click", ".more-box .showing", true, function (elem){
   m = bonzo(bonzo(elem).next());
   if (m.css("display") === "none") {
     // close all other more-boxes
     v(qwery(".more-box .more")).each(function(i) {
       bonzo(i).css("display", "none");
     });
     // now show this one
     m.css("display", "block");
   } else {
     m.css("display", "none");
   }
 });
 declare("click", ".more-box .more", true, function (elem){
   bonzo(elem).css("display", "none");
 });
 
});