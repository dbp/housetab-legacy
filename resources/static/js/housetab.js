bean.add(document, 'DOMContentLoaded', function () {
  // single select box-fields
  declare("click", ".box-field .display", true, function (elem) {
    bonzo(bonzo(elem).next()).show(); // show the box
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
    bonzo(bonzo(elem).next()).show(); // show the box
  });
  // declare("click", ".box-field-multi .box .close", true, function (elem) {
  //   bonzo(elem.parentNode).hide();
  // });
  declare("click", ".box-field-multi .box .option", true, function (elem) {
    e = bonzo(elem);
    d = bonzo(elem.parentNode).previous()[0]; // display
    h = bonzo(d).previous()[0] // hidden value
    if (e.hasClass("selected")) {
      // remove from list
      e.removeClass("selected");
      sels = h.getAttribute("value").split(",");
      // strip out the element, using valentine
      h.setAttribute("value",v(sels).filter(function(i){return i!==elem.getAttribute("data-box-value")}).join(","));
      d.innerHTML = (sels.length - 1) + " selected.";
    } else {
      // add to list
      e.addClass("selected");
      val = h.getAttribute("value");
      if (val.length === 0) {
        sels = [];
      } else {
        sels = val.split(",");
      }
      h.setAttribute("value",sels.concat([elem.getAttribute("data-box-value")]).join(","));
      d.innerHTML = (sels.length + 1) + " selected.";
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
     m.css("display", "block");
   } else {
     m.css("display", "none");
   }
 });
 declare("click", ".more-box .more", true, function (elem){
   bonzo(elem).css("display", "none");
 });
 
});