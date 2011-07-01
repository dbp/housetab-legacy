/*!
  * declarative.js - copyright @dbp 2011
  * BSD3 License
  */
function declare(event, selector, nopropagate, fun) {
  // bean is an cross-browser standalone event handler at https://github.com/fat/bean
  bean.add(document.documentElement, event, function(e) {
    e = e || window.event;
    var elem = e.target || e.srcElement;
    // qwery is a cross-browser selector engine at https://github.com/ded/qwery
    var sel = qwery(selector);
    var count = 5; // don't try further than that.
    function walk(el) {
      if (el && count > 0) {
        count = count - 1;
        if (sel.indexOf(el) !== -1) {
          return el;
        } else {
          return walk(el.parentNode);
        }
      } else {
        return false;
      }
    };
    var w = walk(elem);
    if (!elem || !w) {
      return;
    }
    fun(w);
    
    if (nopropagate) {
      e.stopPropagation();
    }
  });
}