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
    if (!elem || qwery(selector).indexOf(elem) === -1) {
      return;
    }
    fun(elem);
    
    if (nopropagate) {
      e.stopPropagation();
    }
  });
}