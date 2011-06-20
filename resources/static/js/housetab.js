bean.add(document, 'DOMContentLoaded', function () {
  declare("click", ".box-field .display", true, function (elem) {
    bonzo(bonzo(elem).next()).css({display: 'block', width: '100px', height: 20, border: '1px solid red'}); // show the box
  });
  declare("click", ".box-field .box .option", true, function (elem) {
    bonzo(elem.parentNode).css({display: 'none'});
    d = bonzo(elem.parentNode).previous()[0];
    d.innerHTML = elem.innerHTML;
    bonzo(bonzo(d).previous()[0]).attr("value", elem.getAttribute("data-box-value"));
  });
});