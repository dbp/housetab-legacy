bean.add(document, 'DOMContentLoaded', function () {
  // this is the first step of the tutorial. it has to be explicitly triggered, as it is a made up event.
  declare("tutorial", "#tutorial .content", true, function (elem) {
    e = bonzo(elem);
    elem.innerHTML = "<p>You have successfully activated your HouseTab account. To continue setting up your account follow these steps:</p><p>1. Add at least two users to the account.</p>";
  });
  
  // trigger it!
  bean.fire(qwery("#tutorial .content")[0],"tutorial");
});