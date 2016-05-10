var slidy_irc = {
  shown: [],
  pin_regex: /^\d{4}$/,

  start: function ()
  {
    w3c_slidy.add_observer(function(number, title, url) {
      var pin = document.getElementById('slidy_pin');

      if (pin.value && slidy_irc.pin_regex.test(pin.value) && slidy_irc.unposted(number))
      {
        slidy_irc.post("https://www.w3.org/Talks/Tools/slidy-irc.php?" +
          "pin=" + escape(pin.value) +
          "&slide=" + number + 
          "&title=" + escape(title) +
          "&uri=" + escape(url));
        slidy_irc.shown.push(number);
      }
    });

    var pin = document.getElementById('slidy_pin');

    pin.addEventListener('change', function () {
      if (slidy_irc.pin_regex.test(pin.value))
      {
        w3c_slidy.notify_observers();
        pin.blur();
      }
      else
        alert("The PIN should be a 4 digit number");
    }, false);
  },

  post: function (url)
  {
    var req = new XMLHttpRequest();
    req.open("POST", url, true);
    req.send();
  },

  unposted: function (number)
  {
    return this.shown.indexOf(number) < 0;
  }
}

window.addEventListener("load", function() { slidy_irc.start(); }, false); 
document.write('<p id="slidy_irc" title="for notifying slide turns to IRC"'+
  '<label>Presenter PIN: <input id="slidy_pin" type="password"></label></p>');
