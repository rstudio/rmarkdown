var RMD = {};

(function() {


  RMD.AppViewModel = function() {
    var self = this;

    self.handleWindowScroll = function(event) {
      var scrollPos = $(event.target).scrollTop();
      if (scrollPos > 20) {
        $('#rStudioHeader').addClass("shrunk");
      }
      else {
        $('#rStudioHeader').removeClass("shrunk");
      }
    }

    var menuShowing = false;
    self.handleMenuTogglerClick = function(event) {
      menuShowing = !menuShowing;
      if (menuShowing) {
        $('#menuItems').addClass("showMenu");
      }
      else {
        $('#menuItems').removeClass("showMenu");
      }

    }

    self.init = function() {
      $(window).on('scroll', self.handleWindowScroll);
      $('#menuToggler').on('click', self.handleMenuTogglerClick);
    }
  }


}());

$(document).ready(function() {
  RMD.app = new RMD.AppViewModel();
  RMD.app.init();
});
