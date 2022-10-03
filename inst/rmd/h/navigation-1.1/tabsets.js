/**
 * jQuery Plugin: Sticky Tabs
 *
 * @author Aidan Lister <aidan@php.net>
 * adapted by Ruben Arslan to activate parent tabs too
 * http://www.aidanlister.com/2014/03/persisting-the-tab-state-in-bootstrap/
 */
(function($) {
  "use strict";
  $.fn.rmarkdownStickyTabs = function() {
    var context = this;
    // Show the tab corresponding with the hash in the URL, or the first tab
    var showStuffFromHash = function() {
      var hash = window.location.hash;
      var selector = hash ? 'a[href="' + hash + '"]' : 'li.active > a';
      var $selector = $(selector, context);
      if($selector.data('toggle') === "tab") {
        $selector.tab('show');
        // walk up the ancestors of this element, show any hidden tabs
        $selector.parents('.section.tabset').each(function(i, elm) {
          var link = $('a[href="#' + $(elm).attr('id') + '"]');
          if(link.data('toggle') === "tab") {
            link.tab("show");
          }
        });
      }
    };


    // Set the correct tab when the page loads
    showStuffFromHash(context);

    // Set the correct tab when a user uses their back/forward button
    $(window).on('hashchange', function() {
      showStuffFromHash(context);
    });

    // Change the URL when tabs are clicked
    $('a', context).on('click', function(e) {
      history.pushState(null, null, this.href);
      showStuffFromHash(context);
    });

    return this;
  };
}(jQuery));

window.buildTabsets = function(tocID) {

  // build a tabset from a section div with the .tabset class
  function buildTabset(tabset) {

    // check for fade and pills options
    var fade = tabset.hasClass("tabset-fade");
    var pills = tabset.hasClass("tabset-pills");
    var navClass = pills ? "nav-pills" : "nav-tabs";

    // determine the heading level of the tabset and tabs
    var match = tabset.attr('class').match(/level(\d) /);
    if (match === null)
      return;
    var tabsetLevel = Number(match[1]);
    var tabLevel = tabsetLevel + 1;

    // find all subheadings immediately below
    var tabs = tabset.find("div.section.level" + tabLevel);
    if (!tabs.length)
      return;

    // create tablist and tab-content elements
    var tabList = $('<ul class="nav ' + navClass + '" role="tablist"></ul>');
    $(tabs[0]).before(tabList);
    var tabContent = $('<div class="tab-content"></div>');
    $(tabs[0]).before(tabContent);

    var activeTabIndex = 0;
    var activeDropdownTabIndex = 0;
    tabs.each(function(i) {

      // get the tab div
      var tab = $(tabs[i]);

      // get the id then sanitize it for use with bootstrap tabs
      var tabId = tab.attr('id');

      // remove any table of contents entries associated with
      // this ID (since we'll be removing the heading element)
      $("div#" + tocID + " li a[href='#" + tabId + "']").parent().remove();

      // sanitize the id for use with bootstrap tabs
      tabId = tabId.replace(/[.\/?&!#<>]/g, '').replace(/\s/g, '_');
      tab.attr('id', tabId);

      // get the heading element within it, grab it's text, then remove it
      var tabHeading = tab.find('h' + tabLevel + ':first');
      var tabHeadingText = tabHeading.html();
      tabHeading.remove();

      // build and append the tab list item
      var li = $('<li role="presentation"></li>');
      tabList.append(li);

      // see if this is marked as the active tab, or set the first tab by
      // default
      if (tab.hasClass('active')) {
        activeTabIndex = i;
      }

      // check if this is a dropdown tab and process it accordingly
      var dropdown = tab.hasClass("tabset-dropdown");
      if (dropdown) {
        li.addClass('dropdown');

        // build and append the dropdown toggle
        var a = $('<a class="dropdown-toggle" data-toggle="dropdown" href="#">'
          + tabHeadingText
          + ' <span class="caret"></span></a>');
        li.append(a);

        // build and append the dropdown menu
        var dropdownMenu = $('<ul class="dropdown-menu"></ul>');
        li.append(dropdownMenu)

        // determine the heading level of the dropdown tabs
        var match = tab.attr('class').match(/level(\d) /);
        if (match === null)
          return;
        var dropdownLevel = Number(match[1]);
        var dropdownTabLevel = dropdownLevel + 1;

        // find all subheadings immediately below
        var dropdownTabs = tab.find("div.section.level" + dropdownTabLevel);
        if (!dropdownTabs.length)
          return;

        dropdownTabs.each(function(j) {

          // get the dropdown tab div
          var dropdownTab = $(dropdownTabs[j]);

          // get the id then sanitize it for use with bootstrap tabs
          var dropdownId = dropdownTab.attr('id');

          // remove any table of contents entries associated with
          // this ID (since we'll be removing the heading element)
          $("div#" + tocID + " li a[href='#" + dropdownId + "']")
            .parent().remove();

          // sanitize the id for use with bootstrap tabs
          dropdownId = dropdownId
            .replace(/[.\/?&!#<>]/g, '')
            .replace(/\s/g, '_');
          dropdownTab.attr('id', dropdownId);

          // get the heading element within it, grab it's text, then remove it
          var dropdownHeading = dropdownTab
            .find('h' + dropdownTabLevel + ':first');
          var dropdownHeadingText = dropdownHeading.html();
          dropdownHeading.remove();

          // build and append the dropdown tab list item
          var dropdownLi = $('<li role="presentation"></li>');
          dropdownMenu.append(dropdownLi);

          // see if this is marked as the active tab
          if (dropdownTab.hasClass('active')) {
            activeTabIndex = i;
            activeDropdownTabIndex = j;
          }

          // build and append the dropdown tab link
          var dropdownA = $('<a role="tab" data-toggle="tab">'
            + dropdownHeadingText
            + '</a>');
          dropdownA.attr('href', '#' + dropdownId);
          dropdownA.attr('aria-controls', dropdownId);
          dropdownLi.append(dropdownA);

          // set its attributes
          dropdownTab.attr('role', 'tabpanel');
          dropdownTab.addClass('tab-pane');
          dropdownTab.addClass('tabbed-pane');
          if (fade)
            dropdownTab.addClass('fade');

          // move it into the tab content div
          dropdownTab.detach().appendTo(tabContent);
        });
      } else {
        // build and append the tab link
        var a = $('<a role="tab" data-toggle="tab">' + tabHeadingText + '</a>');
        a.attr('href', '#' + tabId);
        a.attr('aria-controls', tabId);
        li.append(a);

        // set its attributes
        tab.attr('role', 'tabpanel');
        tab.addClass('tab-pane');
        tab.addClass('tabbed-pane');
        if (fade)
          tab.addClass('fade');

        // move it into the tab content div
        tab.detach().appendTo(tabContent);
      }
    });

    // set active dropdown item
    var activeTab = $(tabList.children('li')[activeTabIndex])
    activeTab.addClass('active');
    var contentTabID = activeTab.children('a').attr('aria-controls');

    // set active dropdown tab if necessary
    if (activeTab.hasClass("dropdown")) {
      var activeDropdownTab = $(activeTab.find('li')[activeDropdownTabIndex]);
      activeDropdownTab.addClass('active');

      var contentTabID = activeDropdownTab.children('a').attr('aria-controls');
    }

    // set active content tab
    $('#'+contentTabID).addClass('active');
    if (fade)
      $('#'+contentTabID).addClass('in');

    if (tabset.hasClass("tabset-sticky"))
      tabset.rmarkdownStickyTabs();
  }

  // convert section divs with the .tabset class to tabsets
  var tabsets = $("div.section.tabset");
  tabsets.each(function(i) {
    buildTabset($(tabsets[i]));
  });
};
