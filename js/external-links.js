(function() {
  var links = document.getElementsByTagName('a'), link, n, child;
  for (var i = 0; i < links.length; i++) {
    link = links[i];
    if (/^(https?:)?\/\//.test(link.getAttribute('href'))) {
      link.target = '_blank';
      n = link.childElementCount;
      if (n === 0 && link.innerText) {
        link.innerHTML += ' &boxbox;';  // pure text
      } else if (n === 2) {
        // the page /formats.html
        child = link.firstElementChild;
        if (child.className === 'formatName') child.innerHTML += ' &boxbox;';
      }
    }
  }
})();
