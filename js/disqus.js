var disqus_config = function () {
  this.page.url = 'http://rmarkdown.rstudio.com' + location.pathname;
};
$(document).ready(function() {
  var inIFrame = function() {
    var iframe = true;
    try { iframe = window.self !== window.top; } catch (e) {}
    return iframe;
  };
  if (inIFrame()) return;

  // create disqus script tag
  var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
  dsq.src = '//rmarkdown.disqus.com/embed.js';

  // determine container
  var container = document.getElementsByTagName('body')[0] || document.getElementsByTagName('head')[0];

  // append script tag enclosed by google indexing suppression comment
  container.appendChild(document.createComment('googleoff: all'));
  container.appendChild(dsq);
  container.appendChild(document.createComment('googleon: all'));
});
