if (window.location.hostname === 'rstudio.github.io') {
  window.location = 'https://rmarkdown.rstudio.com' + location.pathname.replace(/^\/rmarkdown/, '');
}
