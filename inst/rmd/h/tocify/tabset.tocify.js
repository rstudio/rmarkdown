document.addEventListener('DOMContentLoaded', function() {
  const anchors = Array.from(document.querySelectorAll("ul.nav.nav-tabs li a")).
    filter(a => a.attributes.role.value === "tab").
    reduce((hash, a) => {
      hash["#" + a.innerText.replace(/ /, "_")] = a;
      return hash;
    }, {});
  window.addEventListener('hashchange', function() {
    const anchor = anchors[location.hash];
    if (anchor !== undefined) {
      anchor.click();
    }
  });
});
