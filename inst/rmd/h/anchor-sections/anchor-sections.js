document.addEventListener('DOMContentLoaded', function() {
  const h = document.querySelectorAll('h1, h2, h3, h4, h5, h6');

  /* Do nothing if sections are already anchored */
  if (Array.from(h).some(x => x.classList.contains('hasAnchor'))) {
    return null;
  }

  /* Use parent id when pandoc runs with --section-divs  */
  const parent_id = function(x) {
    const parent = x.parentElement;
    return (
      (parent.classList.contains('section') || (parent.tagName === 'SECTION'))
      ? parent.id : ''
    );
  };

  /* Add anchors */
  h.forEach(function(x) {
    const id = x.id || parent_id(x);
    if (id === "") {
      return null;
    }
    let anchor = document.createElement('a');
    anchor.href = '#' + x.parentElement.id;
    anchor.classList = ['anchor-section'];
    anchor.textContent ='#';
    x.classList.add('hasAnchor');
    x.appendChild(anchor);
  });

  /* Apply CSS */
  let style = document.createElement('style');
  style.type = 'text/css';
  style.innerText = '.hasAnchor:hover a.anchor-section {visibility: visible;}' +
    'a.anchor-section {margin-left: 10px; visibility: hidden;}';
  document.head.appendChild(style);
});
