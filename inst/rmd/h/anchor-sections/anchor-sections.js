document.addEventListener('DOMContentLoaded', function () {
  // Select anchor section links add by Lua filter
  const anchors = document.querySelectorAll('a.anchor-section')

  // Add the .hasAnchor class to parent header
  anchors.forEach(function (x) {
    const parentHeader = x.parentElement
    if (!parentHeader.classList.contains('hasAnchor')) {
      parentHeader.classList.add('hasAnchor')
    }
  })
})
