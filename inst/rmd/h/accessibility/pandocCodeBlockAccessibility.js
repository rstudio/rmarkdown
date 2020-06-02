// Hide empty <a> tag within highlighted CodeBlock for screen reader accessibility (see https://github.com/jgm/pandoc/issues/6352#issuecomment-626106786) -->
// v0.0.1
// Written by JooYoung Seo (jooyoung@psu.edu) on June 1st, 2020.

const codeList = document.getElementsByClassName("sourceCode");

for(var i in codeList) {
  var linkList = codeList[i].getElementsByTagName('a');
  for(var j in linkList) {
    if(linkList[j].innerHTML === "") {
      linkList[j].setAttribute('aria-hidden', 'true');
    }
  }
}
