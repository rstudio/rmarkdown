var langs = [
  'awk', 'bash', 'coffeescript', 'cpp', 'css', 'diff', 'fortran', 'go',
  'ini', 'java', 'javascript', 'json', 'julia', 'makefile', 'markdown',
  'perl', 'python', 'r', 'ruby', 'sql', 'stan', 'tex', 'xml', 'yaml'
];
document.querySelectorAll('input').forEach(function(el) {
  el.checked = langs.indexOf(el.name.replace(/\.js$/, '')) > 0;
});
