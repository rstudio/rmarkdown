--[[
codefolding - fold code blocks in HTML documents

MIT License

Copyright @ 2022-2022 Atsushi Yasumoto

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--]]

local DEFAULT_STATE

function Meta(meta)
  DEFAULT_STATE = meta["rmd_codefolding_lua"] == "show" and " open" or ""
end

function CodeBlock(elem)
  local SUPPORTED_CLASSES = {
    'r', 'python', 'bash', 'sql', 'cpp', 'stan', 'julia', 'foldable'
  }

  -- Do nothing if no supported classes are given
  local folding = false
  for _, v in pairs(SUPPORTED_CLASSES) do
    if elem.classes:find(v) then
      folding = true
      break
    end
  end
  if not folding then return elem end

  -- Do nothin if has "fold-none" class
  if elem.classes:find("fold-none") then return elem end

  -- Do folding
  local state = 
    (elem.classes:find("fold-show") and " open") or
    (elem.classes:find("fold-hide") and "") or
    DEFAULT_STATE

  return {
    pandoc.RawBlock(
      "html",
      "<details class=chunk-details" .. state .. ">"
      ..
      "<summary class=chunk-summary><span class=chunk-summary-text>Code</span></summary>"
    ),
    elem,
    pandoc.RawBlock("html", "</details>")
  }
end

return {
  { Meta = Meta },
  { CodeBlock = CodeBlock }
}
