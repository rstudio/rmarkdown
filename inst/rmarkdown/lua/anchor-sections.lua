--[[
anchor-sections - add anchor sections link to headers

MIT License

Copyright © 2020 RStudio

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
]]

local anchor_depth

-- retrieve metadata
function get_anchor_depth(el)
  anchor_depth = tonumber(el.rmd_anchor_depth)
  if not anchor_depth then
    anchor_depth = 6
  end
end

-- insert anchor link
function insert_anchor(el)
  -- do not add anchor on empty headings
  if #el.content == 0 then return(nil) end

  if el.identifier ~= "" then
    if el.level <= anchor_depth then
      --[[
      with --section-divs this will be added on the section div not the Header.
      The class will be moved to the header node by anchor-sections.js
      ]]
      table.insert(el.classes, "hasAnchor")
    end
    -- .hasAnchor could be added by the piece above or manually by the user
    if el.classes:find("hasAnchor") then
      table.insert(el.content,
        pandoc.Link("", "#"..el.identifier, "",
          pandoc.Attr(
            "", {"anchor-section"}, {["aria-label"]="Anchor link to header"}
          )
        )
      )
    end
  end
  return(el)
end


return {
  { Meta = get_anchor_depth },
  { Header =  insert_anchor }
}
