--[[
number-sections - Number sections like the --number-sections option

If the "data-number" attribute is not required, opt it out by specifying `false`
to the `number_sections_with_attributes` metadata.

# MIT License

Copyright (c) 2020 Atsushi Yasumoto

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
]]
local full_attributes = FORMAT ~= "markdown"
local section_number_table = {0, 0, 0, 0, 0, 0, 0, 0, 0}
local n_section_number_table = #section_number_table
local previous_header_level = 0
local separator = pandoc.Space()
if FORMAT == "docx" then -- to be consistent with Pandoc >= 2.10.1
  separator = pandoc.Str("\t")
end

local function Meta(meta)
  if meta.number_sections_with_attributes then
    full_attributes = meta.number_sections_with_attributes
  end
end

local function Header(elem)
  -- If unnumbered
  if (elem.classes:find("unnumbered")) then
    if full_attributes then
      elem.attributes["data-number"] = ""
    end
    return elem
  end

  -- Else
  --- Reset and update section_number_table
  if (elem.level < previous_header_level) then
    for i=elem.level+1,n_section_number_table do
      section_number_table[i] = 0
    end
  end
  previous_header_level = elem.level
  section_number_table[elem.level] = section_number_table[elem.level] + 1

  --- Define section number as string
  local section_number_string = tostring(section_number_table[elem.level])
  if elem.level > 1 then
    for i = elem.level-1,1,-1 do
      section_number_string = section_number_table[i] .. "." .. section_number_string
    end
  end

  --- Update Header element
  table.insert(elem.content, 1, separator)
  if full_attributes then
    table.insert(elem.content, 1, pandoc.Span(section_number_string))
    elem.content[1].classes = {"header-section-number"}
    elem.attributes["data-number"] = section_number_string
  else
    table.insert(elem.content, 1, pandoc.Str(section_number_string))
  end
  return elem
end

local number_sections = {
  {Meta = Meta},
  {Header = Header}
}

return number_sections
