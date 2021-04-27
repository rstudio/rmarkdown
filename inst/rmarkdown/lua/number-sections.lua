--[[
number-sections - Number sections like the --number-sections option

When output is a markdown flavor +gfm_auto_identifiers, pandoc must run twice
with this filter to keep alive links to numbered headers. In the first time,
add a following argument: `--metadata preprocess_number_sections=true`.

# MIT License

Copyright (c) 2020 Atsushi Yasumoto, Christophe Dervieux

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

https://github.com/atusy/lua-filters/blob/master/lua/number-sections.lua
]]

-- REQUIREMENTS: Load shared lua filter - see `shared.lua` for more details.
dofile(os.getenv 'RMARKDOWN_LUA_SHARED')

--[[
  About the requirement:
  * PANDOC_VERSION -> 2.1
]]
if (not pandocAvailable {2,1}) then
  io.stderr:write("[WARNING] (number-sections.lua) requires at least Pandoc 2.1. Lua Filter skipped.\n")
  return {}
end

-- START OF THE FILTER'S FUNCTIONS --

local section_number_table = {0, 0, 0, 0, 0, 0, 0, 0, 0}
local n_section_number_table = #section_number_table
local previous_header_level = 0
local separator = pandoc.Space()
local identifiers = {}
local needs_number_sections = true
if FORMAT == "docx" then -- to be consistent with Pandoc >= 2.10.1
  separator = pandoc.Str("\t")
end


function is_preprocess(meta)
  preprocess = meta['preprocess_number_sections']
end


function pairwise_link_id(div)
  local old_id = div.attributes['data-rmarkdown-temporarily-recorded-id']

  if not preprocess and old_id and #div.content and (div.content[1].t == 'Header') then
    needs_number_sections = false
    identifiers['#' .. old_id] = '#' .. div.content[1].identifier
    return div.content
  end
end


function fix_link_id(link)
  if not preprocess and identifiers[link.target] then
    link.target = identifiers[link.target]
    return link
  end
end


function number_sections(header)
  -- Unnumbered
  if not needs_number_sections or header.classes:find("unnumbered") then
    return nil
  end

  -- Else
  --- Reset and update section_number_table
  if (header.level < previous_header_level) then
    for i=header.level+1,n_section_number_table do
      section_number_table[i] = 0
    end
  end
  previous_header_level = header.level
  section_number_table[header.level] = section_number_table[header.level] + 1

  --- Define section number as string
  local section_number_string = tostring(section_number_table[header.level])
  if header.level > 1 then
    for i = header.level-1,1,-1 do
      section_number_string = section_number_table[i] .. "." .. section_number_string
    end
  end

  --- Update Header element
  table.insert(header.content, 1, separator)
  table.insert(header.content, 1, pandoc.Str(section_number_string))

  --- Return Div wrapping Header and recording Header's current ID
  if preprocess then
    div = pandoc.Div({header})
    div.attributes['data-rmarkdown-temporarily-recorded-id'] = header.identifier
    return div
  end

  return header
end


return {
  {Meta = is_preprocess},
  {Div = pairwise_link_id},
  {Link = fix_link_id},
  {Header = number_sections}
}
