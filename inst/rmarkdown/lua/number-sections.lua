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
if FORMAT == "docx" then -- to be consistent with Pandoc >= 2.10.1
  separator = pandoc.Str("\t")
end


function is_preprocess(meta)
  -- Check if the filter is run in preprocessing step
  -- This should only be done for markdown document conversion
  preprocess = meta['preprocess_number_sections']
  return nil
end


function fix_link_id(link)
  -- Some markdown formats needs fixing on there link to numbered sections
  -- idenfiers is filled during processing when a pre processing has been run
  if not preprocess and #identifiers and identifiers[link.target] then
    link.target = identifiers[link.target]
    return link
  end
  -- do nothing in all other cases
  return nil
end


function number_sections(header)

  ----------------------------
  -- specific handling when pre-processing has been done
  ----------------------------

  -- This will only run on second conversion when a preprocessing has be run before
  -- to add recorded attributes set by us as Span attribute in first processing.
  if not preprocess and #header.content and (header.content[1].t == 'Span') then
      -- Check attributes
      local span_content = header.content[1]
      local old_id = span_content.attributes['data-rmarkdown-temporarily-recorded-id']
      -- If found, this is our Span added in pre processing step
      if old_id then
        identifiers['#' .. old_id] = '#' .. header.identifier
        -- remove span
        table.remove(header.content, 1)
        -- prepend span Inlines content to the header content
        header.content = span_content.content:__concat(header.content)
        -- return content without the span
        return header
      end
      -- Otherwise, not our spans so do nothing
      return nil
  end

  -- The following part runs in pre-processing step or main processing step
  -- depending on the format.
  -- (md output require a pre processing and numbered section ared added on input).

  -- If explicitly unnumbered, then we do nothing
  if header.classes:find("unnumbered") then
    return nil
  end

  ----------------------------
  -- Adding the number section
  ----------------------------

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

  ----------------------------
  -- Pre-processing specific handling
  ----------------------------

  --- Return Header wrapped in Spans with recording Header's current ID as attribute
  if preprocess then
    local attr = {}
    attr['data-rmarkdown-temporarily-recorded-id'] = header.identifier
    header.content = {pandoc.Span(
      header.content,
      pandoc.Attr('', {}, attr)
    )}
  end

  return header
end


return {
  {Meta = is_preprocess}, -- Are we running as pre-pocessing ?
  {Header = number_sections}, -- Add Number to section
  {Link = fix_link_id}, -- Only done when preprocessing has been run
}
