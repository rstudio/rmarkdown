--[[
     A Pandoc 2 Lua filter converting Pandoc native divs to LaTeX environments
     Author: Romain Lesur, Christophe Dervieux, Atsushi Yasumoto and Yihui Xie
     License: Public domain
--]]

-- COMMON PART ACROSS LUA FILTERS

--[[
  This function tests the pandoc version againt a target version
  For Pandoc 2.7.3, PANDOC_VERSION >= "2.8" would be enough but before 2.7.3
  it is a table object
]]
local function pandocAvailable(target)
  -- this function only work for Pandoc 2.1 and above. It returns false is not
  if not PANDOC_VERSION then return false end
  assert(target, "No target version specified in pandocAvailable.")
  -- checking each version number
  -- EPOCH.MAJOR.MINOR.PATCH (https://pvp.haskell.org)
  for i = 1,4 do
    -- some versions do not have all numbers
    if not PANDOC_VERSION[i] or not target[i] then break end
    if PANDOC_VERSION[i] < target[i] then return false end
    if PANDOC_VERSION[i] > target[i] then return true end
  end
  return true
end

--[[
  About the requirement:
  * PANDOC_VERSION -> 2.1
]]
if (not pandocAvailable {2,7,3}) then
    io.stderr:write("[WARNING] (latex-div.lua) requires at least Pandoc 2.1. Lua Filter skipped.\n")
    return {}
end

-- START OF THE FILTER'S FUNCTIONS

text = require 'text'

Div = function (div)
  -- look for 'latex' or 'data-latex' and at least 1 class
  local options = div.attributes['latex']
  if not options then
    options = div.attributes['data-latex']
  end
  if not options or #div.attr.classes == 0 then
    return nil
  end

  -- if the output format is not latex, remove the attr and return
  if FORMAT ~= 'latex' and FORMAT ~= 'beamer' then
    div.attributes['latex'] = nil
    div.attributes['data-latex'] = nil
    return div
  end

  -- if it's "1" or "true" then just set it to empty string
  if options == "1" or text.lower(options) == "true" then
    options = ""
  end

  -- environment begin/end
  local env = div.classes[1]
  local beginEnv = '\\begin' .. '{' .. env .. '}' .. options
  local endEnv = '\n\\end{' .. env .. '}'

  -- if the first and last div blocks are paragraphs then we can
  -- bring the environment begin/end closer to the content
  if div.content[1].t == "Para" and div.content[#div.content].t == "Para" then
    table.insert(div.content[1].content, 1, pandoc.RawInline('tex', beginEnv .. "\n"))
    table.insert(div.content[#div.content].content, pandoc.RawInline('tex', "\n" .. endEnv))
  else
    table.insert(div.content, 1, pandoc.RawBlock('tex', beginEnv))
    table.insert(div.content, pandoc.RawBlock('tex', endEnv))
  end
  return div
end
