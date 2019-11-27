--[[
     A Pandoc 2 lua filter converting Pandoc native divs to LaTeX environments
     Author: Romain Lesur and Yihui Xie
     License: Public domain
--]]
local pandocList = require 'pandoc.List'

Div = function (div)
  local options = div.attributes['data-latex']
  if options == nil then return nil end

  -- if the output format is not latex, the object is left unchanged
  if FORMAT ~= 'latex' and FORMAT ~= 'beamer' then
    div.attributes['data-latex'] = nil
    return div
  end

  local env = div.classes[1]
  -- if the div has no class, the object is left unchanged
  if not env then return nil end

  -- build the returned list of blocks
  local beginEnv = pandocList:new{pandoc.RawBlock('tex', '\\begin' .. '{' .. env .. '}' .. options)}
  local endEnv = pandocList:new{pandoc.RawBlock('tex', '\\end{' .. env .. '}')}
  local returnedList = beginEnv .. div.content .. endEnv

  return returnedList
end
