--[[
     A Pandoc 2 Lua filter converting Pandoc native divs to LaTeX environments
     Author: Romain Lesur, Christophe Dervieux, and Yihui Xie
     License: Public domain
--]]

Div = function (div)
  local options = div.attributes['data-latex']
  if options == nil then return nil end

  -- if the output format is not latex, remove the data-latex attr and return
  if FORMAT ~= 'latex' and FORMAT ~= 'beamer' then
    div.attributes['data-latex'] = nil
    return div
  end

  local env = div.classes[1]
  -- if the div has no class, the object is left unchanged
  if not env then return nil end

  -- insert raw latex before content
  table.insert(
    div.content, 1,
    pandoc.RawBlock('tex', '\\begin' .. '{' .. env .. '}' .. options)
  )
  -- insert raw latex after content
  table.insert(
    div.content,
    pandoc.RawBlock('tex', '\\end{' .. env .. '}')
  )
  return div
end
