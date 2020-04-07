--[[
     A Pandoc 2 lua filter converting Pandoc native divs to LaTeX environments
     Author: Romain Lesur, Christophe Dervieux, and Yihui Xie
     License: Public domain
--]]

Div = function (div)
  local options = div.attributes['data-latex']

  -- if the output format is not latex, the object is left unchanged
  if FORMAT ~= 'latex' and FORMAT ~= 'beamer' then
    -- if options has been set for latex, unset for other output
    if options then
      div.attributes['data-latex'] = nil
    end
    return div
  end

  local env = div.classes[1]
  -- if the div has no class, the object is left unchanged
  if not env then return nil end

  -- insert raw latex before content
  table.insert(
    div.content, 1,
    pandoc.RawBlock('tex', '\\begin' .. '{' .. env .. '}' .. (options or ""))
  )
  -- insert raw latex after content
  table.insert(
    div.content,
    pandoc.RawBlock('tex', '\\end{' .. env .. '}')
  )
  return div
end
