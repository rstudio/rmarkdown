--[[
     Fix broken links to headers due by `number-sections.lua`.
     `record-header-id.lua` prior to `number-sections.lua`.

     Author: Atsushi Yasumoto
     License: Public domain
]]
if (not PANDOC_VERSION) or (PANDOC_VERSION < "2.1") then
  io.stderr:write("[WARNING] (pagebreak.lua) requires at least Pandoc 2.1. Lua filter skipped.")
  return {}
end

identifiers = {}

function Div(div)
  local old_id = div.attributes['data-rmarkdown-temporarily-recorded-id']

  if old_id and #div.content and (div.content[1].t == 'Header') then
    identifiers['#' .. old_id] = '#' .. div.content[1].identifier
    return div.content
  end
end

function Link(link)
  link.target = identifiers[link.target] or link.target
  return link
end

return {{Div = Div}, {Link = Link}}
