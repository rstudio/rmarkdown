--[[
     Wrap headers and record their identifiers by `Div`.
     `fix-link-after-number-sections.lua` uses the recorded identifiers and
     unwrap headers.

     Author: Atsushi Yasumoto
     License: Public domain
]]
if (not PANDOC_VERSION) or (PANDOC_VERSION < "2.1") then
  io.stderr:write("[WARNING] (pagebreak.lua) requires at least Pandoc 2.1. Lua filter skipped.")
  return {}
end

function Header(header)
  div = pandoc.Div({header})
  div.attributes['data-rmarkdown-temporarily-recorded-id'] = header.identifier
  return div
end
