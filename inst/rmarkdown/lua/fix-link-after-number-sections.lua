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
