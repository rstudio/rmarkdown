function Header(header)
  div = pandoc.Div({header})
  div.attributes['data-rmarkdown-temporarily-recorded-id'] = header.identifier
  return div
end
