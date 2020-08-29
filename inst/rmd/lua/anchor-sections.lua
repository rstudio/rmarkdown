function Header(el)
  content = {pandoc.RawInline('html',
    '<a href="#'..el.identifier..'" class="anchor"></a>')}
  for i,c in ipairs(el.content) do
    content[i+1] = c
  end
  el.content = content
  el.classes[#el.classes+1] = 'hasAnchor'
  return(el)
end
