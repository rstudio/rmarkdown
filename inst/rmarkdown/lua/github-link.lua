

function Link(el)
  -- single 'uri' class is provided by default for pandoc 2.11, which
  -- causes github_document to write links w/ an <a> tag. clear this
  -- class when we see it which provides the expected output
  if #el.attr.classes == 1 and el.attr.classes[1] == 'uri' then
    el.attr.classes = {}
    return el
  else
    return nil
  end
end
