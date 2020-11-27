local anchor_maxlevel

-- retrieve metadata
function get_metadata(el)
  anchor_maxlevel = tonumber(el.rmd_anchor_maxlevel)
  if not anchor_maxlevel then
    anchor_maxlevel = 6
  end
end

-- insert anchor link
function insert_anchor(el)
  if el.identifier ~= "" and el.level <= anchor_maxlevel then
    el.classes:insert("hasAnchor")
    table.insert(el.content,
      pandoc.Link("", "#"..el.identifier, "", {class = "anchor-section"})
    )
  end
  return(el)
end


return {
  { Meta = get_metadata },
  { Header =  insert_anchor}
}
