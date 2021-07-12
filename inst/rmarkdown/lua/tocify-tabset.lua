local tabset_level = 100
local tab_level = 101
local flag = false
local stringify = pandoc.utils.stringify

function is_tabset(classes)

  local res = false

  for _,v in ipairs(classes) do
    res = res or (v == "tabset")
  end

  return res
end

function Block(block)
  if block.tag == "Header" then
    local level = block.level
    if level <= tabset_level then
      flag = is_tabset(block.classes)
      tabset_level = flag and level or 100
      tab_level = tabset_level + 1
    elseif level == tab_level then
      local id = block.identifier
      block.identifier = id .. "-tab"
      return {
        block,
        pandoc.RawBlock(
          "html",
          "<h" .. level .. " style='visibility: hidden'>" ..
          stringify(block) ..
          "</h" .. level .. ">"
        )
      }
    end
  end
end
