
-- Table to store footnotes, so they can be included at the end.
local notes = {}

-- Rendering state
local in_slide = false
local build_slide = false
local in_notes = false

-- Character escaping
local function escape(s, in_attribute)
  s = s:gsub("[<>&\"']",
    function(x)
      if x == '<' then
        return '&lt;'
      elseif x == '>' then
        return '&gt;'
      elseif x == '&' then
        return '&amp;'
      elseif x == '"' then
        return '&quot;'
      elseif x == "'" then
        return '&#39;'
      else
        return x
      end
    end)
  return s
end


-- Helper function to convert an attributes table into
-- a string that can be put into HTML tags.
local function attributes(attr)
  local attr_table = {}
  for x,y in pairs(attr) do
    if y and y ~= "" then
      table.insert(attr_table, ' ' .. x .. '="' .. escape(y,true) .. '"')
    end
  end
  return table.concat(attr_table)
end

-- Helper function to split a string on spaces
-- returns a table
local function split(str)
  local words = {}
  for word in str:gmatch("%S+") do table.insert(words, word) end
  return words
end

-- Helper function to remove duplicates
-- returns a table http://stackoverflow.com/a/20067270
local function uniq(t)
  local seen = {}
  local res = {}
  for _,v in ipairs(t) do
    if (not seen[v]) then
      res[#res+1] = v
      seen[v] = true
    end
  end
  return res
end

-- Helper function to filter a list
-- returns a table
local function grep(f, l)
  local res = {}
  for _,v in ipairs(l) do
    if (f(v)) then
      res[#res+1] = v
    end
  end
  return res
end

-- Blocksep is used to separate block elements.
function Blocksep()
  return "\n\n"
end

local function CompleteSlide()
  if (in_slide) then
    in_slide = false
    return  "</article></slide>"
  else
    return ""
  end
end


-- This function is called once for the whole document. Parameters:
-- body is a string, metadata is a table, variables is a table.
-- One could use some kind of templating
-- system here; this just gives you a simple standalone HTML file.
function Doc(body, metadata, variables)
  -- complete any active slide
  local suffix = CompleteSlide()

  return body .. suffix
end

-- The functions that follow render corresponding pandoc elements.
-- s is always a string, attr is always a table of attributes, and
-- items is always an array of strings (the items in a list).
-- Comments indicate the types of other variables.

function Str(s)
  s = escape(s)
  if smart then
    s = s:gsub("%-%-%-", "&#8212;")
    s = s:gsub("%-%-", "&#8211;")
    s = s:gsub("%.%.%.", "&#8230;")
  end
  return s
end

function Space()
  return " "
end

function LineBreak()
  return "<br/>"
end

function SoftBreak()
  return " "
end

function Emph(s)
  return "<em>" .. s .. "</em>"
end

function Strong(s)
  return "<strong>" .. s .. "</strong>"
end

function Subscript(s)
  return "<sub>" .. s .. "</sub>"
end

function Superscript(s)
  return "<sup>" .. s .. "</sup>"
end

function SmallCaps(s)
  return '<span style="font-variant: small-caps;">' .. s .. '</span>'
end

function Strikeout(s)
  return '<del>' .. s .. '</del>'
end

function SingleQuoted(s)
  if smart then
    return '&lsquo;' .. s .. '&rsquo;'
  else
    return '"' .. s .. '"'
  end
end

function DoubleQuoted(s)
  if smart then
    return '&ldquo;' .. s .. '&rdquo;'
  else
    return '"' .. s .. '"'
  end
end

function Link(s, src, tit)
  return "<a href='" .. escape(src,true) .. "' title='" ..
         escape(tit,true) .. "'>" .. s .. "</a>"
end

function Image(s, src, tit, attr)
  return "<img" .. attributes(attr) .. " src='" .. escape(src,true) .. "' title='" ..
         escape(tit,true) .. "'/>"
end

function CaptionedImage(src, tit, s, attr)
  local caption = ""
  if fig_caption and (string.len(s) > 0) then
    caption = "<p class='caption'>" .. s .. "</p>"
  end
  return Image(s, src, tit, attr) .. caption
end

function Code(s, attr)
  return "<code" .. attributes(attr) .. ">" .. escape(s) .. "</code>"
end

function InlineMath(s)
  s = escape(s)
  if mathjax then
    return "\\(" .. s .. "\\)"
  else
    return "$" .. s .. "$"
  end
end

function DisplayMath(s)
  s = escape(s)
  if mathjax then
    return "\\[" .. s .. "\\]"
  else
    return "$$" .. s .. "$$"
  end
end

function Note(s)
  local num = #notes + 1
  -- insert the back reference right before the final closing tag.
  s = string.gsub(s,
          '(.*)</', '%1 <a href="#fnref' .. num ..  '">&#8617;</a></')
  -- add a list item with the note to the note table.
  table.insert(notes, '<li id="fn' .. num .. '">' .. s .. '</li>')
  -- return the footnote reference, linked to the note.
  return '<a id="fnref' .. num .. '" href="#fn' .. num ..
            '"><sup>' .. num .. '</sup></a>'
end

function Span(s, attr)
  return "<span" .. attributes(attr) .. ">" .. s .. "</span>"
end

function Cite(s)
  return "<span class=\"cite\">" .. s .. "</span>"
end

function Plain(s)
  return s
end

function Para(s)
  return "<p>" .. s .. "</p>"
end

-- lev is an integer, the header level.
function Header(lev, s, attr)

  -- detect level 1 header and convert it to a segue slide
  local slide_class = ""
  local hgroup_class = ""
  local slide_style = ""

  -- make all headers < slide_level as segue slides
  if lev < slide_level then
  	-- create a segue slide but add lev class for possible customization
    slide_class = "segue dark nobackground" .. " level" .. lev
    hgroup_class = " class = 'auto-fadein'"
    lev = 2
  end

  -- support for slide specific image backgrounds
  -- alternative is this css
  -- slide > slide [data-slide-num="7"] {
  --   background-image: url("figures/xx.jpg");
  -- }
  if attr["data-background"] then
    -- dark is incompatible with fill and let us uniquify nobackground
    local slide = split(slide_class .. " fill nobackground")
    slide = grep(function (v)
      if v:match("^dark$") then return false else return true end
    end, slide)
    slide_class = table.concat(uniq(slide), " ")
    if attr["data-background"]:match("^#") then
      slide_style = 'background: ' .. attr["data-background"] .. ';'
    else
      -- assume url
      slide_style = 'background-image: url(' .. attr["data-background"] .. ');'
      local bg_size = attr["data-background-size"]
      if not bg_size then bg_size = "contain" end
      slide_style = slide_style .. ' background-size: ' .. bg_size .. ';'
      local bg_position = attr["data-background-position"]
      if not bg_position then bg_position = "center" end
      slide_style = slide_style .. ' background-position: ' .. bg_position .. ';'
    end
    -- remove noise attributes for article
    attr["data-background"] = nil
    attr["data-background-size"] = nil
    attr["data-background-position"] = nil
  end

  -- extract optional subtitle
  local subtitle = ""
  if lev == 2 then
    local i, j = string.find(s, "|")
    if i then
      subtitle = string.sub(s, i+1, string.len(s))
      s = string.sub(s, 1, i-1)
    end
  end

  -- trick: as lev value 2 is used in code below to start a new slide
  -- we force all lev <= slide_level as new slides
  if lev > 2 and lev <= slide_level then
  	lev = 2
  end

  -- build slide header (including optional subtitle)
  local header = "<h" .. lev ..  ">" .. s .. "</h" .. lev .. ">"
  if string.len(subtitle) > 0 then
    header = header .. "<h3>" .. subtitle .. "</h3>"
  end

  -- treat level 2 headers as slides
  if lev == 2 then

    -- complete previous slide
    local preface = CompleteSlide()

    -- start a new slide
    in_slide = true

    -- update build flag (used by ol and ul)
    if attr["class"] and string.find(attr["class"], "build") then
      build_slide = true
    else
      build_slide = false
    end

    -- add 'smaller' class if it was globally specified
    if smaller then
      if (attr["class"]) then
        attr["class"] = "smaller " .. attr["class"]
      else
        attr["class"] = "smaller"
      end
    end

    if string.len(slide_style) > 0 then
      slide_style = ' style="' .. slide_style .. '"'
    end

    -- return the beginning of the slide
    return preface .. "<slide class=\"" .. slide_class .. "\"" .. slide_style .. ">" ..
           "<hgroup" .. hgroup_class .. ">" .. header ..  "</hgroup>" ..
           "<article " .. attributes(attr) .. ">"
  else
    return header
  end

end

function BlockQuote(s)
  -- if this is a list then blockquote means invert the default
  -- incremental behavior
  local prefix = string.sub(s, 1, 3)
  if prefix == "<ol" or prefix == "<ul" then
    if not incremental then
      s = s:gsub(prefix .. ">", prefix .. " class = 'build'>", 1)
    else
      s = s:gsub(prefix .. " class = 'build'>", prefix .. ">", 1)
    end
    return s
  else
    return "<blockquote>\n" .. s .. "\n</blockquote>"
  end
end

function HorizontalRule()
  return Header(2, "", {})
end

function CodeBlock(s, attr)

  -- if this code block has a class then add prettyprint to it
  local class_attrib = ''
  if attr["class"] then
    local class = attr["class"]
    if string.len(class) > 0 then
      class_attrib = "class = 'prettyprint lang-" .. class .. "'"
    end
  end

  -- substitute for code highlighting/emphasis
  local code = escape(s)
  code_sub = code:gsub("[ \t]*[#/]+[ \t]*&lt;b&gt;[ \t]*\n", "<b>")
  code = code_sub:gsub("[ \t]*[#/]+[ \t]*&lt;/b&gt;[ \t]*\n", "</b>")
  code = code:gsub("[ \t]*[#/]+[ \t]*&lt;/b&gt;[ \t]*$", "</b>")

  return "<pre " .. class_attrib .. ">" .. code .. "</pre>"
end

function BulletList(items)
  local buffer = {}
  for _, item in pairs(items) do
    table.insert(buffer, "<li>" .. item .. "</li>")
  end
  list_class = ""
  if incremental then
    list_class = " class = 'build'"
  end
  return "<ul" .. list_class .. ">\n" .. table.concat(buffer, "\n") .. "\n</ul>"
end

function OrderedList(items)
  local buffer = {}
  for _, item in pairs(items) do
    table.insert(buffer, "<li>" .. item .. "</li>")
  end
  list_class = ""
  if incremental then
    list_class = " class = 'build'"
  end
  return "<ol" .. list_class .. ">\n" .. table.concat(buffer, "\n") .. "\n</ol>"
end

-- Revisit association list STackValue instance.
function DefinitionList(items)
  local buffer = {}
  for _,item in pairs(items) do
    for k, v in pairs(item) do
      table.insert(buffer,"<dt>" .. k .. "</dt>\n<dd>" ..
                        table.concat(v,"</dd>\n<dd>") .. "</dd>")
    end
  end
  return "<dl>\n" .. table.concat(buffer, "\n") .. "\n</dl>"
end

-- Convert pandoc alignment to something HTML can use.
-- align is AlignLeft, AlignRight, AlignCenter, or AlignDefault.
function html_align(align)
  if align == 'AlignLeft' then
    return 'left'
  elseif align == 'AlignRight' then
    return 'right'
  elseif align == 'AlignCenter' then
    return 'center'
  else
    return 'left'
  end
end

-- Caption is a string, aligns is an array of strings,
-- widths is an array of floats, headers is an array of
-- strings, rows is an array of arrays of strings.
function Table(caption, aligns, widths, headers, rows)
  local buffer = {}
  local function add(s)
    table.insert(buffer, s)
  end
  add("<table class = 'rmdtable'>")
  if caption ~= "" then
    add("<caption>" .. caption .. "</caption>")
  end
  if widths and widths[1] ~= 0 then
    for _, w in pairs(widths) do
      add('<col width="' .. string.format("%f%%", w * 100) .. '" />')
    end
  end
  local header_row = {}
  local empty_header = true
  for i, h in pairs(headers) do
    local align = html_align(aligns[i])
    table.insert(header_row,'<th align="' .. align .. '">' .. h .. '</th>')
    empty_header = empty_header and h == ""
  end
  if empty_header then
    head = ""
  else
    add('<tr class="header">')
    for _,h in pairs(header_row) do
      add(h)
    end
    add('</tr>')
  end
  local class = "even"
  for _, row in pairs(rows) do
    class = (class == "even" and "odd") or "even"
    add('<tr class="' .. class .. '">')
    for i,c in pairs(row) do
      add('<td align="' .. html_align(aligns[i]) .. '">' .. c .. '</td>')
    end
    add('</tr>')
  end
  add('</table>')
  return table.concat(buffer,'\n')
end

function Div(s, attr)
  if attr["class"] == "notes" then
    return "<aside class='note'><section>" .. s ..
           "</section></aside>"
  else
    return "<div" .. attributes(attr) .. ">\n" .. s .. "</div>"
  end
end

function Span(s, attr)
  return "<span" .. attributes(attr) .. ">\n" .. s .. "</span>"
end

function RawInline(f, s)
  if f == "html" then
    return s
  else
    return ''
  end
end

function RawBlock(f, s)
  if f == "html" then
    return s
  else
    return ''
  end
end

-- The following code will produce runtime warnings when you haven't defined
-- all of the functions you need for the custom writer, so it's useful
-- to include when you're working on a writer.
local meta = {}
meta.__index =
  function(_, key)
    io.stderr:write(string.format("WARNING: Undefined function '%s'\n",key))
    return function() return "" end
  end
setmetatable(_G, meta)
