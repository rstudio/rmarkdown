
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

-- Lua 5.1+ base64 v3.0 (c) 2009 by Alex Kloss <alexthkloss@web.de>
-- licensed under the terms of the LGPL2
-- character table string
local b='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
function base64encode(data)
    return ((data:gsub('.', function(x)
        local r,b='',x:byte()
        for i=8,1,-1 do r=r..(b%2^i-b%2^(i-1)>0 and '1' or '0') end
        return r;
    end)..'0000'):gsub('%d%d%d?%d?%d?%d?', function(x)
        if (#x < 6) then return '' end
        local c=0
        for i=1,6 do c=c+(x:sub(i,i)=='1' and 2^(6-i) or 0) end
        return b:sub(c+1,c+1)
    end)..({ '', '==', '=' })[#data%3+1])
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

function Image(s, src, tit)
  if not base64_images or (string.sub(src, 1, 4) == "http") then
    -- leave the image alone
    return "<img src='" .. escape(src,true) .. "' title='" ..
            escape(tit,true) .. "'/>"
  else
    -- base64 encode image
    local png_file = io.open(src, "rb")
    local png_data = png_file:read("*all")
    png_file:close()
    png_data = base64encode(png_data)
    return "<img src='data:image/png;base64," .. png_data .. "' title='"  ..
           escape(tit,true) .. "'/>"
  end
end

function CaptionedImage(src, tit, s)
  local caption = ""
  if fig_caption and (string.len(s) > 0) then
    caption = "<p class='caption'>" .. s .. "</p>"
  end
  return Image(s, src, tit) .. caption
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
  if lev == 1 then
    slide_class = "segue dark nobackground"
    hgroup_class = " class = 'auto-fadein'"
    lev = 2
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
      attr["class"] = "smaller " .. attr["class"]
    end

    -- return the beginning of the slide
    return preface .. "<slide class='" .. slide_class .. "'>" ..
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

  -- trim whitespace
  code = code:gsub("^%s*(.-)%s*$", "%1")

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
      add('<col width="' .. string.format("%d%%", w * 100) .. '" />')
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

