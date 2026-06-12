--[[
extract-data-uri.lua – extract only data URI images to files for non-HTML output

Pandoc cannot use base64-encoded data URIs in LaTeX/PDF output. This filter
extracts only those images to real files, leaving local file references
unchanged. This avoids `--extract-media`, which copies ALL referenced media
(including local files) and prevents cleanup of the _files directory.

The extraction directory is read from the RMARKDOWN_EXTRACT_MEDIA environment
variable (e.g., `slides_files/figure-latex`), set by rmarkdown before rendering.
]]

dofile(os.getenv 'RMARKDOWN_LUA_SHARED')

local extract_dir = os.getenv("RMARKDOWN_EXTRACT_MEDIA")
if not extract_dir or extract_dir == "" then return {} end
extract_dir = extract_dir:gsub("[/\\]+$", "")

-- Pure-Lua base64 decoder (avoids relying on pandoc.mediabag.fetch, whose
-- behaviour with data URIs varies across pandoc 2.x versions).
local b64chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
local b64lookup = {}
for i = 1, #b64chars do b64lookup[b64chars:sub(i, i)] = i - 1 end

local function decode_base64(data)
  data = data:gsub('[^' .. b64chars .. ']', '')  -- strip whitespace / padding
  local out = {}
  for i = 1, #data - 1, 4 do
    local a = b64lookup[data:sub(i,   i  )] or 0
    local b = b64lookup[data:sub(i+1, i+1)] or 0
    local c = b64lookup[data:sub(i+2, i+2)]
    local d = b64lookup[data:sub(i+3, i+3)]
    local n = a * 0x40000 + b * 0x1000 + (c or 0) * 0x40 + (d or 0)
    out[#out+1] = string.char(math.floor(n / 0x10000))
    if c then out[#out+1] = string.char(math.floor(n / 0x100) % 0x100) end
    if d then out[#out+1] = string.char(n % 0x100) end
  end
  return table.concat(out)
end

local image_index = 0

function Image(img)
  -- Only handle base64 data URIs; leave plain data: URIs and file paths alone.
  local mime_type, b64data = img.src:match("^data:([^;,]+);base64,(.+)$")
  if not mime_type then return img end

  local content = decode_base64(b64data)
  if #content == 0 then return img end

  local ext = (mime_type:match("/([^/;]+)") or "bin")
    :gsub("svg%+xml", "svg"):gsub("jpeg", "jpg")

  -- Use sha1 for a collision-free filename when available; otherwise a counter.
  local stem
  if pandoc.utils and pandoc.utils.sha1 then
    stem = pandoc.utils.sha1(img.src)
  else
    image_index = image_index + 1
    stem = "data-uri-" .. image_index
  end
  local outfile = extract_dir .. "/" .. stem .. "." .. ext

  make_directory(extract_dir)
  local f = io.open(outfile, "wb")
  if f then
    f:write(content)
    f:close()
    img.src = outfile
  end
  return img
end
