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

function Image(img)
  if not img.src:match("^data:") then return img end

  local ok, mime_type, content = pcall(pandoc.mediabag.fetch, img.src)
  if not ok or not mime_type or not content or #content == 0 then return img end

  local ext = (mime_type:match("/([^/;]+)") or "bin")
    :gsub("svg%+xml", "svg"):gsub("jpeg", "jpg")
  local name = (pandoc.utils and pandoc.utils.sha1 and pandoc.utils.sha1(img.src))
    or string.format("data-uri-%x", #content)
  local outfile = extract_dir .. "/" .. name .. "." .. ext

  make_directory(extract_dir)
  local f = io.open(outfile, "wb")
  if f then
    f:write(content)
    f:close()
    img.src = outfile
  end
  return img
end
