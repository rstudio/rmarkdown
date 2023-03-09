--[[
  SHARED LUA FILTER
    This filter contains utility functions that are useful in several filters.

  USAGE
    This script is loaded in other script by an RMARKDOW_LUA_SHARED environment variable
    set inside rmarkdown R package by `render()`.

    Use this line in other script to use this function
      dofile(os.getenv 'RMARKDOWN_LUA_SHARED')

    To use this script directly, use `dofile(<path-to-script>)`

  NOTE
    USAGE could change if Pandoc 2.1.2+ is supported only as PANDOC_SCRIPT_FILE is
    available from other Lua Filters. In this case, path to `shared.lua` could be
    determined from other filters relatively and loaded with `dofile()`

    See https://gist.github.com/cderv/ca24d2681b0a1c4b6f91e0b49d725f36 for an example
    of `import("shared.lua")`

  LICENSE
    MIT License
]]


--[[
  This function tests the pandoc version againt a target version.

  USAGE
    pandocAvailable {2,1}
  NOTE
    For Pandoc 2.7.3, PANDOC_VERSION >= "2.8" would be enough but before 2.7.3
    it is a table object
]]
function pandocAvailable(target)
  -- this function only work for Pandoc 2.1 and above. It returns false is not
  if not PANDOC_VERSION then return false end
  assert(target, "No target version specified in pandocAvailable.")
  -- checking each version number
  -- EPOCH.MAJOR.MINOR.PATCH (https://pvp.haskell.org)
  for i = 1,4 do
    -- some versions do not have all numbers
    if not PANDOC_VERSION[i] or not target[i] then break end
    if PANDOC_VERSION[i] < target[i] then return false end
    if PANDOC_VERSION[i] > target[i] then return true end
  end
  return true
end

-- for debuging purpose
function print_debug(label,obj,iter)
  local debug_mode = os.getenv("DEBUG_PANDOC_LUA") == "TRUE"
  obj = obj or nil
  iter = iter or pairs
  label = label or ""
  label = "DEBUG (from custom-environment.lua): "..label
  if (debug_mode) then
      if not obj then
          print(label.." nil")
      elseif (type(obj) == "string") then
          print(label.." "..obj)
      elseif type(obj) == "table" then
          for k,v in iter(obj) do
              print(label.."id:"..k.. " val:"..v)
          end
      end
  end
  return nil
end


--- Returns the type of a metadata value.
--  Author: Albert Krewinkel
--  Source: https://github.com/pandoc/lua-filters/commit/3057acc52d1d6cfb7134c934a5039ce170bf78fa
--
--  Backward compatible function as retrieving type on Metadata has changed.
--  `pandoc.utils.type()` not available before Pandoc 2.17
--
-- @param v a metadata value
-- @treturn string one of `Blocks`, `Inlines`, `List`, `Map`, `string`, `boolean`
function pandoc_type (v)
  if pandoc.utils.type == nil then
    local metatag = type(v) == 'table' and v.t and v.t:gsub('^Meta', '')
    return metatag and metatag ~= 'Map' and metatag or type(v)
  end
  return pandoc.utils.type(v)
end

