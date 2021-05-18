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
