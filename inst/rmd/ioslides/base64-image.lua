

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

function Image(s, src, tit)
  if string.sub(src, 1, 4) == "http" then
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
