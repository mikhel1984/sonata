--[[		sonata/core/win.lua

--- Translate utf8 into DOS code pages.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2023.

	module 'win'
]]

local code = {}

-- first 128 characters
local cp437 = {
--  0       1       2       3       4       5       6       7       8       9       A       B       C       D       E      F
 [0]=0,   0x01,   0x02,   0x03,   0x04,   0x05,   0x06,   0x07,   0x08,   0x09,   0x0A,   0x0B,   0x0C,   0x0D,   0x0E,   0x0F, -- 00  - from 1251
  0x10,   0x11,   0x12,   0x13,   0x14,   0x15,   0x16,   0x17,   0x18,   0x19,   0x1A,   0x1B,   0x1C,   0x1D,   0x1E,   0x1F, -- 10  - from 1251
  0x20,   0x21,   0x22,   0x23,   0x24,   0x25,   0x26,   0x27,   0x28,   0x29,   0x2A,   0x2B,   0x2C,   0x2D,   0x2E,   0x2F, -- 20
  0x30,   0x31,   0x32,   0x33,   0x34,   0x35,   0x36,   0x37,   0x38,   0x39,   0x3A,   0x3B,   0x3C,   0x3D,   0x3E,   0x3F, -- 30
  0x40,   0x41,   0x42,   0x43,   0x44,   0x45,   0x46,   0x47,   0x48,   0x49,   0x4A,   0x4B,   0x4C,   0x4D,   0x4E,   0x4F, -- 40
  0x50,   0x51,   0x52,   0x53,   0x54,   0x55,   0x56,   0x57,   0x58,   0x59,   0x5A,   0x5B,   0x5C,   0x5D,   0x5E,   0x5F, -- 50
  0x60,   0x61,   0x62,   0x63,   0x64,   0x65,   0x66,   0x67,   0x68,   0x69,   0x6A,   0x6B,   0x6C,   0x6D,   0x6E,   0x6F, -- 60
  0x70,   0x71,   0x72,   0x73,   0x74,   0x75,   0x76,   0x77,   0x78,   0x79,   0x7A,   0x7B,   0x7C,   0x7D,   0x7E, 0x2302, -- 70
}
for i = 0, 127 do code[cp437[i]] = string.char(i) end


if SONATA_DOS_PAGE == 866 then  -- Cyrillic symbols

local cp866 = {
--   0       1       2       3       4       5       6       7       8       9       A       B       C       D       E      F
[0]=0x410,  0x411,  0x412,  0x413,  0x414,  0x415,  0x416,  0x417,  0x418,  0x419,  0x41A,  0x41B,  0x41C,  0x41D,  0x41E,  0x41F, -- 80
    0x420,  0x421,  0x422,  0x423,  0x424,  0x425,  0x426,  0x427,  0x428,  0x429,  0x42A,  0x42B,  0x42C,  0x42D,  0x42E,  0x42F, -- 90
    0x430,  0x431,  0x432,  0x433,  0x434,  0x435,  0x436,  0x437,  0x438,  0x439,  0x43A,  0x43B,  0x43C,  0x43D,  0x43E,  0x43F, -- A0 
   0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556, 0x2555, 0x2563, 0x2552, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510, -- B0
   0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F, 0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567, -- C0
   0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B, 0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580, -- D0
    0x440,  0x441,  0x442,  0x443,  0x444,  0x445,  0x446,  0x447,  0x448,  0x449,  0x44A,  0x44B,  0x44C,  0x44D,  0x44E,  0x44F, -- E0
    0x401,  0x451,  0x404,  0x454,  0x407,  0x457,  0x40E,  0x45E,   0xB0, 0x2219,   0xB7, 0x221A, 0x2116,   0xA4, 0x25A0,   0xA0  -- F0
}
for i = 0, 127 do code[cp866[i]] = string.char(i+128) end

elseif SONATA_DOS_PAGE == 853 then  -- Esperanto symbols

local cp853 = {
--   0       1       2       3       4       5       6       7       8      9       A       B       C       D       E      F
[0]=0xC7,   0xFC,   0xE9,   0xE2,   0xE4,   0xE0, 0x109,  0xE7,   0xEA,   0xEB,   0xE8,   0xEF,   0xEE,   0xEC,   0xC4,  0x108, -- 80
    0xC9,  0x10B,  0x10A,   0xF4,   0xFB,   0xF6,  0xF2,  0xF9,  0x130,   0xD6,   0xDC,  0x11D,   0xA3,  0x11C,   0xD7,  0x135, -- 90
    0xE1,   0xED,   0xF3,   0xFA,   0xF1,   0xD1, 0x11E, 0x11F,  0x124,  0x125,    0x0,   0xBD,  0x134,  0x15F,   0xAB,   0xBB, -- A0 
  0x2591, 0x2592, 0x2593, 0x2502, 0x2524,   0xC1,  0xC2,  0xC0,  0x15E, 0x2563, 0x2551, 0x2557, 0x255D,  0x17B,  0x17C, 0x2510, -- B0
  0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x15C, 0x15D, 0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C,   0xA4, -- C0
     0x0,    0x0,   0xCA,   0xCB,   0xC8,   0x49,  0xCD,  0xCE,   0xCF, 0x2518, 0x250C, 0x2588, 0x2584,    0x0,   0xCC, 0x2580, -- D0
    0xD3,   0xDF,   0xD4,   0xD2,  0x120,  0x121, 0x3BC, 0x126,  0x127,   0xDA,   0xDB,   0xD9,  0x16C,  0x16D, 0x2027,   0xB4, -- E0
    0xAD,    0x0,   0x6C,  0x149,  0x2D8,   0xA7,  0xF7,  0xB8,   0xB0,  0x308,  0x307,    0x0,   0xB3,   0xB2, 0x25A0,   0xA0  -- F0
}
for i = 0, 127 do code[cp853[i]] = string.char(i+128) end

end  -- if

-- output
return {
--- Convert text into a Windows console compatible form.
--  @param txt Source text.
--  @return Converted text.
convert = function (txt)
  local res = {}
  for _, c in utf8.codes(txt) do
    res[#res+1] = code[c] or '?'
  end
  return table.concat(res)
end
}

--==============================
--TODO implement for 5.1 and 5.2

