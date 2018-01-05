--[[       liblc/const.lua

--- Collection of the constant values.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

           module 'const'
--]]


---------------------------------
-- @class table
-- @name const
local const = {
   -- physics
G_u='N*m^2/kg^2',   G    = 6.672041E-11,            -- constant of gravitation
e_u='Cl',           e    = 1.602189246E-19,         -- charge of electron
mu0_u='H/m',        mu0  = 4*math.pi*1E-7,          -- magnetic constant
R_u='J/(mol*K)',    R    = 8.31441,                 -- molar gas constant
Vm_u='m^3/mol',     Vm   = 22.41383E-3,             -- volume of one mol of the ideal gas
NA_u='1/mol',       NA   = 6.02204531E23,
k_u='J/K',          k    = 1.38066244E-23,
h_u='J*s',          h    = 6.62617636E-34,
c_u='m/s',          c    = 2.99792458E8,            -- speed of light
g_u='m/s^2',        g    = 9.80665,
eps0_u='F/m',       eps0 = 8.85418781871E-12,  
}

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
const.about = help:new("Collection of constant values.")

const.about[const.G] = {"G", "Constant of gravitation.", help.CONST}
const.about[const.e] = {"e", "The electron charge.", help.CONST}
const.about[const.mu0] = {"mu0", "Magnetic constant.", help.CONST}
const.about[const.R] = {"R", "Molar gas constant.", help.CONST}
const.about[const.Vm] = {"Vm", "Volume of one mole of ideal gas.", help.CONST}

const.about[const.c] = {"c", "Speed of light.", help.CONST}



--[[
const.__index = const
-- mark
const.type = 'const'
const.isconst = true
local function isconst(t) return type(t)=='table' and t.isconst end
]]

-- free memory in case of standalone usage
if not lc_version then const.about = nil end

return const
