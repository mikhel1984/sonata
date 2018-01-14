--[[       liblc/const.lua

--- Collection of the constant values.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

           module 'const'
--]]


---------------------------------
-- @class table
-- @name const
-- @field about Description of functions.

local const = {
   -- physics
G_u='N*m^2/kg^2',   G    = 6.672041E-11,            -- constant of gravitation
e_u='C',            e    = 1.602189246E-19,         -- charge of electron
mu0_u='N/A^2',      mu0  = 4E-7*math.pi,            -- magnetic constant
R_u='J/(mol*K)',    R    = 8.31441,                 -- molar gas constant
Vm_u='m^3/mol',     Vm   = 22.41383E-3,             -- volume of one mole of the ideal gas
NA_u='1/mol',       NA   = 6.02204531E23,           -- Avogadro's number
k_u='J/K',          k    = 1.38066244E-23,          -- Boltzmann constant
h_u='J*s',          h    = 6.62617636E-34,          -- Planck's constant
c_u='m/s',          c    = 2.99792458E8,            -- speed of light
g_u='m/s^2',        g    = 9.80665,                 -- 'standard' acceleration of free fall
eps0_u='F/m',       eps0 = 8.85418781871E-12,       -- permitivity of free space
}

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
const.about = help:new("Collection of constant values.")

const.about[const.G] = {"G", "Gravitational constant.", help.CONST}
const.about[const.e] = {"e", "Electron charge.", help.CONST}
const.about[const.mu0] = {"mu0", "Permeability of free space.", help.CONST}
const.about[const.R] = {"R", "Universal gas constant.", help.CONST}
const.about[const.Vm] = {"Vm", "Volume of one mole of ideal gas.", help.CONST}
const.about[const.NA] = {"NA", "Avogadro's number.", help.CONST}
const.about[const.k] = {"k", "Boltzman's constant.", help.CONST}
const.about[const.h] = {"h", "Planck's constant.", help.CONST}
const.about[const.c] = {"c", "Speed of light.", help.CONST}
const.about[const.g] = {"g", "Acceleration of free fall.", help.CONST}
const.about[const.eps0] = {"eps0", "Permitivity of free space.", help.CONST}


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
