--[[       liblc/const.lua

--- Collection of the constant values.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

           module 'const'
--]]

--[[!!
_C = require 'liblc.const'

-- charge of electron
ans = _C.e                  --~ 1.602e-19

-- units has postfix _u
ans = _C.e_u                --> 'C'
]]

--	LOCAL

local help = lc_version and (require "liblc.help") or {new=function () return {} end}

local CONST = help.CONST

--	MODULE

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
eps0_u='F/m',       eps0 = 8.85418781871E-12,       -- permittivity of a vacuum
sigma_u='W/(m^2*K^4)', sigma = 5.6704E-8,           -- Stefan-Boltzmann constant
Rinf_u='1/m',       Rinf = 10973731.56852773,       -- Rydberg constant
   -- astronomy
pc_u='m',           pc = 3.08567758128E16,          -- one parsec
ly_u='m',           ly = 9.4607304725808E15,        -- light year
   -- math
                    phi  = 0.5*(1+math.sqrt(5)),    -- golden ratio
		    EuMa = 0.5772156649015328606065120, -- gamma
   --
                    Adams= 42,                      -- ;)

-- description
about = help:new("Collection of constant values.")
}

-- physics
const.about[const.G] = {"G", "Gravitational constant.", CONST}
const.about[const.e] = {"e", "Electron charge.", CONST}
const.about[const.mu0] = {"mu0", "Permeability of free space.", CONST}
const.about[const.R] = {"R", "Universal gas constant.", CONST}
const.about[const.Vm] = {"Vm", "Volume of one mole of ideal gas.", CONST}
const.about[const.NA] = {"NA", "Avogadro's number.", CONST}
const.about[const.k] = {"k", "Boltzmann's constant.", CONST}
const.about[const.h] = {"h", "Planck's constant.", CONST}
const.about[const.c] = {"c", "Speed of light.", CONST}
const.about[const.g] = {"g", "Acceleration of free fall.", CONST}
const.about[const.eps0] = {"eps0", "Permittivity of free space.", CONST}
const.about[const.sigma] = {"sigma", "Stefan-Boltzmann constant.", CONST}
const.about[const.Rinf] = {"Rinf", "Rydberg constant", CONST}
-- astronomy
const.about[const.pc] = {"pc", "One parsec.", CONST}
const.about[const.ly] = {"ly", "One light year.", CONST}
-- mathematics
const.about[const.phi] = {"phi", "Golden ratio.", CONST}
const.about[const.EuMa] = {"EuMa", "Difference between harmonic series and the natural logarithm.", CONST}
--
const.about[const.Adams] = {"Adams", "Answer to the ultimate question of life, the Universe, and Everything.", CONST}

-- free memory in case of standalone usage
if not lc_version then const.about = nil end

return const
