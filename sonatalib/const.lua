--[[       sonatalib/const.lua

--- Collection of constants.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">sonatalib</a> collection, 2017-2018.

           module 'const'
--]]

--[[TEST

-- import 'const'
_C = require 'sonatalib.const'

-- charge of electron
ans = _C.e                  --~ 1.602e-19

-- units have postfix _u
ans = _C.e_u                --> 'C'
--]]

--	LOCAL

local help = lc_version and (require "sonatalib.help") or {new=function () return {} end}

local PHY, ASTR, MATH = "physics", "astronomy", "math"

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
                    the_answer_to_the_ultimate_question_of_life_the_universe_and_everything = 42,

-- description
about = help:new("Collection of constants.")
}

-- physics
const.about[const.G] = {"G", "Gravitational constant.", PHY}
const.about[const.e] = {"e", "Electron charge.", PHY}
const.about[const.mu0] = {"mu0", "Permeability of free space.", PHY}
const.about[const.R] = {"R", "Universal gas constant.", PHY}
const.about[const.Vm] = {"Vm", "Volume of one mole of ideal gas.", PHY}
const.about[const.NA] = {"NA", "Avogadro's number.", PHY}
const.about[const.k] = {"k", "Boltzmann's constant.", PHY}
const.about[const.h] = {"h", "Planck's constant.", PHY}
const.about[const.c] = {"c", "Speed of light.", PHY}
const.about[const.g] = {"g", "Acceleration of free fall.", PHY}
const.about[const.eps0] = {"eps0", "Permittivity of free space.", PHY}
const.about[const.sigma] = {"sigma", "Stefan-Boltzmann constant.", PHY}
const.about[const.Rinf] = {"Rinf", "Rydberg constant", PHY}
-- astronomy
const.about[const.pc] = {"pc", "One parsec.", ASTR}
const.about[const.ly] = {"ly", "One light year.", ASTR}
-- mathematics
const.about[const.phi] = {"phi", "Golden ratio.", MATH}
const.about[const.EuMa] = {"EuMa", "Difference between harmonic series and the natural logarithm.", MATH}

-- free memory in case of standalone usage
if not lc_version then const.about = nil end

return const

--============================================
