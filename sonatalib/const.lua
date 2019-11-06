--[[       sonatalib/const.lua

--- Collection of constants.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2017-2019.

           module 'const'
--]]

--[[TEST

-- import 'const'
_C = require 'sonatalib.const'

-- charge of electron
ans = _C.phy.e * 1E19          --3> 1.602

-- units have postfix _u
ans = _C.phy.e_u                --> 'C'

-- create "immutable" value
_C.add('myConst', 10)
ans = _C.myConst            --> 10

-- modification generate error
ans = pcall(function() _C.myConst = 0 end)  --> false

-- remove constant
_C.remove('myConst')
ans = _C.myConst            --> nil
 
--]]

--	LOCAL

local help = LC_DIALOG and (require "sonatalib.help") or {new=function () return {} end}

local PHY, ASTRO, MATH = "physics", "astronomy", "math"

--- Call error when user try to modify constant value
local function modifyError () error('Constants are immutable!') end

--	MODULE

-- Set of constants
-- physics
local _phy_ = {
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
}

-- astronomy
local _astro_ = {
pc_u='m',           pc = 3.08567758128E16,          -- one parsec
ly_u='m',           ly = 9.4607304725808E15,        -- light year
}

-- math
local _math_ = { 
                    phi  = 0.5*(1+math.sqrt(5)),    -- golden ratio
		    EuMa = 0.5772156649015328606065120, -- gamma
		    pi   = 3.1415926535897932384626434, -- length to diameter
		    e    = 2.7182818284590452353602875, -- base of natural logarithm
}

-- user defined
local _user_ = {
                    the_answer_to_the_ultimate_question_of_life_the_universe_and_everything = 42,
}

-- Interface
local const = {
phy = {},
astro = {},
math = {},
-- description
about = help:new("Collection of constants."),
}

-- physics
const.about[_phy_.G] = {"phy.G", "Gravitational constant.", PHY}
const.about[_phy_.e] = {"phy.e", "Electron charge.", PHY}
const.about[_phy_.mu0] = {"phy.mu0", "Permeability of free space.", PHY}
const.about[_phy_.R] = {"phy.R", "Universal gas constant.", PHY}
const.about[_phy_.Vm] = {"phy.Vm", "Volume of one mole of ideal gas.", PHY}
const.about[_phy_.NA] = {"phy.NA", "Avogadro's number.", PHY}
const.about[_phy_.k] = {"phy.k", "Boltzmann's constant.", PHY}
const.about[_phy_.h] = {"phy.h", "Planck's constant.", PHY}
const.about[_phy_.c] = {"phy.c", "Speed of light.", PHY}
const.about[_phy_.g] = {"phy.g", "Acceleration of free fall.", PHY}
const.about[_phy_.eps0] = {"phy.eps0", "Permittivity of free space.", PHY}
const.about[_phy_.sigma] = {"phy.sigma", "Stefan-Boltzmann constant.", PHY}
const.about[_phy_.Rinf] = {"phy.Rinf", "Rydberg constant.", PHY}
-- astronomy
const.about[_astro_.pc] = {"astro.pc", "One parsec.", ASTRO}
const.about[_astro_.ly] = {"astro.ly", "One light year.", ASTRO}
-- mathematics
const.about[_math_.phi] = {"math.phi", "Golden ratio.", MATH}
const.about[_math_.EuMa] = {"math.EuMa", "Difference between harmonic series and the natural logarithm.", MATH}
const.about[_math_.pi] = {"math.pi", "Ratio of a circle's circumference to its diameter.", MATH}
const.about[_math_.e] = {"math.e", "Base of the natural logarithm.", MATH}

--- Make value "constant".
--  @param name Name of constant.
--  @param val  Value of constant.
const.add = function (name,val)
   -- add only new constants
   if _user_[name] then error('Cannot modify '..tostring(name)) end
   _user_[name] = val
   print('Done')
end
const.about[const.add] = {'add(name,value)','Create new constant.'}

--- Remove existing constant.
--  @param name Name of constant.
const.remove = function (name)
   if _user_[name] then      
      _user_[name] = nil
      print('Done')
   end
end
const.about[const.remove] = {'remove(name)','Delete constant.'}

-- Make objects "immutable"
setmetatable(const,       {__newindex=modifyError, __index = function (t,k) return _user_[k] end})
setmetatable(const.phy,   {__newindex=modifyError, __index = function (t,k) return _phy_[k] end})
setmetatable(const.astro, {__newindex=modifyError, __index = function (t,k) return _astro_[k] end})
setmetatable(const.math,  {__newindex=modifyError, __index = function (t,k) return _math_[k] end})

-- free memory in case of standalone usage
if not LC_DIALOG then const.about = nil end

return const

--============================================
