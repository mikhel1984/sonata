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

-- modification generate error
ans = pcall(function() _C.e = 0 end)  --> false

-- as well as addition
ans = pcall(function() _C.myConst = 10 end) --> false

-- create "immutable" value
_C.add('myConst', 10)
ans = _C.myConst            --> 10

-- remove constant
_C.remove('myConst')
ans = _C.myConst            --> nil
 
--]]

--	LOCAL

local help = lc_version and (require "sonatalib.help") or {new=function () return {} end}

local PHY, ASTR, MATH = "physics", "astronomy", "math"

--	MODULE

-- Set of constants
_data_ = {
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
}

-- Interface
local const = {
-- description
about = help:new("Collection of constants.")
}

-- physics
const.about[_data_.G] = {"G", "Gravitational constant.", PHY}
const.about[_data_.e] = {"e", "Electron charge.", PHY}
const.about[_data_.mu0] = {"mu0", "Permeability of free space.", PHY}
const.about[_data_.R] = {"R", "Universal gas constant.", PHY}
const.about[_data_.Vm] = {"Vm", "Volume of one mole of ideal gas.", PHY}
const.about[_data_.NA] = {"NA", "Avogadro's number.", PHY}
const.about[_data_.k] = {"k", "Boltzmann's constant.", PHY}
const.about[_data_.h] = {"h", "Planck's constant.", PHY}
const.about[_data_.c] = {"c", "Speed of light.", PHY}
const.about[_data_.g] = {"g", "Acceleration of free fall.", PHY}
const.about[_data_.eps0] = {"eps0", "Permittivity of free space.", PHY}
const.about[_data_.sigma] = {"sigma", "Stefan-Boltzmann constant.", PHY}
const.about[_data_.Rinf] = {"Rinf", "Rydberg constant", PHY}
-- astronomy
const.about[_data_.pc] = {"pc", "One parsec.", ASTR}
const.about[_data_.ly] = {"ly", "One light year.", ASTR}
-- mathematics
const.about[_data_.phi] = {"phi", "Golden ratio.", MATH}
const.about[_data_.EuMa] = {"EuMa", "Difference between harmonic series and the natural logarithm.", MATH}

--- Make value "constant".
--  @param name Name of constant.
--  @param val  Value of constant.
const.add = function (name,val)
   -- add only new constants
   if _data_[name] then error('Cannot modify '..tostring(name)) end
   _data_[name] = val
   print('Done')
end
const.about[const.add] = {'add(name,value)','Create new constant.'}

--- Remove exixting constant.
--  @param name Name of constant.
const.remove = function (name)
   if _data_[name] then      
      _data_[name] = nil
      print('Done')
   end
end
const.about[const.remove] = {'remove(name)','Delete constant.'}

-- Define behavior of the "interface".
setmetatable(const, 
{
   -- read existing values
   __index = function (t,k) return _data_[k] end,
   -- don't modify values
   __newindex = function (t,k,v) error('Constants are immutable!') end,
})

-- free memory in case of standalone usage
if not lc_version then const.about = nil end

return const

--============================================
