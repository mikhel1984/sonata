--[[		sonata/lib/const.lua

--- Collection of constants.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2022.

	module 'const'
--]]

--[[TEST

-- use 'const'
_C = require 'lib.const'
-- external dependencies, can be loaded implicitly
require 'lib.units'  -- convert into Unit object

-- charge of electron
ans = _C.phy.e * 1E19        --3> 1.602

-- convert to Unit object (add _U)
e = _C.phy.e_U
ans = e:key()                 --> _C.phy.e_u_

-- change units
ans = e['nC']                --3> _C.phy.e * 1E9

-- create "immutable" value
_C:add('myConst', 10)
ans = _C.myConst              --> 10

-- modification generates error
ans = pcall(function() _C.myConst = 0 end)  --> false

-- remove constant
_C:remove('myConst')
ans = _C.myConst              --> nil

--]]

--	LOCAL

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Collection of constants.")

local PHY, ASTRO, MATH = "physics", "astronomy", "math"

--- Call error when user try to modify constant value
local function modifyError () error('Constants are immutable!') end

--	MODULE

-- Set of constants

-- astronomy
local _astro_ = {
pc_u_='m',        pc = 3.08567758128E16,     -- one parsec
ly_u_='m',        ly = 9.4607304725808E15,   -- light year
au_u_='m',        au = 149597870700,         -- astronomic unit
}

-- math
local _math_ = { 
                 phi = 1.6180339887498948482045868, -- golden ratio
                  pi = 3.1415926535897932384626434, -- length to diameter
                   e = 2.7182818284590452353602875, -- base of natural logarithm
}

-- physics
local _phy_ = {
G_u_='N*m^2/kg^2', G = 6.672041E-11,       -- constant of gravitation
e_u_='C',          e = 1.602189246E-19,    -- charge of electron
mu0_u_='N/A^2',  mu0 = 4E-7*math.pi,       -- magnetic constant
R_u_='J/(mol*K)',  R = 8.31441,            -- molar gas constant
Vm_u_='m^3/mol',  Vm = 22.41383E-3,        -- volume of one mole of the ideal gas
NA_u_='1/mol',    NA = 6.02204531E23,      -- Avogadro's number
k_u_='J/K',        k = 1.38066244E-23,     -- Boltzmann constant
h_u_='J*s',        h = 6.62617636E-34,     -- Planck's constant
c_u_='m/s',        c = 2.99792458E8,       -- speed of light
g_u_='m/s^2',      g = 9.80665,            -- 'standard' acceleration of free fall
eps0_u_='F/m',  eps0 = 8.85418781871E-12,  -- permittivity of a vacuum
sigma_u_='W/(m^2*K^4)', sigma = 5.6704E-8, -- Stefan-Boltzmann constant
Rinf_u_='1/m',  Rinf = 10973731.56852773,  -- Rydberg constant
Da_u_='kg',       Da = 1,660539066605E-27, -- unified atomic mass unit (Dalton unit)
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
ext_units = false,
}

-- physics
about[_phy_.G] = {"phy.G", "Gravitational constant.", PHY}
about[_phy_.e] = {"phy.e", "Electron charge.", PHY}
about[_phy_.mu0] = {"phy.mu0", "Permeability of free space.", PHY}
about[_phy_.R] = {"phy.R", "Universal gas constant.", PHY}
about[_phy_.Vm] = {"phy.Vm", "Volume of one mole of ideal gas.", PHY}
about[_phy_.NA] = {"phy.NA", "Avogadro's number.", PHY}
about[_phy_.k] = {"phy.k", "Boltzmann's constant.", PHY}
about[_phy_.h] = {"phy.h", "Planck's constant.", PHY}
about[_phy_.c] = {"phy.c", "Speed of light.", PHY}
about[_phy_.g] = {"phy.g", "Acceleration of free fall.", PHY}
about[_phy_.eps0] = {"phy.eps0", "Permittivity of free space.", PHY}
about[_phy_.sigma] = {"phy.sigma", "Stefan-Boltzmann constant.", PHY}
about[_phy_.Rinf] = {"phy.Rinf", "Rydberg constant.", PHY}
about[_phy_.Da] = {"phi.Da", "Unified atomic mass unit.", PHY}
-- astronomy
about[_astro_.pc] = {"astro.pc", "One parsec.", ASTRO}
about[_astro_.ly] = {"astro.ly", "One light year.", ASTRO}
about[_astro_.au] = {"astro.au", "Astronomic unit.", ASTRO}
-- mathematics
about[_math_.phi] = {"math.phi", "Golden ratio.", MATH}
about[_math_.pi] = {"math.pi", "Ratio of a circle's circumference to its diameter.", MATH}
about[_math_.e] = {"math.e", "Base of the natural logarithm.", MATH}

--- Convert to Unit object.
--  @param t Table with the constant.
--  @param sKey The constant name.
--  @return Unit object or nil.
const._unit_ = function (t, sKey)
  if type(sKey) == 'string' and string.find(sKey, '_U$') then
    const.ext_units = const.ext_units or require('lib.units')
    local name = string.sub(sKey, 1, -3)
    local val = t[name]
    return val and const.ext_units(val, t[name..'_u_'] or '') or nil
  end
end

--- Make value "constant".
--  @param self Do nothing.
--  @param sName Name of constant.
--  @param val  Value of constant.
--  @param sUunit String with units.
const.add = function (self,sName,val,sUnit)
  -- add only new constants
  if _user_[sName] then error('Cannot modify '..tostring(sName)) end
  _user_[sName] = val
  _user_[sName..'_u_'] = sUnit
  return 'Done'
end
about[const.add] = {'_C:add(sName,value,[sUnits])','Create new constant.'}

--- Remove existing constant.
--  @param self Do nothing.
--  @param sName Name of constant.
const.remove = function (self,sName)
  if _user_[sName] then
    _user_[sName] = nil
    _user_[sName..'_u_'] = nil
    return 'Done'
  end
end
about[const.remove] = {'_C:remove(sName)','Delete user-defined constant.'}

-- Comment to remove descriptions
const.about = about

-- Make objects "immutable"
setmetatable(const,       {__newindex=modifyError, __index = function (t,k) return _user_[k] or const._unit_(_user_, k) end})
setmetatable(const.phy,   {__newindex=modifyError, __index = function (t,k) return _phy_[k]  or const._unit_(_phy_, k) end})
setmetatable(const.astro, {__newindex=modifyError, __index = function (t,k) return _astro_[k] or const._unit_(_astro_, k) end})
setmetatable(const.math,  {__newindex=modifyError, __index = function (t,k) return _math_[k] or const._unit_(_math_, k) end})

return const

--============================================
