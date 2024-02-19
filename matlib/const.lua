--[[		sonata/lib/const.lua

--- Collection of constants.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2023.

	module 'const'
--]]


--[[TEST_IT

-- use 'const'
C = require 'matlib.const'
-- external dependencies, can be loaded implicitly
require 'matlib.units'  -- convert into Unit object

-- charge of electron
ans = C.phy.e * 1E19         --3>  1.602

-- convert to Unit object (add _U)
e = C.phy.e_U
ans = e:key()                 -->  C.phy.e_u_

-- change units
ans = e['nC']                --3>  C.phy.e * 1E9

-- create "immutable" value
C:add('myConst', 10)
ans = C.myConst               -->  10

-- modification generates error
ans = pcall(function() C.myConst = 0 end)  -->  false

-- remove constant
C:remove('myConst')
ans = C.myConst               -->  nil

--]]


--	LOCAL

-- description
local about = {
__module__ = "Collection of constants."
}


local PHY, ASTRO, MATH = "physics", "astronomy", "math"


--- Call error when user try to modify constant value
local function modifyError () error('Constants are immutable!') end


--	MODULE

-- astronomy
local _astro = {
pc_u_='m',    pc = 3.08567758128E16,     -- one parsec
ly_u_='m',    ly = 9.4607304725808E15,   -- light year
au_u_='m',    au = 149597870700,         -- astronomic unit
               k = 0.01720209895,        -- Gaussian gravitational constant
}


-- math
local _math = {
             phi = 1.6180339887498948482045868,  -- golden ratio
               e = 2.7182818284590452353602875,  -- base of natural logarithm
           gamma = 0.5772156649015328606065121,  -- Euler-Mascheroni constant
}


-- physics
local _phy = {
G_u_='N*m^2/kg^2', G = 6.672041E-11,        -- constant of gravitation
e_u_='C',          e = 1.602189246E-19,     -- charge of electron
mu0_u_='N/A^2',  mu0 = 4E-7*math.pi,        -- magnetic constant
R_u_='J/(mol*K)',  R = 8.31441,             -- molar gas constant
Vm_u_='m^3/mol',  Vm = 22.41383E-3,         -- volume of one mole of the ideal gas
NA_u_='1/mol',    NA = 6.02204531E23,       -- Avogadro's number
k_u_='J/K',        k = 1.38066244E-23,      -- Boltzmann constant
h_u_='J*s',        h = 6.62617636E-34,      -- Planck's constant
c_u_='m/s',        c = 2.99792458E8,        -- speed of light
g_u_='m/s^2',      g = 9.80665,             -- 'standard' acceleration of free fall
eps0_u_='F/m',  eps0 = 8.85418781871E-12,   -- permittivity of a vacuum
sigma_u_='W/(m^2*K^4)', sigma = 5.6704E-8,  -- Stefan-Boltzmann constant
Rinf_u_='1/m',  Rinf = 10973731.56852773,   -- Rydberg constant
Da_u_='kg',       Da = 1.660539066605E-27,  -- unified atomic mass unit (Dalton unit)
}


-- user defined
local _user = {
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
about[_phy.G] = {".phy.G --> 6.7E-11", "Gravitational constant.", PHY}
about[_phy.e] = {".phy.e --> 1.6E-19", "Electron charge.", PHY}
about[_phy.mu0] = {".phy.mu0 --> 1.2E-6", "Permeability of free space.", PHY}
about[_phy.R] = {".phy.R --> 8.31", "Universal gas constant.", PHY}
about[_phy.Vm] = {".phy.Vm --> 2.2E-2", "Volume of one mole of ideal gas.", PHY}
about[_phy.NA] = {".phy.NA --> 6E23", "Avogadro's number.", PHY}
about[_phy.k] = {".phy.k --> 1.4E-23", "Boltzmann's constant.", PHY}
about[_phy.h] = {".phy.h --> 6.6E-34", "Planck's constant.", PHY}
about[_phy.c] = {".phy.c --> 3E8", "Speed of light.", PHY}
about[_phy.g] = {".phy.g --> 9.81", "Acceleration of free fall.", PHY}
about[_phy.eps0] = {".phy.eps0 --> 8.8E-12", "Permittivity of free space.", PHY}
about[_phy.sigma] = {".phy.sigma --> 5.6E-8", "Stefan-Boltzmann constant.", PHY}
about[_phy.Rinf] = {".phy.Rinf --> 1.1E7", "Rydberg constant.", PHY}
about[_phy.Da] = {".phi.Da --> 1.7E-27", "Unified atomic mass unit.", PHY}
-- astronomy
about[_astro.pc] = {".astro.pc --> 3.1E16", "One parsec.", ASTRO}
about[_astro.ly] = {".astro.ly --> 9.5E15", "One light year.", ASTRO}
about[_astro.au] = {".astro.au --> 1.5E11", "Astronomic unit.", ASTRO}
about[_astro.k] = {".astro.k --> 0.017", "Gaussian gravitational constant.", ASTRO}
-- mathematics
about[_math.phi] = {".math.phi --> 1.62", "Golden ratio.", MATH}
about[_math.e] = {".math.e --> 2.72", "Base of the natural logarithm.", MATH}
about[_math.gamma] = {".math.gamma --> 0.577", "Euler-Mascheroni constant.", MATH}


--- Convert to Unit object.
--  @param t Table with the constant.
--  @param sKey The constant name.
--  @return Unit object or nil.
const._unit_ = function (t, sKey)
  if type(sKey) == 'string' and string.find(sKey, '_U$') then
    const.ext_units = const.ext_units or require('matlib.units')
    local name = string.sub(sKey, 1, -3)
    local val = t[name]
    return val and const.ext_units(val, t[name..'_u_'] or '') or nil
  end
end


--- Make value "constant".
--  @param sName Name of constant.
--  @param val  Value of constant.
--  @param sUunit String with units.
const.add = function (self, sName, val, sUnit)
  -- add only new constants
  if _user[sName] then error('Cannot modify '..tostring(sName)) end
  _user[sName] = val
  _user[sName..'_u_'] = sUnit
end
about[const.add] = {':add(name_s, value, [units_s])', 'Create new constant.'}


--- Remove existing constant.
--  @param sName Name of constant.
const.remove = function (self, sName)
  if _user[sName] then
    _user[sName] = nil
    _user[sName..'_u_'] = nil
    return true
  end
  return false
end
about[const.remove] = {':remove(name_s) --> bool', 'Delete user-defined constant.'}


-- Comment to remove descriptions
const.about = about


-- Make objects "immutable"
setmetatable(const,       {__newindex=modifyError,
  __index = function (t, k) return _user[k] or const._unit_(_user, k) end})
setmetatable(const.phy,   {__newindex=modifyError,
  __index = function (t, k) return _phy[k]  or const._unit_(_phy, k) end})
setmetatable(const.astro, {__newindex=modifyError,
  __index = function (t, k) return _astro[k] or const._unit_(_astro, k) end})
setmetatable(const.math,  {__newindex=modifyError,
  __index = function (t, k) return _math[k] or const._unit_(_math, k) end})


return const

--============================================
