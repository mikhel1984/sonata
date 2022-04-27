#!/usr/local/bin/lua

-- Lua based calculator 
-- This file is a part of 'sonata.lib' collection, 2017 - 2022.

--================= CONFIGURATION ====================

--	Path (optional, for bash alias, e.g. sonata='path/to/sonata.lua')
--SONATA_ADD_PATH = path/to/dir/

--	Text coloring
--SONATA_USE_COLOR = true

--	Load after start (optional)
--SONATA_DEFAULT_MODULES = {'matrix','numeric'}

--	Uncomment to set the localization file
--SONATA_LOCALIZATION = "ru.lng"

--	Decode text for Windows
--SONATA_WIN_CODE = 'cp866'

--=====================  CODE  ========================

-- Modules
use = {
--  name     alias
  array     = "Arr",
  asciiplot = "Ap",
  bigint    = "Int",
  complex   = "Comp",
  const     = "_C",
  geodesy   = "Geo",
  gnuplot   = "Gp",
  graph     = "Graph",
  lens      = "Lens",
  matrix    = "Mat",
  numeric   = "Num",
  polynomial = "Poly",
  quaternion = "Quat",
  rational  = "Rat",
  special   = "Spec",
  stat      = "Stat",
  units     = "Unit",
}

-- Add path to the libraries
if SONATA_ADD_PATH then
  package.path = string.format("%s;%s?.lua", package.path, SONATA_ADD_PATH)
end

require('core.load')
