#!/usr/local/bin/lua

-- Lua based calculator
-- This file is a part of 'sonata.lib' collection, 2017 - 2022.

--================= CONFIGURATION ====================

--	Path ('sonata.lua' location, optional)
--SONATA_ADD_PATH = 'path/to/dir/'

--	Text coloring
--SONATA_USE_COLOR = true

--	Load after start (optional)
--SONATA_DEFAULT_MODULES = {'matrix','numeric'}

--	Uncomment to set the localization file
--SONATA_LOCALIZATION = 'ru.lng'

--	Decode text for Windows
--SONATA_WIN_CODE = 'cp866'

--=====================  CODE  ========================

-- Modules
use = {
--  name     alias
  asciiplot = "Ap",
  bigint    = "Int",
  complex   = "_Z",
  const     = "_C",
  data      = "_D",
  graph     = "Graph",
  matrix    = "Mat",
  numeric   = "Num",
  polynomial = "Poly",
  rational  = "Rat",
  special   = "Spec",
  symbolic  = "Sym",
  units     = "Unit",
--
  array     = "Arr",
  geodesy   = "Geo",
  gnuplot   = "Gp",
  lens      = "Lens",
  quaternion = "Quat",
}

-- Add path to the libraries
if SONATA_ADD_PATH then
  package.path = string.format("%s;%s?.lua", package.path, SONATA_ADD_PATH)
end

require('core.load')
