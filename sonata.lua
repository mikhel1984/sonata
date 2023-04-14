#!/usr/local/bin/lua

-- Lua based calculator
-- This file is a part of 'sonata.lib' collection, 2017 - 2023.

--================= CONFIGURATION ====================

--	Path ('sonata.lua' location)
--SONATA_ADD_PATH = 'path/to/dir/'

--	Text coloring
--SONATA_USE_COLOR = true

--	Load after start
--SONATA_DEFAULT_MODULES = {'matrix','asciiplot'}

--	Unicode symbols for function plot
--SONATA_ASCIIPLOT_UNICODE = true

--	Uncomment to set the localization file
--SONATA_LOCALIZATION = 'eo.lua'
-- Windows: call first
--   chcp 65001 
-- in console to use Unicode symbols

--===================  MODULES  =======================

-- List of modules in the 'lib' library.
-- Aliases can be changed.
use = {
--  name       alias
  asciiplot  = "Ap",
  bigint     = "Int",
  complex    = "_Z",
  data       = "_D",
  graph      = "Graph",
  main       = "Main",
  matrix     = "Mat",
  numeric    = "Num",
  polynomial = "Poly",
  random     = "Rand",
  rational   = "Rat",
  symbolic   = "Sym",
  units      = "Unit",
--------------------
  array      = "Arr",
  const      = "_C",
  geodesy    = "Geo",
  gnuplot    = "Gp",
  lens       = "Lens",
  quaternion = "Quat",
  special    = "Spec",
}

--=====================  RUN  =========================

if SONATA_ADD_PATH then
  package.path = string.format("%s;%s?.lua", package.path, SONATA_ADD_PATH)
end
require('core.load')
