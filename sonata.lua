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
--  Windows: to use Unicode symbols call in console
--    chcp 65001
--SONATA_LOCALIZATION = 'ru.lua'


--===================  MODULES  =======================

-- List of modules in the 'lib' library.
-- Aliases can be changed.
use = {
--  name       alias
  asciiplot  = "Ap",     -- use pseudo-graphic to print figures
  bigint     = "Int",    -- manipulations with integers
  complex    = "_Z",     -- complex numbers
  data       = "_D",     -- data processing
  graph      = "Graph",  -- operations with graphs
  main       = "Main",   -- common functions
  matrix     = "Mat",    -- matrices and linear algebra
  numeric    = "Num",    -- numeric algorithms
  polynomial = "Poly",   -- operations with polynomials and splines
  random     = "Rand",   -- random number generators
  rational   = "Rat",    -- rational numbers and continued fractions
  symbolic   = "Sym",    -- some symbolic methods
  units      = "_U",     -- units conversation
--------------------
  const      = "_C",     -- physical and other constants
  geodesy    = "Geo",    -- geodesy transformations
  gnuplot    = "Gp",     -- GnuPlot 'binding'
  lens       = "Lens",   -- paraxial optics and laser beams
  quaternion = "Quat",   -- working with quaternions
  special    = "Spec",   -- list of 'special' functions
}


--=====================  RUN  =========================

if SONATA_ADD_PATH then
  package.path = string.format("%s;%s?.lua", package.path, SONATA_ADD_PATH)
end
require('core.load')
