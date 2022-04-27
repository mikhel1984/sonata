# Sonata
**Sonata** is a **Lua** based program for mathematical computations. It is also a library of objects and functions for solving different mathematical tasks. Each module is independent of others and can be used standalone.

![Screenshot](https://user-images.githubusercontent.com/20392522/51679461-17989b00-1ff0-11e9-83f8-922a3356f505.png)

## Features

* Pure Lua code
* Variety of modules
* Generating templates for new modules and localization files
* Interactive execution of _note_-files

## Getting started

To run the **Sonata**, call

    lua sonata.lua

To get a brief introduction, call

    lua sonata.lua notes/intro.note

Use flag _-h_ to see additional options:

    lua sonata.lua -h

## Structure 

Main components:
* _core_ - interpreter, help and test systems, other program specific elements 
* _sonata.lua_ - program configuration and loading 

Additional components:
* _lib_ - collection of mathematical libraries 
* _locale_ - localization files 
* _notes_ - predefined _note_-files 

## Dependencies 

**Sonata** works in a standard **Lua 5.x** interpreter, version 5.3+ is preferable. 

Module dependencies:
* _gnuplot.lua_ requires [gnuplot](http://www.gnuplot.info/) program to be installed

