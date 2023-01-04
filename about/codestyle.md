# Sonata code style elements

This file contains rules to make the system more homogeneous, readable and reusable.

## General 

* maximal line width is 80 characters (when possible)
* use camelCase naming style
* avoid global variables
* always initialize variables
* avoid trailing whitespaces
* use whitespace after a coma symbol

## Object elements

* begin private method names from underscore 
* avoid "default" argument values in private methods
* public mehtods must be always called with ':' sign; a user shouldn't think which sign to choose for the method call 
* use dot notation only for access to the object members and only when it is safe

## Mutable and immutable objects

* numeric objects (complex, rational etc.) should be immutable; it allows to avoid deep copy when work with containers and in some operations
* public function should return either a deep copy or a new value 
* the function that modifies the internal state of an object should not return reference to it

## Comments

* use LuaDoc comments
* at least 2 whitespaces between code and line comment
