# C library wrapper

This module simplifies making Lua library from a C source code. It works as follows:

- define header file with description of required functions and additional information for compilation
- call generation as "lua maker/cgen.lua lib.h lib1.c lib2.c etc."

In the case of successfull generation the obtained ".c" file and dynamic library will be placed in the 
current directory. It could be used with "required" command as usual.

The module can work with functions and C structures. Arguments of functions could be on form:

- primitive types (int, float, char), read from Lua numbers
- C structures with primitive types or other structures, read from Lua tables
- arrays of primitive types of structures, read from Lua tables, check size
- pointers to dynamic arrays of different size, read from Lua tables without checking size
- strings in form "const char*", read from Lua strings

Function could return:

- primitive type
- C structure

By default all pointers in function arguments are inputs, this can be changed with function settings (see below).

## Header structure

The wrapper expects to get C header file as a first argument. This is a usual ".h" file with additional
blocks of descriptions in multiline comments. Such blocks must be of the form 
```C
/*{
  key1 = val1,
  key2 = val2,
  val3, val4, val5,
}*/
```
i.e. it is a Lua table inside /* */ symbols. There are 2 types of desciption tables: library settings
and function settings.

### Library settings

Table with a library settings is placed once in any part of the header file. It could have
the following keys:

- **compiler** (required) - compiler name (gcc, clang etc.)
- **include** (optional) - parameters before the list of files, typically contains headers
- **lib** (optional) - parameters after the list of files, typically contains dependencies
- **flags** (optional) - additional compilation flags
- **name** (optional) - library name
- **desciption** (optional) - library description for generated ".c" file

### Function settings 

Table with function settings should be placed in front of each function that expected to be wrapped 
into Lua function. Even an empty table could be used as a marker. Table keys:

- **fname** (optional) - expected Lua name for the function
- **out** (optional) - string with argument names (pointers) to read function outputs
- **inout** (optional) - string with argument names (pointers) to set input and read output
- **function** (optional) - string with the function signature; it could be used to avoid duplication of a function definition in the case additional header files

