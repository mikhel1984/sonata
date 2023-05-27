# __Note__-files

## About

**Sonata** can work with the modification of **Lua** scripts called "notes". These files emulate interaction with user:
* script is splited into blocks, each block has tile and is finished with __pause__ keyword
* each block is executed line by line, the interpreter prints current string and result of evaluation (if any)
* after each block the user can enter arbitrary command, empty line leads to execution of the next block
* string comments are considered as "description" and visualized as well

## File syntax 

In general, syntax is the same as in **Lua** but with small additions. 
* to break a long expression into several lines the symbol \ should be used 
* title is the first line of a block
* "_-- [tab] Some text_" allows to highlight text in colored mode
* "_-- PAUSE_" break the execution and switch program to interactive mode (end of a block)
* to hide any part of file use multiline comments

For example, you have following _note_-file
```lua
--    Test
a = 1
b = 2
-- get result
a + b
-- long line
a + \
b   -- add comments if need
-- PAUSE
```
The first line is a title, second and third will be executed (and shown) but without any result, the fourth line will be printed as is, then you will see the result of sum operation, new text and the result for the next two lines. At the _PAUSE_ line the program will be switched into interactive mode.

## List of files
* intro.note - short introduction to Sonata interface and components.
* intro_ru.note - Russian translation of the introduction.
* intro_eo.note - Esperanto translation of the introduction.
* modules.note - short description of Sonata modules.