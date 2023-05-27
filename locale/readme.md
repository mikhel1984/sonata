# Localization Files

Language files for **Sonata**. 

## Add new language

Template for a new translation file can be generated with command 

    lua sonata.lua --lang xx

where "xx" is the language designation. Then open the file locale/xx.lng, uncomment desired strings and replace English text with your translation.

## Configure localization

In order to change language of the program, write desired file name into the parameter __SONATA_LOCALIZATION__ in the head of __sonata.lua__ file. 
For example,

    SONATA_LOCALIZATION = "ru.lua"

## Update translation 

If the list of functions in any module have been changed, the translation file can be updated with the same command as for template generation:

    lua sonata.lua --lang xx

## Current localization

Call of __lang__ option without file name allows to get information about the current localization file:

    lua sonata.lua --lang
