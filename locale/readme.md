# Sonata LC Localization Files

Language files for **Sonata**. 

## Set localization

In order to change language of the program, write desired name into parameter __SONATA_LOCALIZATION__ in the head of file __sonata.lua__. 
For example, 

    SONATA_LOCALIZATION = "ru.lng"

## Add new language 

Generate new file using command 

    lua sonata.lua --lang xx

where "xx" is the language short name. Then open the file locale/xx.lang, uncomment desired (or all) strings and replace english sentences with your translation.

