# mathematica-theme-switcher

Originally forked from
[maTHEMEatica](https://gitlab.com/jakobneef/mathemeatica). I've added a
convenience command line script for switching themes.

## Usage 

Theme switching is driven by the command line script: `change-theme`. To use it,
you must have the `wolframscript` binary installed. Running:
```
./change-theme --help
```
Should describe its use. Generally, to change a theme:
```
./change-theme <theme-of-choice>
```
To reset back to the default theme, run with no arguments:
```
./change-theme
```
