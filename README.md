# Sudoku Solver as Firemonkey Education Application

This is an education Delphi project prepared for Delphi 10.3 Rio.

## Precondition

1. You can start with the Community Edition of Delphi. 
See also https://www.embarcadero.com/products/delphi/starter/free-download 

2. You should be familiar with the rules of the math game sudoku.
See also https://en.wikipedia.org/wiki/Sudoku

## Getting started
1. Clone this repo;
2. Open the project _Sudoku.dpr_ in the Delphi IDE;
3. Press menu command Run in the Delphi IDE;
4. Click in a number field __(9)__ and select the first digit;

![Sudoku Example](/Example.png)


## Hint for field content
* Field with back color __white__: Entered digit;
* Field with back color __green__: Calculated digit;
* Field with back color __red__: Selected field in order to choose a digit;
* Field with back color __pink__: Rule conflict;
* (3..9): Number of selectable digits;
* {Digit1, Digit2}: If only two digits can be selected, these are displayed directly as a set.
