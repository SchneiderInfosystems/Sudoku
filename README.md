# Sudoku Solver as Firemonkey Education Application


## Introduction 
This is an education Delphi project prepared for Delphi 10.3 Rio. This application is prepared to run on Windows 32/64, and on MacOS 64, and on Android and on iOS devices. 

Thanks to the integrated half-solver, all 3 rules are checked continuously during input and only those digits are automatically available that do not cause rule conflicts.

There are plans to add a full solver based on a backtracking algorithm to this application to explain the simplicity of the algorithm to the students.

## Precondition

1. You can start with the Community Edition of Delphi. 
See also https://www.embarcadero.com/products/delphi/starter/free-download 

2. You should be familiar with the rules of the math game sudoku.
See also https://en.wikipedia.org/wiki/Sudoku

## Getting started
1. Clone this repo;
2. Open the project _Sudoku.dpr_ in the Delphi IDE;
3. Press menu command Run in the Delphi IDE;

![Start Form on Max](/InitialForm.png)

4. Click in a number field __(9)__ and select the first digit according to your first Sudoku example;
5. Enter field by field until all of the predefined fields are filled out;
6. Hint during entering the fields: For a simple Sudokus, the half-solver automatically fills all digits with the background color green as soon as only one digit can be selected;
7. After filling in all the predefined fields, you can start to solve the Sudoku;
8. Hint for solving: Begin with the smaller sets and choose a digit. Check the impact of your selected option in the matrix;
9. In the case of a rule violation, you can use the undo button;
10. The button __Init__ let you start a new game;
11. The button __Save__ let you store the current matrix into a text file;
12. With the button __Load__ you can restore the former game stage.

![Sudoku Example](/Example.png)


## Hint for field visualzation
* (3..9): Number of selectable digits;
* {Digit1, Digit2}: If only two digits can be selected, these are displayed directly as a set;
* Field with back color __white__: Entered digit;
* Field with back color __green__: Calculated digit;
* Field with back color __red__: Selected field in order to choose a digit;
* Field with back color __pink__: Role conflict in the horizontal line, the vertical line or within the 3x3 box.
