# LinearPowerSum
This is a Mathematica script to handle the linear power sums, i.e. the expressions of the form \sum_{i} c_i a_i^m in a symbolic way.

## Usage
Load `linearpowersum.m` into Mathematica.
Then, for example, the expression `a*x^m+b*y^m` can be represented as `linPS[{a,b},{x,y},m]`. It symbolically manipulates such expressions and related basic operations (such as scalar multiplication, addition, multiplication, and exponentiation).
If you evaluate
```
linPS[{a, b}, {x, y}, m]^2
```
then it returns
```
linPS[{a^2, 2 a b, b^2}, {x^2, x y, y^2}, m]
```
Instead of `m`, you can put a linear polynomial in `m` with integer coefficients or an integer. For example,
```
2 linPS[{a, b}, {x, y}, 3 m - 1] + linPS[{c}, {z}, 2 m]
```
returns
```
linPS[{c, (2 a)/x, (2 b)/y}, {z^2, x^3, y^3}, m]
```
Similarly,
```
linPS[{a, b}, {x, y}, 3]
```
returns 
```
a x^3 + b y^3
```


## Sample script
Put the following two files in the same directory :
- `linearpowersum.m` : define `linPS` with its properties
- `KR_character_G2.m` (or `KR_character_A1.m`) : sample Mathematica script for the linear power sum expression of KR modules of type G2 (A1)

To run the sample script (assuming that the Mathematica kernel is on your path):
```
$ math -script KR_character_G2.m
```
