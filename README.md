A shitty dimensional analysis calculator in haskell

## Disclaimer

- hastily written in a day
- not extensively tested
- written by a haskell noob
- not very consistent syntax

## Usage

The programme takes in the input string as the first command line argument.
`???` stands for the unknown unit you're trying to solve for. Multiplication is using spaces (` `), division is by `/`,
associativity is preserved.

I think its easiest to show some examples:

- `Wb = ???`
- `kg m ??? = kg m s^-2`
- `s^-8 kg^3 m^3 A^-4 ??? / ( kg^2 m^4 s^-6 A^-4 ) = kg m^2 s^-2`
- `( kg / (N / m) )^(1/2) = ???`

You can run this like any other cabal project, `cabal run dim-analysis-tool -- '<your input>'`.

The derived SI units on [this wikipedia page](https://en.wikipedia.org/w/index.php?title=SI_derived_unit&oldid=1087797097#Derived_units_with_special_names) are supported
(with the ohm literally spelt `ohm` instead of the unicode omega).

The parser is case sensitive but extra whitespace shouldn't be an issue.

## Caveats

- Doing `s^(2)` doesn't work, but you must do `s^(1/2)` or `s^(-1/2)` if you want fractions (not bothered to fix this)

