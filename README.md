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

## Caveats

- Doing `s^(2)` doesn't work, but you must do `s^(1/2)` or `s^(-1/2)` if you want fractions (not bothered to fix this)

