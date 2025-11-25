# Split up a string (with separators) into a character vector.

Split up a string (with separators) into a character vector (whitespace
around separator is trimmed).

## Usage

``` r
cc(..., sep = "auto", trim = TRUE)
```

## Arguments

- ...:

  Character string(s).

- sep:

  Pattern for separation. Defaults to `"auto"`: `,` `;` `|` `\n` `\t`

- trim:

  Remove whitespace from start and end of string(s)? Defaults to `TRUE`.

## Value

Character vector.

## Examples

``` r
cc("a,b,c,d,e")
#> [1] "a" "b" "c" "d" "e"

cc(" a , b , c , d , e ")
#> [1] "a" "b" "c" "d" "e"

cc(" a , b , c , d , e ", trim=FALSE)
#> [1] " a " " b " " c " " d " " e "

cc("1, 2, 3, 4, 5")
#> [1] "1" "2" "3" "4" "5"

cc("A 1 , B 2 ; C 3 | D 4 \t E 5")
#> [1] "A 1" "B 2" "C 3" "D 4" "E 5"

cc("A, B, C",
   " D | E ",
   c("F", "G"))
#> [1] "A" "B" "C" "D" "E" "F" "G"

cc("
American
British
Chinese
")
#> [1] "American" "British"  "Chinese" 
```
