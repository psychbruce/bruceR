# Timer (compute time difference).

Timer (compute time difference).

## Usage

``` r
dtime(t0, unit = "secs", digits = 0)
```

## Arguments

- t0:

  Time at the beginning.

- unit:

  Options: `"auto"`, `"secs"`, `"mins"`, `"hours"`, `"days"`, `"weeks"`.
  Defaults to `"secs"`.

- digits:

  Number of decimal places of output. Defaults to `0`.

## Value

A character string of time difference.

## Examples

``` r
if (FALSE) { # \dontrun{

t0 = Sys.time()
dtime(t0)
} # }
```
