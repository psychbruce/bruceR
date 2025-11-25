# Check dependencies of R packages.

Check dependencies of R packages.

## Usage

``` r
pkg_depend(pkgs, excludes = NULL)
```

## Arguments

- pkgs:

  Package(s).

- excludes:

  \[Optional\] Package(s) and their dependencies excluded from the
  dependencies of `pkgs`. Useful if you want to see the unique
  dependencies of `pkgs`.

## Value

A character vector of package names.
