## Resubmission

This is a resubmission. I have fixed all problems pointed out by Julia Haider:

* Removed "| file LICENSE" in DESCRIPTION file and added "^LICENSE$" in .Rbuildignore (otherwise, a NOTE would appear when conducting R CMD check).
* Added more details about the package functionality in the Description text.
* Added all "\value" descriptions in the documentation.
* Used `\dontrun{}` to deal with lengthy examples.
* Ensured that code lines in examples are not commented out.
* Ensured that the functions do not change users' options. One function `set.wd()` was designed to help users set their working directory to where their current R script is. So the working directory will be set to where the users want when this function is exited. To remind users of this change, a message will be printed in the console if they use the `set.wd()` function.
* Prof. Hua Shu is not an author, contributor, or copyright holder. To describe the demo datasets more clearly, I rewrote the help page of `bruceR-demodata` as follows:

> These demo datasets were obtained from the book "\href{https://book.douban.com/subject/1195181/}{Factorial Experimental Design in Psychological and Educational Research}". The book provides these demo datasets to show different experimental designs and how to conduct MANOVA using syntax in SPSS. Here, I use these demo datasets as examples to show how the functions \code{\link{MANOVA}} and \code{\link{EMMEANS}} work.

## Test environments

* Windows 10 (local installation), R 4.0.4
* Mac OS 11.2 (user installation), R 4.0.4
* Ubuntu 16.04 (on travis-ci.com), R 4.0.2

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies

No reverse dependency currently.
