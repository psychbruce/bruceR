## Resubmission (New)

In this resubmission, I have addressed the two points mentioned by Uwe Ligges. (All the 7 problems pointed out by Julia Haider have been fixed in the last resubmission.)

1. The 404 URL in README.md has been removed.

2. "Is there some reference about the method you can add in the Description field in the form Authors (year) <doi:.....>?"

Reply: No, but thanks for reminding me. The current version of this package does not use any novel method that was published in journals. For those useful references that some users may be interested in but were not critical to this package, I have provided references and their DOIs in the documentation of relevant functions.

Thank you for inspecting the package and hope this version of resubmission can work.


## Resubmission (Old)

This is a resubmission. I have fixed all problems pointed out by Julia Haider:

1. Removed "| file LICENSE" in DESCRIPTION file and added "^LICENSE$" in .Rbuildignore (otherwise, a NOTE would appear when conducting R CMD check).

2. Added more details about the package functionality in the Description text.

3. Added all "\value" descriptions in the documentation.

4. Used `\dontrun{}` to deal with lengthy examples.

5. Ensured that code lines in examples are not commented out.

6. Ensured that the functions do not change users' options. One function `set.wd()` was designed to help users set their working directory to where their current R script is. So the working directory will be set to where the users want when this function is exited. To remind users of this change, a message will be printed in the console if they use the `set.wd()` function.

7. Prof. Hua Shu is not an author, contributor, or copyright holder. To describe the demo datasets more clearly, I rewrote the description of `bruceR-demodata` as follows:

> These demo datasets were obtained from the book "\href{https://book.douban.com/subject/1195181/}{Factorial Experimental Design in Psychological and Educational Research}". The book provides these demo datasets to show different experimental designs and how to conduct MANOVA using syntax in SPSS. Here, I use these demo datasets as examples to show how the functions \code{\link{MANOVA}} and \code{\link{EMMEANS}} work.

Thank you for inspecting the package and hope this version of resubmission can work.


## Test environments

* Windows 10 (local installation), R 4.0.4
* Mac OS 11.2 (user installation), R 4.0.4
* Ubuntu 16.04 (on travis-ci.com), R 4.0.2


## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.


## Downstream dependencies

No reverse dependency currently.
