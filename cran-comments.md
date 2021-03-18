## Resubmission

In this resubmission, I have fixed the 3 problems pointed out by Gregor Seyer.

1. Used `\donttest{}` to deal with all lengthy examples. In the example of `set.wd()`, I used `\dontrun{}` because RStudio is required for running this function.

2. Ensured that now none of the examples are wrapped in `if(FALSE){}`.

3. Checked all scripts and fixed two places to ensure that the functions do not change the user's options, par, or working directory.

  * `bruceR_stats_01_basic.R`: in lines 405-406, I used `on.exit()`.
    ```
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    ```
  * `bruceR_basic.R`: in lines 127-139, for the function `set.wd()` (which is designed for setting working directory to where the currently opened file is), I modified the function so that it no longer executes `setwd()` directly within the function, but instead will send code containing `setwd("...")` to the R console via `rstudioapi::sendToConsole()`. Users can set whether to execute the code using the `execute` parameter. Hence, now this function does not change the user's working directory within itself, but it explicitly casts a line of code containing `setwd("...")` to the R console to let users be aware of that.

Thank you very much for inspecting the package and hope this version of resubmission can work.


## Test environments

* Windows 10 (local installation), R 4.0.4
* Mac OS 11.2 (user installation), R 4.0.4
* Ubuntu 16.04 (on travis-ci.com), R 4.0.2


## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.


## Downstream dependencies

No reverse dependency currently.
