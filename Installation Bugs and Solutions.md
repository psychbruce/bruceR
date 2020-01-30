# Installation Bugs and Solutions

All the bugs DO NOT have relation with the `bruceR` package *per se*.

您在安装过程中遇到的所有问题**均不是**`bruceR`包本身的问题！

**强烈建议**先安装好`tidyverse`和`ggstatsplot`两个包，并且更新一遍已安装的所有R包，然后再安装`bruceR`！
```r
install.packages("devtools")
install.packages("tidyverse")
install.packages("ggstatsplot")
update.packages(ask=F)

devtools::install_github("psychbruce/bruceR")
```

注：R包一般都会依赖于其他R包，在安装时会先自动安装其他R包，然后再安装自身。`bruceR`依赖于`tidyverse`、`ggstatsplot`等10+个R包（[查看完整列表](https://github.com/psychbruce/bruceR/blob/master/DESCRIPTION)），而`tidyverse`和`ggstatsplot`又依赖于一共200+个更基础的R包，所以如果直接安装`bruceR`，你会发现有非常多的R包需要安装，而且一旦安装某些其他包时出错（常见的容易安装出错的基础R包例如`rlang`、`gsl`、`Rcpp`等），则前功尽弃，又要重新下载一遍，并且依然有可能报错，浪费了大量时间。这是因为，`devtools::install_github()`函数在安装GitHub上的R包时有一个缺陷，即当安装依赖的其他R包失败时，该函数无法很好地处理安装失败的状况，还会返回一个没有任何信息含量的报错信息（`Error: Failed to install 'bruceR' from GitHub`）。因此，强烈建议用户先更新完所有的R包（有的R包则需要手动卸载重装，见Bug #05），再正常安装`bruceR`，这将会大大节省您的时间！


## Bug #01:
```
> install.packages("bruceR")
Warning in install.packages :
  package ‘bruceR’ is not available (for R version 3.6.1)
```
### Solution:
Use `devtools::install_github("psychbruce/bruceR")`.
DO NOT use `install.packages("bruceR")`.

由于`bruceR`并没有发布在CRAN官网上，所以请通过`devtools`包来安装，而不能直接使用`install.packages()`函数。


## Bug #02:
```
> devtools::install_github("psychbruce/bruceR")
Downloading GitHub repo psychbruce/bruceR@master
Error in utils::download.file(url, path, method = method, quiet = quiet,  : 
  cannot open URL 'https://api.github.com/repos/psychbruce/bruceR/tarball/master'
```
### Solution:
Check your network connections, or see https://ask.csdn.net/questions/713186.

安装前先确认连接好了网络。如果还是不行，可以看看上面这个链接。


## Bug #03:
```
WARNING: Rtools is required to build R packages, but is not currently installed.

Please download and install Rtools 3.5 from http://cran.r-project.org/bin/windows/Rtools/

Error in parse_repo_spec(repo) : Invalid git repo specification: 'bruceR'
```
### Solution:
Download and install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).

Rtools不是一个R包，而是一个额外的工具，最好安装。


## Bug #04:
```
* installing *source* package 'bruceR' ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
错误: (由警告转换成)程辑包'rio'是用R版本3.6.1 来建造的
停止执行
ERROR: lazy loading failed for package 'bruceR'
Error: Failed to install 'bruceR' from GitHub:
  (converted from warning) installation of package ‘bruceR’ had non-zero exit status
```
or
```
* installing *source* package 'bruceR' ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
错误: (由警告转换成)程辑包'dplyr'是用R版本3.6.1 来建造的
停止执行
ERROR: lazy loading failed for package 'bruceR'
Error: Failed to install 'bruceR' from GitHub:
  (converted from warning) installation of package ‘bruceR’ had non-zero exit status
```
### Solution:
Update R to the [latest version](https://cran.r-project.org/), because the latest versions of some packages (e.g., `rio`, `dplyr`) also require the latest version of R.

Tips: You can use the `installr` package to copy all your installed packages from the old folder to the new one.
```r
install.packages("installr")
library(installr)
copy.packages.between.libraries(ask=TRUE)
```


## Bug #05:
```
Error: Failed to install 'bruceR' from GitHub:
  (converted from warning) installation of package ‘rlang’ had non-zero exit status
```
or
```
Error: Failed to install 'bruceR' from GitHub:
  (converted from warning) cannot remove prior installation of package ‘Rcpp’
```
### Solution:
Use the RStudio menu bar [**`Packages -> Update`**](https://shimo.im/docs/YWwKvcRgqWRdh3HR) or the code `update.packages(ask=F)` to update all the other packages before you install `bruceR`.

Sometimes you have to first use `remove.packages()` to uninstall the old packages (e.g., `rlang`, `gsl`, `Rcpp`) and then use `install.packages()` to reinstall them. Sometimes you also have to restart RStudio and try these again.

这个Bug是用户最常遇到的。很多基础包在更新的时候，容易出现更新失败的情况，目前发现`rlang`、`gsl`、`Rcpp`等最容易安装失败/更新失败（`bruceR`作者表示同样很无奈）。此时，请尝试手动卸载、重装这些基础R包（可以使用代码，也可以使用RStudio面板上的**`Packages`**来管理）。终极解决方案就是**手动卸载重装**、**手动卸载重装**、**手动卸载重装**！

