# Installation Bugs and Solutions

All the bugs DO NOT have relation with the `bruceR` package *per se*.

您在安装过程中遇到的所有问题**都不是**`bruceR`包本身的问题！

如果有关，也是因为`bruceR`需要安装很多其他的R包，在安装这些R包的时候出错！

**大部分用户可以正常安装**`bruceR`！如果安装过程中遇到问题，请仔细阅读本文档，这里有6种常见bug的解决方案！

**强烈建议**先安装好`tidyverse`和`ggstatsplot`两个包，并且更新一遍已安装的所有R包，然后再安装`bruceR`！
```r
install.packages("devtools")
install.packages("tidyverse")
install.packages("ggstatsplot")
update.packages(ask=F)

devtools::install_github("psychbruce/bruceR")
```

注：R包一般都会依赖于其他R包，在安装时会先自动安装其他R包，然后再安装自身。`bruceR`依赖于`tidyverse`、`ggstatsplot`等10+个R包（[查看完整列表](https://github.com/psychbruce/bruceR/blob/master/DESCRIPTION)），而`tidyverse`和`ggstatsplot`又依赖于一共200+个更基础的R包，所以如果直接安装`bruceR`，你会发现有上百个R包需要安装，而且一旦安装出错（容易安装出错的基础R包例如`rlang`、`gsl`、`Rcpp`等），则前功尽弃，又要重新下载一遍，并且依然有可能报错，浪费了大量时间。出现上述情况主要是因为：1）这些基础R包的安装编译过程可能比较复杂，容易出现未知的bug；2）`devtools::install_github()`函数在安装GitHub上的R包时有一个缺陷，即无法很好地处理安装其他R包失败的状况，还会返回一个没有任何信息含量的报错信息（`Error: Failed to install 'bruceR' from GitHub`），使用户认为是`bruceR`本身的问题。因此，强烈建议用户先更新完所有的R包（有的R包则需要手动卸载重装，见[Bug #03](https://github.com/psychbruce/bruceR/blob/master/Installation%20Bugs%20and%20Solutions.md#bug-03)），再正常安装`bruceR`，这将会大大节省时间！


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

安装前先确认连接好了网络。如果还是不行，可以看看上面这个链接。另外，如果网速较慢，则安装过程会非常漫长，毕竟有200+个依赖的R包需要先行下载安装，所以强烈建议在网速较快的环境下安装`bruceR`！


## Bug #03:
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

这个Bug最常见！某些基础R包在更新时，容易出现更新失败的情况。目前发现，`rlang`、`gsl`、`Rcpp`等最容易出现更新失败（`bruceR`作者表示很无奈）。究其原因，一般是这些包的新版本在**刚刚发布**的时候，需要通过编译（compilation）的方式来安装更新，这与R包的正常安装过程不同，更复杂更耗时也更容易出错。

请**手动卸载重装**这些R包。例如对于`rlang`包，可以这么做：
```r
## 第1步：手动卸载
remove.packages("rlang")

## 第2步：重启R语言
# 在RStudio里面可以使用快捷键`Ctrl+Shift+F10`
# 也可以鼠标点击菜单栏里面的`Session -> Restart R`

## 第3步：手动安装
install.packages("rlang")
```


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

Tips: Use the `installr` package to update R and copy R packages from the old folder to the new one.
```r
install.packages("installr")
library(installr)
updateR()
copy.packages.between.libraries(ask=TRUE)
```

当R语言本身的版本有更新时，某些R包也会同步更新并且需要依赖最新版本的R，所以如果您想更新这些R包，则必须先把R语言升级到最新版本。上面提供了一个关于升级R语言的小贴士，可以方便管理更新过程。


## Bug #05:
```
WARNING: Rtools is required to build R packages, but is not currently installed.

Please download and install Rtools 3.5 from http://cran.r-project.org/bin/windows/Rtools/

Error in parse_repo_spec(repo) : Invalid git repo specification: 'bruceR'
```
### Solution:
Download and install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).

Rtools不是一个R包，而是一个额外的工具，最好安装，不过不安装似乎也没有太大的问题。


## Bug #06:
```
Skipping install of 'bruceR' from a github remote, the SHA1 has not changed since last install.
Use `force = TRUE` to force installation
```
### Solution:
Congratulations! You have already installed the latest `bruceR`. No need to reinstall or update it.

请仔细阅读报错信息啊亲，它提示你GitHub上的版本与你已经安装过的版本之间没有变化，**你已经成功安装了最新版本**！

当然，如果你有**闲情逸致**，可以在`install_github()`函数里面加一个参数`force = TRUE`来强制重装一遍，这么做的目的是打发时间。

