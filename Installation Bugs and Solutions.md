# Installation Bugs and Solutions

All bugs are NOT related to the `bruceR` package!

安装过程中所有问题**都不是**`bruceR`包的问题！（如果有关，也是因为`bruceR`需要事先安装很多其他R包，是在安装这些R包时出了错）

**大部分用户可以顺利安装**`bruceR`！如果在安装过程中遇到问题，请仔细阅读本文档，这里有7种常见bug的解决方案！（**重启RStudio**通常也管用！）

**强烈建议**先安装好`tidyverse`和`ggstatsplot`这两个包，并更新一遍所有已安装的包，再安装`bruceR`！（无论你是否需要使用`bruceR`，都强烈推荐安装`tidyverse`和`ggstatsplot`）

为保证下载速度，**强烈建议**中国用户把CRAN镜像设置为国内的镜像（如清华大学），方法可参考：[R镜像设定与R包安装](https://mp.weixin.qq.com/s/7_Z4RZOMgyI3DQn045PAHg)

```r
#### 安装步骤 ####

## 准备：为保证下载速度，中国用户请选择一个国内的镜像（方法见上面的链接）

## 第1步：安装基础R包（如果已经安装过，请忽略）
install.packages("devtools")
install.packages("tidyverse")
install.packages("ggstatsplot")

## 第2步：更新所有R包（为了避免第3步频繁出错）
update.packages(ask=F)

## 第3步：安装bruceR包
devtools::install_github("psychbruce/bruceR")
```

> R包一般都会依赖于其他R包，在安装时会先自动安装其他R包，再安装自身。`bruceR`依赖于`tidyverse`、`ggstatsplot`等10+个R包（[查看完整列表](https://github.com/psychbruce/bruceR/blob/master/DESCRIPTION)），而`tidyverse`和`ggstatsplot`又依赖于一共200+个更基础的R包，所以如果直接安装`bruceR`，你会发现有上百个R包需要安装！一旦安装出错（容易出错的例如`rlang`、`gsl`、`Rcpp`等），则前功尽弃，又要重来一遍，并且依然有可能出错，浪费了大量时间。因此，**强烈建议**先安装好`tidyverse`和`ggstatsplot`这两个综合性的R包，再安装`bruceR`，这将大大节省时间！


## Bug #01:
```
> install.packages("bruceR")
Warning in install.packages :
  package ‘bruceR’ is not available (for R version 4.0.0)
```
### Solution:
Use `devtools::install_github("psychbruce/bruceR")`.
DO NOT use `install.packages("bruceR")`.

由于`bruceR`并没有发布在CRAN官网上，所以请通过`devtools`包来安装，而不能直接使用`install.packages()`函数。

（吐槽：`package ‘bruceR’ is not available (for R version 4.x.x)`这句话会让人**误以为**bruceR不支持R的某个版本，不得不说，Python的报错信息比R语言的报错信息有用多了！）


## Bug #02:
```
> devtools::install_github("psychbruce/bruceR")
Error: Failed to install 'unknown package' from GitHub:
Timeout was reached: [http://api.github.com] Connection timed out after 10000 milliseconds
```
or
```
> devtools::install_github("psychbruce/bruceR")
Error: Failed to install 'bruceR' from GitHub:
  schannel: failed to receive handshake, SSL/TLS connection failed
```
or
```
> devtools::install_github("psychbruce/bruceR")
Downloading GitHub repo psychbruce/bruceR@master
Error in utils::download.file(url, path, method = method, quiet = quiet,  : 
  cannot open URL 'https://api.github.com/repos/psychbruce/bruceR/tarball/master'
```
### Solution:
Check your network connection, or see:
- https://ask.csdn.net/questions/713186

安装前先确认网络畅通。如果依然不行，请等一会儿再试试。

对于“SSL/TLS connection failed”问题（即无法安装任何来自GitHub的R包），解决方案如下：
1. 依次打开IE的Internet选项、高级，找到安全模块，勾选4个：使用SSL 3.0、使用TLS 1.0、使用TLS 1.1、使用TLS 1.2
2. 找到R安装路径中的“etc”文件夹下的“Rprofile.site”文件（例如C:\\Program Files\\R\\R-4.0.0\\etc\\Rprofile.site），用RStudio或文本编辑器打开，然后在其中添加一行`options(download.file.method="libcurl")`，可以添加在`options(help_type="html")`这一行下方，保存后重启RStudio
3. 如果依然不行，请等一会儿再试试


## Bug #03:
```
> devtools::install_github("psychbruce/bruceR")
...
Error: Failed to install 'bruceR' from GitHub:
  (converted from warning) installation of package ‘rlang’ had non-zero exit status
```
or
```
> devtools::install_github("psychbruce/bruceR")
...
Error: Failed to install 'bruceR' from GitHub:
  (converted from warning) cannot remove prior installation of package ‘Rcpp’
```
### Solution:
Use the RStudio menu bar [**`Packages -> Update`**](https://shimo.im/docs/YWwKvcRgqWRdh3HR) or the code `update.packages(ask=F)` to update all the other packages before you install `bruceR`.

Sometimes you have to first use `remove.packages()` to uninstall the old packages (e.g., `rlang`, `gsl`, `Rcpp`) and then use `install.packages()` to reinstall them. Sometimes you also have to restart RStudio and try these again.

这个Bug最常见！某些基础R包在更新时，容易出现更新失败的情况。目前发现，`rlang`、`gsl`、`Rcpp`等最容易出现更新失败。究其原因，一般是这些包的新版本在**刚刚发布**的时候，需要通过编译（compilation）的方式来安装更新，这与R包的正常安装过程不同，更复杂更耗时也更容易出错。

请**手动卸载重装**这些R包。例如对于`rlang`包，可以这么做：
```r
## 第1步：手动卸载
remove.packages("rlang")

## 第2步：重启R语言
# 在RStudio里面可以点击菜单栏的`Session -> Restart R`，或使用快捷键`Ctrl+Shift+F10`

## 第3步：手动安装
install.packages("rlang")
```


## Bug #04:
```
> devtools::install_github("psychbruce/bruceR")
...
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
> devtools::install_github("psychbruce/bruceR")
...
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

当R语言本身的版本有更新时，某些R包也会同步更新并且需要依赖最新版本的R，所以如果你想更新这些R包，则必须先把R语言升级到最新版本。上面提供了一个关于升级R语言的小贴士，可以方便管理更新过程。


## Bug #05:
```
> devtools::install_github("psychbruce/bruceR")
...
* installing *source* package 'bruceR' ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) :
there is no package called 'emmeans'
Calls: ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
停止执行
ERROR: lazy loading failed for package 'bruceR'
* removing 'C:/Users/THINK/Documents/R/win-library/4.0/bruceR'
错误: Failed to install 'bruceR' from GitHub:
(由警告转换成)installation of package ‘C:/Users/THINK/AppData/Local/Temp/RtmpSoK5KO/file48d823453f6b/bruceR_0.5.1.tar.gz’ had non-zero exit status
```
### Solution:
You can find `there is no package called 'emmeans'` in the error messages. In other circumstances, there may be "no package" called another one. Just install the package(s) (e.g., `install.packages("emmeans")`) and then re-install `bruceR`.

仔细阅读报错信息。仔细阅读报错信息。仔细阅读报错信息。重要的事情说三遍。


## Bug #06:
```
> devtools::install_github("psychbruce/bruceR")
WARNING: Rtools is required to build R packages, but is not currently installed.

Please download and install Rtools 3.5 from http://cran.r-project.org/bin/windows/Rtools/

Error in parse_repo_spec(repo) : Invalid git repo specification: 'bruceR'
```
### Solution:
Download and install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).

Rtools不是一个R包，而是一个额外的工具，最好安装。


## Bug #07:
```
> devtools::install_github("psychbruce/bruceR")
Skipping install of 'bruceR' from a github remote, the SHA1 has not changed since last install.
Use `force = TRUE` to force installation
```
### Solution:
Congratulations! You have already installed the latest version of `bruceR`. No need to reinstall or update it.

请仔细阅读报错信息啊亲，它提示你GitHub上的版本与你已经安装过的版本之间没有变化，**你已经成功安装了最新版本**！

当然，如果你有**闲情逸致**，可以在`install_github()`函数里面加一个参数`force = TRUE`来强制重装一遍，这么做的目的是打发时间。

