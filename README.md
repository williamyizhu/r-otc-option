# r-otc-option

# Install R
0. https://www.r-project.org/
0. http://mirrors.xmu.edu.cn/CRAN/

# Install R library
0.Open R console as administrator.
0.install.packages("gWidgets2").
0.install.packages("RGtk2").
0.install.packages("rgl")
0.install.packages("RQuantLib")
0.install.packages("WindR") # used in aUpdateMarketData to update current market price

# Install Wind
0.http://www.wind.com.cn/
0.http://www.wind.com.cn/NewSite/wft.html
0.打开Wind Terminal, 量化-修复插件-修复全部接口

# Run r-otc-option
0.setwd("/Documents/workspace/r-otc-option") # Set working directory to r-otc-option
0.source(paste(getwd(), "/main.R", sep=""), echo=FALSE, encoding="GBK")

