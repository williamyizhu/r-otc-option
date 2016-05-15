# r-otc-option

# Install R
1.https://www.r-project.org/
2.http://mirrors.xmu.edu.cn/CRAN/

# Install R library
1.Open R console as administrator
2.install.packages("gWidgets2")
3.install.packages("RGtk2")
4.install.packages("rgl")
5.install.packages("RQuantLib")
6.install.packages("WindR") # used in aUpdateMarketData to update current market price

# Install Wind
1.http://www.wind.com.cn/
2.http://www.wind.com.cn/NewSite/wft.html
3.打开Wind Terminal，量化-修复插件-修复全部接口

# Run r-otc-option
1.setwd("/Documents/workspace/r-otc-option") # Set working directory to r-otc-option
2.source(paste(getwd(), "/main.R", sep=""), echo=FALSE, encoding="GBK")
