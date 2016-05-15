# r-otc-option

# Install R
https://www.r-project.org/
http://mirrors.xmu.edu.cn/CRAN/

# Install R library
Open R console as administrator
install.packages("gWidgets2")
install.packages("RGtk2")
install.packages("rgl")
install.packages("RQuantLib")
install.packages("WindR") # used in aUpdateMarketData to update current market price

# Install Wind
http://www.wind.com.cn/
http://www.wind.com.cn/NewSite/wft.html
量化-修复插件-修复全部接口

# Run r-otc-option
setwd("/Documents/workspace/r-otc-option") # Set working directory to r-otc-option
source(paste(getwd(), "/main.R", sep=""), echo=FALSE, encoding="GBK")
