# TODO: Add comment

###############################################################################
rm(list=ls(all=TRUE)) 
options(width = 438L)
library("RQuantLib")
library("fExoticOptions")
library("fAsianOptions")
library("fOptions")
source(paste(getwd(), "/option_mc.R", sep=""), echo=FALSE, encoding="GBK")

#=============================================================================================================================================================
# TODO # OptionMC2, deprecated
#=============================================================================================================================================================
#OptionMCApply(cDaysIndex, style, S_vec, X, r, q, volatility, nSims, minSteps=500, rValue=c("path","sigma","call_value","call_delta","put_value","put_delta"))
source(paste(getwd(), "/option_mc.R", sep=""), echo=FALSE, encoding="GBK")

#number of simulation runs
nSims = 1000
X = 3000
r = 0.
q = 0.
volatility = 0.25
option_style = "arithmetic"

#starting point
S0 = 3000
#drift term
mu = r - q
#every trading day increment 
dt = 1 / 365	
#standard normal distribution random number
z = rnorm(29, mean=0, sd=1)	
#generate log-normal return matrix
return_vec = exp((mu - 0.5 * volatility ^ 2) * dt + volatility * sqrt(dt) * z)
#existing price path
S = c(S0, S0 * cumprod(return_vec))

#"OptionMCApply", let "minSteps=1", otherwise, default value "minSteps=500"
cDaysIndex = matrix(1:length(S))
optionMC = apply(cDaysIndex, 1, OptionMC2, option_style, S, X, r, q, volatility, nSims)

par(mfrow=c(5,6))
for (i in 1:length(optionMC)) {
#	mean of existing price path
	if (option_style == "arithmetic") {
		S_mean = mean(S[1:i])
	} else if (option_style == "geometric") {
		S_mean = exp(mean(log(S[1:i])))
	}	
	matplot(optionMC[[i]]$path, type='l', xlab=paste("X=", X, ", mean=", round(S_mean,4), ", sigma=", round(optionMC[[i]]$sigma,4), sep=""))
	title(paste("call value =", round(unlist(optionMC[[i]]["call_value"]),4), "delta =", round(unlist(optionMC[[i]]["call_delta"]),4), "\nput value =", round(unlist(optionMC[[i]]["put_value"]),4), "delta =", round(unlist(optionMC[[i]]["put_delta"]),4), sep=" "))
}




length(ex1 <- expression(1 + 0:9)) # 1
ex1
eval(ex1) # 1:10

length(ex3 <- expression(u, v, 1+ 0:9)) # 3
mode(ex3 [3])   # expression
mode(ex3[[3]])  # call
rm(ex3)

#--------------------------------------------------- apply ---------------------------------------------------
require(stats)
require(graphics)

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
lapply(x, mean)
# median and quartiles for each list element
lapply(x, quantile, probs = 1:3/4)
sapply(x, quantile)
i39 <- sapply(3:9, seq) # list of vectors
sapply(i39, fivenum)
vapply(i39, fivenum, c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))

## sapply(*, "array") -- artificial example
(v <- structure(10*(5:8), names = LETTERS[1:4]))

f2 <- function(x, y) {
	outer(rep(x, length.out = 3), y)
}

(a2 <- sapply(v, f2, y = 2*(1:5), simplify = "array"))





a.2 <- vapply(v, f2, outer(1:3, 1:5), y = 2*(1:5))
stopifnot(dim(a2) == c(3,5,4), all.equal(a2, a.2), identical(dimnames(a2), list(NULL,NULL,LETTERS[1:4])))

hist(replicate(100, mean(rexp(10))))

## use of replicate() with parameters:
foo <- function(x = 1, y = 2) c(x, y)
# does not work: bar <- function(n, ...) replicate(n, foo(...))
bar <- function(n, x) replicate(n, foo(x = x))
bar(5, x = 3)

#--------------------------------------------------- tapply ---------------------------------------------------
#Apply a function to each cell of a ragged array, that is to each (non-empty) group of values given by a unique combination of the levels of certain factors.

require(stats)
groups <- as.factor(rbinom(32, n = 5, prob = 0.4))
tapply(groups, groups, length) #- is almost the same as
table(groups)

## contingency table from data.frame : array with named dimnames
tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
tapply(warpbreaks$breaks, warpbreaks[, 3, drop = FALSE], sum)

n <- 17; fac <- factor(rep(1:3, length = n), levels = 1:5)
table(fac)
tapply(1:n, fac, sum)
tapply(1:n, fac, sum, simplify = FALSE)
tapply(1:n, fac, range)
tapply(1:n, fac, quantile)

## example of ... argument: find quarterly means
tapply(presidents, cycle(presidents), mean, na.rm = TRUE)

ind <- list(c(1, 2, 2), c("A", "A", "B"))
table(ind)
tapply(1:3, ind) #-> the split vector
tapply(1:3, ind, sum)


#----------------------------------------------------------------------------------------------------------------------------



library(gWidgets2RGtk2)
library(cairoDevice)

w <- gwindow("notebook example")
nb <- gnotebook(cont=w)
gg <- ggraphics(cont=nb, label='1',visible=FALSE)
plot(c(1,2,3),c(3,4,5))
visible(gg) <- TRUE




rm(list=ls(all=TRUE)) 


library(gWidgets2)
library(rgl)

w <- gwindow("brushing example", visible=FALSE)
g <- ggroup(cont=w)
pg <- gnotebook(cont=g, expand=TRUE)
dev1 <- ggraphics(cont=pg)

visible(w) <- TRUE

visible(dev1) <- TRUE	
plot.new()	

x <- sort(rnorm(1000))
y <- rnorm(1000)
z <- rnorm(1000) + atan2(x,y)
plot3d(x, y, z, col=rainbow(1000))



plot(y ~ x)













## Now make interactive

## click on histogram, update scatter
addHandlerClicked(dev1, handler=function(h,...) {
			xc <- h$x
			## which values are in x
			l <- make_data()
			bins <- l$bins; cnts <- l$cnts
			
			n <- length(bins) - 1
			which_bin <- which(sapply(1:n, function(i) bins[i] <= xc & xc < bins[i+1]))
			ind <- sapply(x,function(j) bins[which_bin] <= j & j < bins[which_bin + 1])
			plot_hist(ind)
			plot_scatter(ind)
		})

addHandlerChanged(dev2, handler=function(h,...) {
			
			x_ind <- h$x[1] <= x & x < h$x[2]
			y_ind <- h$y[1] <= y & y < h$y[2]
			
			
			
			plot_hist(x_ind)
			plot_scatter(x_ind & y_ind)
		})


## show which bin
addHandlerMouseMotion(dev1, handler=function(h,...) {
			xc <- h$x
			## which values are in x
			l <- make_data()
			bins <- l$bins; cnts <- l$cnts
			
			n <- length(bins) - 1
			which_bin <- which(sapply(1:n, function(i) bins[i] <= xc & xc < bins[i+1]))
			svalue(sb) <- sprintf("In bin %s", which_bin)
		})



split.screen(rbind(c(0.1,0.292,0.1, 0.98), c(0.312, 0.95, 0.1, 0.98)))
screen(1)
par(mar = c(0, 0, 0, 0))
plot(1:30, rnorm(30), xaxs = "i", ylim = c(-3, 3), xaxt = "n", xlab="abc", ylab="xxx")
axis(1, at = seq(0, 30, 20))

screen(2)
par(mar = c(0, 0, 0, 0))
plot(1:100, rnorm(100), xaxs = "i", ylim = c(-3, 3), yaxt = "n", col = "red", xlab="abc", ylab="xxx")
close.screen(all.screens = TRUE)

erase.screen(n = 2)


par(bg = "white")           # default is likely to be transparent
split.screen(c(2, 1))       # split display into two screens
split.screen(c(1, 3), screen = 2) # now split the bottom half into 3
screen(1) # prepare screen 1 for output
plot(10:1)
screen(4) # prepare screen 4 for output
plot(10:1)
close.screen(all = TRUE)    # exit split-screen mode



split.screen(c(2, 1))       # split display into two screens
screen(1)
par(mar = c(0, 0, 0, 0))
plot(1:10)                  # screen 3 is active, draw plot

screen(2)
par(mar = c(0, 0, 0, 0))
plot(rep(1:30)) 

screen(1)
plot(rep(1,10))
lines(rep(-0.8,20))

split.screen(c(1, 2), 2)    # split bottom half in two





erase.screen()              # forgot label, erase and redraw
plot(1:10, ylab = "ylab 3")
screen(1)                   # prepare screen 1 for output
plot(1:10)
screen(4)                   # prepare screen 4 for output
plot(1:10, ylab = "ylab 4")
screen(1, FALSE)            # return to screen 1, but do not clear
plot(10:1, axes = FALSE, lty = 2, ylab = "")  # overlay second plot
axis(4)                     # add tic marks to right-hand axis
title("Plot 1")
close.screen(all = TRUE)    # exit split-screen mode



split.screen(rbind(c(0.1,0.292,0.1, 0.98), c(0.312, 0.95, 0.1, 0.98)))
screen(1)
par(mar = c(0, 0, 0, 0))
plot(1:30, rnorm(30), xaxs = "i", ylim = c(-3, 3), xaxt = "n")
axis(1, at = seq(0, 30, 20))
screen(2)
par(mar = c(0, 0, 0, 0))
plot(1:100, rnorm(100), xaxs = "i", ylim = c(-3, 3), yaxt = "n", col = "red")
close.screen(all.screens = TRUE)



def.par <- par(no.readonly = TRUE) # save default, for resetting...

## divide the device into two rows and two columns
## allocate figure 1 all of row 1
## allocate figure 2 the intersection of column 2 and row 2
layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE))
## show the regions that have been allocated to each plot
layout.show(2)

## divide device into two rows and two columns
## allocate figure 1 and figure 2 as above
## respect relations between widths and heights
nf <- layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE), respect = TRUE)
layout.show(nf)

## create single figure which is 5cm square
nf <- layout(matrix(1), widths = lcm(5), heights = lcm(5))
layout.show(nf)



##-- Create a scatterplot with marginal histograms -----
plot.new()
x <- pmin(3, pmax(-3, stats::rnorm(50)))
y <- pmin(3, pmax(-3, stats::rnorm(50)))

nf <- layout(matrix(c(1,2),nrow=2), heights = c(1,3))
layout.show(nf)

par(mar = c(3,3,1,1))
plot(x, y, xlab = "", ylab = "")
lines(x,y)

par(mar = c(3,3,1,1))
plot(x, y, xlab = "", ylab = "")




x <- pmin(3, pmax(-3, stats::rnorm(50)))
y <- pmin(3, pmax(-3, stats::rnorm(50)))
xhist <- hist(x, breaks = seq(-3,3,0.5), plot = FALSE)
yhist <- hist(y, breaks = seq(-3,3,0.5), plot = FALSE)
top <- max(c(xhist$counts, yhist$counts))
xrange <- c(-3, 3)
yrange <- c(-3, 3)

nf <- layout(matrix(c(2,0,1,3),nrow=2), c(3,1), c(1,3), TRUE)

nf <- layout(matrix(c(1,2),nrow=2), c(3,1), c(1,3), TRUE)




c(bottom, left, top, right)

plot(c(1,2,3.1,14,-1), c(2,3,-4,-5,10), type="h", xlim=c(-5,20), lwd = 10)



lines(c(11,12,13,14.2), c(2,3,-4,-5), type="h", lwd = 10)

plot(table(rpois(100, 5)), type = "h", col = "red", lwd = 10,
		main = "rpois(100, lambda = 5)")

par(mar = c(3,0,1,1))
barplot(yhist$counts, axes = FALSE, xlim = c(0, top), space = 0, horiz = TRUE)

par(def.par)  #- reset to default




library(RGtk2) ## need this
library(gWidgets)
options(guiToolkit="RGtk2")



model <- rGtkDataFrame(mtcars)
view <- gtkTreeView(model)
## Michael Lawrence's trick to add cell renderer's
mapply(view$insertColumnWithAttributes,  -1, colnames(model), 
		list(gtkCellRendererText()), text = seq_len(ncol(model)) - 1)

sw <- gtkScrolledWindow()
sw$add(view)

sapply(1:ncol(model), function(j) {
			cr <- view$getColumn(j-1)$getCellRenderers()[[1]]
			cr['editable'] <- TRUE
			gSignalConnect(cr, "edited", 
					f=function(cr, path, newtext, user.data) {
						curRow <- as.numeric(path) + 1
						curCol <- user.data$column
						model <- user.data$model
						## coerce newtext from character to desired type
						## otherwise this coerces to character
						model[curRow, curCol] <- as.numeric(newtext)
					}, data=list(model=model, column=j))
		})

Sys.setlocale(locale="chs")
library(gridExtra)
png(filename = "output1.png", width=480,height=480) 
ss=read.csv("mg.txt", header=FALSE)
#grid.table(ss) 
tableGrob(ss)
dev.off() 
Sys.setlocale(locale="us")



.GlobalEnv$grs = option_mm_list2

Sys.setlocale(locale="chs")

library(grid)
library(gridExtra)
png(filename = "output1.png", width=980, height=980) 
ss = read.csv("mg.txt", header=FALSE)
#png(filename = "output1.png") 
grid.text(ss)
#ss=read.csv("mg.txt", header=FALSE)
#grid.table(grs[["put"]][["vanilla"]]) 
#grid.text("aaaaa")
dev.off() 

#============================================================================================================================================

Sys.setlocale(locale="us")

png(filename = "output1.png", width=980, height=980) 
write.table("aa", "output1.png", append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)	
#png(filename = "output1.png") 

#ss=read.csv("mg.txt", header=FALSE)
grid.table(grs[["put"]][["vanilla"]]) 
grid.table(grs[["call"]][["vanilla"]]) 

#tableGrob(ss)
dev.off() 

library(gridExtra)
grid.table(c("a","b"))

library(grid)
g <- tableGrob(head(iris, 3))
m <- tableGrob(tail(iris, 3))
grid.arrange(gList(g,m))
grid.newpage()
grid.draw(gList(g,m),gpar(byrow=TRUE))

Sys.setlocale(locale="chs")
ss = "我们 "
sg <- tableGrob(ss)
grid.draw(sg)


library(grid)
grid.arrange(rectGrob(), rectGrob())

## Not run:
library(ggplot2)
pl <- lapply(1:11, function(.x) qplot(1:10, rnorm(10), main=paste("plot", .x)))
ml <- marrangeGrob(pl, nrow=2, ncol=2)
## non-interactive use, multipage pdf
ggsave("multipage.pdf", ml)
## interactive use; open new devices
ml
## End(Not run)






library(grid)
library(gtable)


library(gridExtra)
title <- textGrob("----------------------------------------------------------------------------------", x=0)
title2 <- textGrob("我们是社会", x=0)
table <- tableGrob(head(iris))
table2 <- tableGrob(tail(iris), rows=NULL)

ml=marrangeGrob(gList(title, title, table, table2),ncol=1,nrow=4)
ggsave("multipage.jpg", ml)


#table <- gtable_add_rows(table, heights = grobHeight(footnote2) + padding)
table <- gtable_add_grob(table, list(table2), t=c(1,2), l=c(2,2))
grid.newpage()
grid.draw(table)

title <- textGrob("----------------------------------------------------------------------------------", x=0)
footnote1 <- textGrob("footnote1", x=0, hjust=0, gp=gpar( fontface="italic"))
footnote2 <- textGrob("footnote2", x=0, hjust=0, gp=gpar( fontface="italic"))
padding <- unit(0.5,"line")
table <- gtable_add_rows(table, heights = grobHeight(title) + padding, pos = 0)
table <- gtable_add_rows(table, heights = grobHeight(footnote1) + padding,pos=0)
table <- gtable_add_rows(table, heights = grobHeight(footnote2) + padding)

table <- gtable_add_grob(table, list(title, footnote1, footnote2), t=c(1,2, nrow(table)), l=c(2,2,1),r=ncol(table))
grid.newpage()
grid.draw(table)


library(gridExtra)

a1 = textGrob("-------------------------------------------")
a2 = textGrob("一切都在不严重")

hh <- tableGrob(head(iris), rows=NULL, cols=c("今天","每天","需要","我们","吃饭"))
tt <- tableGrob(tail(iris), rows=NULL, cols=c("天","天","需要","们","吃饭"))
#grid.arrange(a1,hh,a1,a2,tt)
				
padding <- unit(1,"line")
hh <- gtable_add_rows(hh, heights = grobHeight(a1) + padding, pos = 0)
hh <- gtable_add_grob(table, hh)

grid.newpage()
grid.draw(table)

library(gridExtra)
d <- head(iris)
table <- tableGrob(d)

library(grid)
library(gtable)

title <- textGrob("Title",gp=gpar(fontsize=50))
footnote <- textGrob("footnote", x=0, hjust=0,
		gp=gpar( fontface="italic"))


table <- gtable_add_rows(table, 
		heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title, footnote),
		t=c(1, nrow(table)), l=c(1,2), 
		r=ncol(table))
grid.newpage()
grid.draw(table)





library(grid)
library(gtable)

title <- textGrob("Title")
footnote <- textGrob("我们在这里")

padding <- unit(0.5,"line")
table <- gtable_add_rows(table, heights = grobHeight(title) + padding, pos = 0)
table <- gtable_add_rows(table, heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title, footnote), t=c(1, nrow(table)), l=c(1,2), r=ncol(table))

grid.newpage()
grid.draw(table)

grid.arrange(title, table,footnote)

library(ggplot2)
ggsave("saving.png", table)


dd <- head(iris)
colnames(dd) = c("今天","每天","需要","我们","吃饭")

aa = textGrob("我们在这里")
mg = tableGrob(dd)
mg <- gtable_add_grob(mg, list(aa))


grid.arrange(list(aa,mg), newpage=TRUE)

l <- linesGrob()
## Draw it
grid.draw(l)
