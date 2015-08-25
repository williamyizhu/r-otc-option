# TODO: Add comment

##############################################################################################################
rm(list=ls(all=TRUE)) 
options(width = 438L)

library(gWidgets)
options("guiToolkit"="RGtk2")
#options("guiToolkit"="tcltk")

##############################################################################################################
# TODO: example 1
##############################################################################################################
#Proceeding, first we define the two distributions and the possible kernels.
availDists <- c(Normal="rnorm", Exponential="rexp")
availKernels <- c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine")

#We then define the key function for drawing the graphic. This refers to widgets yet to be defined.
updatePlot <- function(h, ...) {
	tryCatch({
				x <- do.call(availDists[svalue(distribution)], list(svalue(sampleSize)))
#				print(svalue(distribution))
#				print(svalue(sampleSize))
#				print(x)
				plot(density(x, adjust=svalue(bandwidthAdjust), kernel=svalue(kernel)), main=paste(svalue(mainTitle), "Density plot"))
				rug(x)					
			}, warning = function(war) {
				print(war)
			}, error = function(err) {
				print(err)				
			}, finally = {				
			})	
}

#Now to define the widgets.
mainTitle = gedit("1", coerce.with=as.numeric, handler=updatePlot)
distribution <- gradio(names(availDists), horizontal=FALSE, handler=updatePlot)
sampleSize <- gradio(c(50,100,200,300), handler=updatePlot)
kernel <- gcombobox(availKernels, handler=updatePlot)
#bandwidthAdjust <- gslider(from=0.01, to=2, by=0.01, value=1, handler=updatePlot)
bandwidthAdjust <- gspinbutton(from=0.0, to=2, by=0.01, value=1, handler=updatePlot)

kernel2 <- gcombobox(availKernels, handler=updatePlot)
#bandwidthAdjust <- gslider(from=0.01, to=2, by=0.01, value=1, handler=updatePlot)
bandwidthAdjust2 <- gspinbutton(from=0.0, to=2, by=0.01, value=1, handler=updatePlot)

#Now the layout. We use frames to set off the different arguments. A frame is like a group, 
#only it has an option for placing a text label somewhere along the top. 
#The position is specified via the pos argument, with a default using the left-hand side.

#now layout, create the main window frame
window <- gwindow("gWidgetsDensity")
BigGroup0 <- ggroup(horizontal=FALSE, container=window)
BigGroup1 <- ggroup(horizontal=TRUE, container=window)

#inside "group3, widgets are placed "horizontal=FALSE", i.e., "vertical=TRUE"
group3 <- ggroup(horizontal=FALSE, container=BigGroup0)
group4 <- ggroup(horizontal=TRUE, container=BigGroup0)

group5 <- ggroup(horizontal=FALSE, container=group4)
group6 <- ggroup(horizontal=FALSE, container=group4)

#add elements into the main window frame
add(gframe("Main title", container=group3), mainTitle)
add(gframe("Distribution", container=group3), distribution)
add(gframe("Sample size", container=group3), sampleSize)

add(gframe("Kernel", container=group5), kernel)
add(gframe("Bandwidth adjust", container=group5), bandwidthAdjust, expand=TRUE)

add(gframe("Kernel", container=group6), kernel2)
add(gframe("Bandwidth adjust", container=group6), bandwidthAdjust2, expand=TRUE)

#include plot window in the main window frmae
add(BigGroup1, ggraphics())

##############################################################################################################
# TODO: example 2
##############################################################################################################
#The textboxes and checkboxes and so forth that we need are known as widgets (hence ¡°gWidgets¡±). 
#They need to be contained inside a window, which we create using the function gwindow.
win <- gwindow("Tab delimited file upload example")

#By default, the widgets will be stacked up vertically. 
#We can create groups of widgets that are stacked horizontally with ggroup(which is a widget in itself). 
#Notice that all widgets must specify their container; in this case it¡¯s just the window.
grp_name <- ggroup(container=win)

#A glabel is a widget that represents a text label. Notice that it is contained inside the group we just created.
lbl_data_frame_name <- glabel("Variable to save data to: ",	container=grp_name)

#A gedit is a single line textbox. (Not to be confused with a gtext, which is a multiline textbox.)
txt_data_frame_name <- gedit("dfr", container=grp_name)

#Another horizontal group, for the upload button.
grp_upload <- ggroup(container=win)

#For widgets that we want to respond to an action, we need to add a handler argument. 
#This is always a function accepting a list as its first argument (named h by convention), and dots. 
#The gbutton handler is called whenever the button is clicked. 
#Don¡¯t worry about the contents of the handler function for now; we¡¯ll add them in a moment.
btn_upload <- gbutton(
		text = "Upload tab delimited file",
		container = grp_upload,
		handler = function(h, ...) {
			# TODO!
		}
)

#Since tab delimited files can have decimal places represented as full-stops or commas (depending upon local conventions), we need a checkbox to choose between cases. 
#We define a function to get the default value from the system locale settings. 
#Conveniently, checkboxes have their own label built-in so we don¡¯t need to create our own this time.
use_comma_for_decimal <- function() {
	unname(Sys.localeconv()["decimal_point"] == ",")
}

chk_eurostyle <- gcheckbox(
		text = "Use comma for decimal place",
		checked = use_comma_for_decimal(),
		container = grp_upload
)

#The last widget we¡¯ll include is a status bar, so that users don¡¯t have to refer back to the R main window for messages.
status_bar <- gstatusbar("", container=win)

#Finally, here¡¯s the content for the button handler. 
#It creates a file open dialog box, which in turn has its own handler function. 
#The action argument names the function to be applied to the file that is opened. 
#The svalue function returns the ¡°most useful thing¡± from a widget. For a checkbox, the svalue is whether or not it is checked. 
#For a textbox or status bar, the svalue is its text. 
#The filter argument populates the ¡°Files of type¡± drop down list in the file open dialog.
function(h, ...) {
	gfile(
			text = "Upload tab delimited file",
			type = "open",
			action = ifelse(svalue(chk_eurostyle), "read.delim2", "read.delim"),
			handler = function(h, ...) {
				tryCatch(
						{
							data_frame_name <- make.names(svalue(txt_data_frame_name))
							the_data <- do.call(h$action, list(h$file))
							assign(data_frame_name, the_data, envir = globalenv())
							svalue(status_bar) <- paste(nrow(the_data), "records saved to variable", data_frame_name)
						},
						error = function(e) svalue(status_bar) <- "Could not upload data"
				)
			},
			filter = list(
					"Tab delimited" = list(patterns = c("*.txt","*.dlm","*.tab")),
					"All files" = list(patterns = c("*"))
			)
	)
}

#If you¡¯re feeling enthusiastic, see if you can adapt this to work with CSV files, or even better a general delimited file.
#One last trick to finish the post: You can create a GUI interface to any function using ggenericwidget. Try
lmwidget <- ggenericwidget(lm)





