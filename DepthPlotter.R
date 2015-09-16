# Super easy depth plotting in R
# Ilja Kocken
# Student of Marine Sciences at Utrecht University
# First version: 2014-04-11
# Latest version: 2015-09-13

DepthPlotter <- function(var, ...){
    UseMethod("DepthPlotter", var)
}

# Creates a plot based on depth vector and a variable vector
DepthPlotter.default <- function(
    var,                       # a vector of the variable values
    depth,                     # a vector of depth values
    add = FALSE,               # logical, add the plot to the current plot
    xax = TRUE,                # logical, draw x-axis
    type = "o",                # default type
    ylab = "Depth (mbsf)",     # default ylab
    xlab = "",                 # default xlab xtitle
    ylim = c(max(depth, na.rm=TRUE), min(depth, na.rm=TRUE)),# default ylim
    legend = NULL,             # default no legend
    legendpos = "topright",    # default legend position
    bty = "n",                 # default legend box type 
    mar = c(2, 5, 5, 2) + 0.1, # default plot margins
    ...){                      # possible additional plotting/legend parameters
    # set up plotting margins.
    par(mar = mar)
    if(add){ # add to existing plot
        points(var, depth, type = type, ...)
    } else { # create a new plot
        plot(var, depth, ylim = ylim, type = type, xaxt = "n", 
             xlab = "", ylab = ylab, ...)
        if(xax) 
            axis(3)
        if(!is.null(xlab))
            mtext(xlab, side=3, line=2)
        if(!is.null(legend))
            legend(legendpos, legend = legend, bty = bty, ...)
    } 
       
}

# Takes a dataframe of one or multiple variable(s) to  create a (set of) plot(s)
DepthPlotter.data.frame <- function(
    var,                  # a dataframe of variable(s)
    depth = NULL,         # a vector of depth values
    xlab = "",            # a vector of characters or a list of formulae 
                # if length(xlab) > 1 gives errors when checking but works
    oneplot = FALSE,      # logical, if TRUE plot all variables in the same plot
    depthcol = 1,         # the column no. in var that specifies the depth
    sscols = 1:ncol(var), # specifies columns of var to subset
    ...){
 
    var <- var[, c(sscols)] # subset only columns of interest   
    # parameter validation
    if(any(is.na(var))) { warning("NAs found in var, ignoring") }
    if(!is.null(depth)){
        if(any(is.na(depth))) { warning("NAs found in depth, ignoring") }
            warning("Assuming only variables in dataframe")
    } else { # only var is provided
        depth <- var[ ,  depthcol]
        var   <- var[ , -depthcol]
    }
    
    # parsing of xlab
    if(xlab != ""){
        if(class(xlab) == "formula"){
            if(length(xlab == 1))
                xlab <- as.expression(xlab)
            else
                lapply(xlab, as.expression)
        }
        if(length(xlab) > 1 && length(xlab) != ncol(var)){ 
            warning("Incorrect length of xlab, ignoring")
            xlab <- ""
        }
    }
    
    # only one variable in the dataframe var
    if(is.null(ncol(var)))
        return(DepthPlotter(var, depth, xlab = xlab, ...))
    # multiple variables
    # everything in one plot
    invisible(      # hide output, such as lists of NULL from lapply
    if(oneplot){
        rangeofall <- c(min(var, na.rm = TRUE), max(var, na.rm = TRUE))
        # plot the first variable
        DepthPlotter(var[ , 1], depth, xlim=rangeofall, xlab = xlab, ...) 
        # add the other variables
        lapply(2:ncol(var), function(i) {
            DepthPlotter(var[ , i], depth, add = TRUE, ...)
        })
    # multiple plots
    } else if(xlab == ""){ 
    # no or wrong xlab 
        lapply(1:ncol(var), function(i) { 
            DepthPlotter(var[ , i], depth, xlab = names(var)[i], ...)
        })
    } else if(length(xlab) == 1) # multiplot with provided xlab repeated
        lapply(1:ncol(var), function(i) { 
            DepthPlotter(var[ , i], depth, xlab = xlab, ...)})
    # multiple xlabs
    else if(length(xlab) == ncol(var))      
        lapply(1:ncol(var), function(i) { 
            DepthPlotter(var[ , i], depth, xlab = xlab[[i]], ...)})
    ) # end of invisible
}

# experimental function that allows filled plots to be made
AreaPlotter <- function(var, depthcol=1, ...){
    DepthPlotter(var, type="n", ...)
    depth <- c(var[ , depthcol], rev(var[ , depthcol]))
    var   <- var[ , -depthcol]
    var   <- c(rep(0, length(var)), rev(var))
    polygon(var, depth, ...)
}
