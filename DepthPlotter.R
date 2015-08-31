# Super easy depth plotting in R
# Ilja Kocken
# Student of Marine Sciences at Utrecht University
# First version: 2014-04-11
# Latest version: 2015-08-31

# Creates a plot based on depth and one variable (DepthPlotter calls this function)
BasicPlot <- function(depth,      # a vector of depth values
                      var,        # a vector of the variable
                      add=FALSE,  # whether or not to add the plot to the current plot
                      type="o",   
                      ylab=Depth~(m), 
                      xtitle="",
                      ylim=c(max(depth, na.rm=TRUE), min(depth, na.rm=TRUE)), 
                      legend=NULL,
                      legendpos="topright",
                      bty="n",
                      mar=c(2, 5, 5, 2) + 0.1, 
                      ...){
    # set up plotting margins.
    par(mar=mar)
    if(!add){
        plot(var, depth, ylim=ylim, type=type, xaxt="n", xlab="", ylab=ylab, ...)
        axis(3)
        if(!is.null(xtitle))
            mtext(xtitle, side=3, line=2)
        if(!is.null(legend))
            legend(legendpos, legend=legend, bty=bty)
    } else 
        points(var, depth, type=type, ...)
}

# Takes depth data with one (or multiple) variable(s) to  create a (set of) plot(s)
DepthPlotter <- function(depth=NULL,    # a vector of depth values
                         var=NULL,      # a vector (or dataframe) of variable(s)
                         xlab=NULL,     # a vector of characters or a list of formulae
                         df=NULL,       # a dataframe with variables in columns
                         oneplot=FALSE, # logical, plot all variables in the same plot
                         depthcol=1,    # the column no. in df that specifies the depth
                         sscols=1:ncol(df), #specifies columns of df to subset as vars 
                         ...){
    
    # parameter validation
    if(is.null(df) & is.null(depth))
        stop("Enter data with at least a df, or a var with associated depth")
    if(is.null(df)){
        if(is.null(var))
            stop("no var")
        else {
            if(any(is.na(var))) { warning("NAs found in var, ignoring") }
            if(any(is.na(depth))) { warning("NAs found in depth, ignoring") }
        }
    } else{ # if there is a df
        if(any(is.na(df))) { warning("NAs found in df, ignoring") }
        if(!is.null(var)){
            if(any(is.na(var))) { warning("NAs found in var, ignoring") }
            warning("Adding var to vars from df")
            #df <- df[, c(sscols)] # subset only columns of interest 
            #doesn't work yet b/c I lose the depth
            df <- cbind(df, var)
        }
        if(!is.null(depth)){
            if(any(is.na(depth))) { warning("NAs found in depth, ignoring") }
            warning("Assuming only vars in df")
            df <- df[, c(sscols)] # subset only columns of interest
            var <- df
        } else { # only df is provided
            depth <- df[,  depthcol]
            df <- df[, c(sscols)] # subset only columns of interest
        	var   <- df[, -depthcol]
        }
    } 
    # parsing of xlab
    if(!is.null(xlab)){
        if(class(xlab) == "formula"){
            if(length(xlab == 1))
                xlab <- as.expression(xlab)
            else
                lapply(xlab, as.expression)
        }
        if(length(xlab) > 1 && length(xlab) != ncol(var)){ 
            warning("Incorrect length of xlab")
            xlab <- NULL
        }
    }
    
    # by now the data is provided in a depth/var format for the rest of the function
    # only one var
    if(is.null(ncol(var)))
        return(BasicPlot(depth, var, xtitle=xlab, ...))       

    # multiple vars
    
    # everything in one plot
    invisible(      # hide output, such as lists of NULL from lapply
    if(oneplot){
        rangeofall <- c(min(var, na.rm = TRUE), max(var, na.rm = TRUE))
        BasicPlot(depth, var[,1], xlim=rangeofall, ...) # plot the first variable
        # add the other variables
        lapply(2:ncol(var), function(i) {
            BasicPlot(depth, var[,i], add=TRUE, xtitle=xlab, ...)
        })
    }
    # multiple plots
    # no or wrong xlab 
    else if(is.null(xlab)){
        # varnames are available
        if(!is.null(names(var))) 
            lapply(1:ncol(var), function(i) { 
                BasicPlot(depth, var[,i],  xtitle=names(var)[i], ...)
            })
        else # no varnames
            lapply(1:ncol(var), function(i) { BasicPlot(depth, var[,i], ...)})
    # multiplot with provided xlab repeated
    } else if(length(xlab) == 1)                               
        lapply(1:ncol(var), function(i) { 
            BasicPlot(depth, var[,i],  xtitle=xlab, ...)})
    # multiple xlabs
    else if(length(xlab) == ncol(var))      
        lapply(1:ncol(var), function(i) { 
            BasicPlot(depth, var[,i],  xtitle=xlab[[i]], ...)})
    ) # end of invisible
}