# Super easy depth plotting in R
# Ilja Kocken
# Student of Marine Sciences in Utrecht
# 2014-04-11 started
# latest edit: 2015-08-21
-
# Creates a plot based on depth and one variable (DepthPlotter calls this function)
BasicPlot <- function(depth,      # a vector of depth values
                      var,        # a vector of the variable
                      add=FALSE,  # whether or not to add the plot to the current plot
                      type="l",   
                      ylab=Depth~(m), 
                      xtitle="",
                      ylim=c(max(depth, na.rm=TRUE), min(depth, na.rm=TRUE)), ...){
    if(!add){
        plot(var, depth, ylim=ylim, type=type, xaxt="n", xlab="", ylab=ylab, ...)
        axis(3)
        if(!is.null(xtitle))
            mtext(xtitle, side=3, line=2)
    } else 
        points(var, depth, type=type, ...)
}

# Takes depth data with one (or multiple) variable(s) to  create a (set of) plot(s)
DepthPlotter <- function(depth=NULL,    # a vector of depth values
                         var=NULL,      # a df of variables or one vector
                         xlab=NULL,     # a vector of characters or a list of formulae
                         df=NULL,       # a dataframe with variables in columns (and depth information)
                         oneplot=FALSE, # if true, plots all variables in the same plot
                         depthcol=1,    # the column no. in df that specifies the depth information
                         ...){
    # parameter validation
    if(is.null(df) & is.null(depth))
        stop("Enter data with at least a df or var and depth")
    if(is.null(df)){
        if(is.null(var))
            stop("no var")
        if(is.null(depth))
            stop("no depth")
    } else{ # if there is a df
        if(!is.null(var)){
            warning("Adding var to vars from df")
            df <- cbind(df, var)
        }
        if(!is.null(depth)){
            warning("Assuming only vars in df")
            var <- df
        }
        depth <- df[,depthcol]
        var   <- df[,-depthcol]
    } 
    # parsing of xlab
    if(!is.null(xlab)){
        if(class(xlab)=="formula"){
            if(length(xlab==1))
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
    if(oneplot){
        rangeofall <- c(min(var), max(var))
        BasicPlot(depth, var[,1], xlim=rangeofall, ...) # plot the first variable
        for(i in 2:ncol(var))
            BasicPlot(depth, var[,i], add=TRUE, xtitle=xlab, ...) # add the other variables
    # multiple plots
    # no or wrong xlab 
    } else if(is.null(xlab)){
        if(!is.null(names(var))) # varnames are available
            for(i in 1:ncol(var))
                BasicPlot(depth, var[,i], xtitle=names(var)[i], ...) 
        else # no varnames
            for(i in 1:ncol(var))                             
                BasicPlot(depth, var[,i], ...)
    # multiplot with provided xlab repeated
    } else if(length(xlab)==1)                                
        for(i in 1:ncol(var))
            BasicPlot(depth, var[,i], xtitle=xlab, ...)       
    # multiple xlabs
    else if(length(xlab)==ncol(var))                       
        for(i in 1:ncol(var))
            BasicPlot(depth, var[,i], xtitle=xlab[[i]], ...)    
}
