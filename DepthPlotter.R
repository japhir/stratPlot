# Super easy depth plotting in R
# Ilja Kocken
# Student of Marine Sciences in Utrecht
# 2014-04-11

# Creates a plot based on depth and one variable (DepthPlotter can also do this)
BasicPlot <- function(depth, var, add=FALSE, type="l", ylab=Depth~(m), xtitle="",
                      ylim=c(max(depth), min(depth)), ...){
    if(!add){
        plot(var, depth, ylim=ylim, type=type, xaxt="n", xlab="", ylab=ylab, ...)
        axis(3)
        if(!is.null(xtitle))
            mtext(xtitle, side=3, line=2)
    } else 
        points(var, depth, type=type, ...)
}

# Takes depth data to  create a (set of) plot(s)
DepthPlotter <- function(depth=NULL, var=NULL, xlab=NULL, 
                         df=NULL, oneplot=TRUE, depthcol=1, ...){
    # parameter validity and assignment
    if(is.null(df) & is.null(depth))
        stop("Enter data with at least a df or var and depth")
    if(is.null(df)){
        if(is.null(var))
            stop("no var")
        if(is.null(depth))
            stop("no depth")
    } else{ # i.e. if there is a df
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
    } # by now the data is provided in a depth/var format for the rest of the function
    if(is.null(ncol(var)))
        return(BasicPlot(depth, var, xtitle=xlab, ...))     # only depth and one var
    if(oneplot){
        rangeofall <- c(min(var), max(var))
        BasicPlot(depth, var[,1], xlim=rangeofall, ...)
        for(i in 2:ncol(var))
            BasicPlot(depth, var[,i], add=TRUE, xtitle=xlab, ...) # everything in one plot
    } else if(length(xlab)==ncol(var)){
        xlabs <- xlab
        for(i in 1:ncol(var))
            BasicPlot(depth, var[,i], xtitle=xlabs[i], ...) # multiplot it with xlabs
    } else if(length(xlab)==1)
        for(i in 1:ncol(var))
            BasicPlot(depth, var[,i], xtitle=xlab, ...)     # multiplot with same xlab
    else if(is.null(xlab)){                                 # no x-lab but varnames        
        if(!is.null(names(var)))
            for(i in 1:ncol(var))
                BasicPlot(depth, var[,i], xtitle=names(var)[i], ...) 
        else 
            for(i in 1:ncol(var))                           # no xlabs
                BasicPlot(depth, var[,i], ...)
    }
}
