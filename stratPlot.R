## Super easy plotting of paleodata in R
## Ilja Kocken
## Student of Marine Sciences at Utrecht University
## First version: 2014-04-11
## Latest version: 2016-09-16

## ## test code
##                                         # a vector
## stratPlot(1:10, c(1, 4, 5, 100, 400, 1000, 50000, 90000, 1000000, 1000000),
##           ylab = pCO[2]~(ppm),
## 	  pol = T, polcol = "purple", bar = T, barcol = "green",
## 	  error = c(1.5, 10, 10, 40, 100, 100, 30000, 90000, 10000, 10000),
##           errorcol = "red",
## 	  abc = "A", abcadj = -0.2,
## 	  xax = c(1, 3),
## 	  xntck = 5,
##           log = "y", mar = "auto")

## stratPlot(data.frame(var0 = c(1, 4, 5, 100, 400, 1000, 50000, 90000, 1000000, 1000000),
##                      age = seq(100, 1000, length.out = 10),  # automatically extracts age column
##                      var1 = rnorm(10, 40)),
##           log = c("y", ""),
##           pol = c(T, F))

stratPlot <- function(var, ...){
    UseMethod("stratPlot", var)
}

## Creates a plot based on a depth/age vector and a variable vector
stratPlot.numeric <- function(age, var, 
                              ## direction of age "v", "ver", "vertical" or "h" "hor" "horizontal"
                              agedir = "h",
                              ## GTS colour scale on age axis
                              GTS = F, Era = F, Period = T, Epoch = T, Age = F, GTSfrac = .05,
                              ## polygon to plot
                              pol = F, pol0 = NULL, polcol = "#4682B4E6", border = NA, 
                              ## bar to plot
                              bar = F, barcol = "orange", barlwd = 2,
                              ##  gapmaker = NULL, # TODO
                              add = F,  # logical, add to plot or start new one
                              ## vector of relative errors or matrix/df of absolute values to plot
                              error = NULL, errortype = "bars", # or "area"
                              errorcol = "#BEBEBEE6",
                              abc = NULL, abcadj = NULL, # add index letter topleft
                              mar = "inherit",  # or "auto" or specified
                              ##  TODO: add standard Geologic Time Scale to region near x or y axis
                              ...,  # other graphical parameters
                              ## default positions of axes (1:4)
                              xax = if (agedir == "h") 1 else 3, yax = 2,
                              xlim = NULL, ylim = NULL, 
                              xlab = NULL, ylab = NULL,
                              ## positions of minor tick marks
                              xtck = NULL, ytck = NULL, 
                              ## number of minor tick marks between major marks
                              xntck = 2, yntck = 2,
                              ## default plot/log options
                              las = 1, log = "", bty = "n", type = "o",
                              lty = 1, pch = 16) {
    ##  check var and age
    if (length(var) != length(age)) {
        stop("Unequal length of var and age")
    }

    ## TODO
    ## check gapmaker with vectors?
    ## if (!is.null(gapmaker)) {
    ##  gapMaker()
    ## }
    
    ## check agedir
    if (!agedir %in% c("v", "ver", "vertical", "h", "hor", "horizontal")) {
        stop("agedir must be either 'v', 'ver', 'vertical' or 'h', 'hor, 'horizontal'")
    } else {
        agedir <- substr(agedir, 1, 1) # agedir is converted to either 'v' or 'h'
    }
    
    if (!add) {
        ##  mar (depends on xax and yax, which are not checked)
        if (identical(mar, "auto")) {
            mar <- c(if (1 %in% xax || 1 %in% yax) 5 else 2,
                     if (2 %in% xax || 2 %in% yax) 5 else 2,
                     if (3 %in% xax || 3 %in% yax) 5 else 2,
                     if (4 %in% xax || 4 %in% yax) 5 else 2) + .1
        } else if (identical(mar, "inherit")) {
            mar <- par(no.readonly = TRUE)$mar
        }

        ##  xlim, ylim
        if (agedir == "h") {
            if (is.null(xlim)) xlim <- range(age, na.rm = TRUE)
            if (is.null(ylim)) ylim <- range(var, na.rm = TRUE)
        } else {
            if (is.null(xlim)) xlim <- range(var, na.rm = TRUE)
            if (is.null(ylim)) ylim <- rev(range(age, na.rm = TRUE))
        }
    
        ##  default xlab, ylab
        if (agedir == "h") {
            if (is.null(xlab)) {
                if (max(age, na.omit = T) < 100) {
                    message("Assuming age is given in Mya")
                    xlab <- "Age (Mya)"
                } else {
                    message("Assuming age is given in kya")
                    xlab <- "Age (kya)"
                }
            }
            if (is.null(ylab)) {
                message("No ylab provided")
                ylab <- ""
            }
        } else {
            if (is.null(xlab)) {
                message("No xlab provided")
                xlab <- ""
            }
            if (is.null(ylab)) {
                if (max(age, na.omit = T) < 100) {
                    message("Assuming age is given in Mya")
                    ylab <- "Age (Mya)"
                } else {
                    message("Assuming age is given in kya")
                    ylab <- "Age (kya)"
                }
            }
        }

        ##  set up plotting margins
        par(mar = mar, bty = bty)
    
        ##  set up empty plot
        plot(1, type = "n", xlim = xlim, ylim = ylim, log = log,
             xlab = "", ylab = "", axes = FALSE, ...)
        ## axes added later
    }
    
    if (pol || bar) {
        ## baseline to draw polygon and bar to
        if (is.null(pol0)) {
            ## logarithmic var axis -> draw polygon to minimum value
            if ((agedir == "h" && grepl("y", log)) ||
                (agedir == "v" && grepl("x", log))) {
                pol0 <- min(var)
            } else  {
                pol0 <- 0
            }
        }
    }
    
    ##  plot polygon
    if (pol) {
        if (anyNA(var) || anyNA(age)) {
            message("NAs found in var/age, currently ignoring")
            nona <- data.frame(var = var, age = age)
            nona <- nona[complete.cases(nona), ]
            x <- c(nona$age[1], nona$age, tail(nona$age, n = 1))
            y <- c(pol0, nona$var, pol0)
            ##  TODO: create polygons per non-NA region, like we do with errorregion
            ##  enc <- rle(!is.na(var))
            ##  endIdxs <- cumsum(enc$lengths)
            ##  for (i in 1:length(enc$lengths)) {
            ##    if (enc$values[i]) {
            ##      endIdx <- endIdxs[i]
            ##      startIdx <- endIdx - enc$lengths[i] + 1
            ##      subvar <- var[startIdx:endIdx]
            ##      subage <- age[startIdx:endIdx]
            ##      x <- c(left, subvar, left)
            ##      y <- c(subage[1], age, tail(age, n = 1))
            ##    }
            ##  }
        } else {
            ##  not sure if I want to use the current left or use:
            ##  min(var, na.rm = TRUE) - .04 * diff(range(var, na.rm = TRUE))
            x <- c(age[1], age, tail(age, n = 1))
            y <- c(pol0, var, pol0)
        }
        polygon(x = if (agedir == "h") x else y, y = if (agedir == "h") y else x,
                col = polcol, border = border) 
    }
    
    ## plot error bars/area
    if (!is.null(error)) {
        if (length(error) > 0) {  # plot errorstuff
            if(errortype == "bars") {
                errorBarsPlot(age, var, error, col = errorcol, agedir = agedir)
            } else if (errortype == "area")
                errorAreaPlot(age, var, error, col = errorcol, agedir = agedir)
        }
    }

    ##  plot bars (added after errors to overlap the possible areas)
    if (bar) {
        if (is.null(pol0)) pol0 <- 0
        segments(x0 = if (agedir == "h") age else rep(pol0, length(var)),
                 y0 = if (agedir == "h") rep(pol0, length(var)) else age,
                 x1 = if (agedir == "h") age else var,
                 y1 = if (agedir == "h") var else age,
                 col = barcol, lwd = barlwd)
    }
    
    ##  plot the actual record
    points(x = if (agedir == "h") age else var,
           y = if (agedir == "h") var else age, type = type,
           pch = pch, lty = lty, ...)

    ## add axes
    if (!add) {
        if ((length(xax == 1) && xax %in% c(1, 3)) || identical(xax, c(1, 3))) {
            ## if the x-axis is on a logarithmic scale
            if (grepl("x", log)) {
                ## the log10 range of powers that need something done
                xpow <- c(
                    if (xlim[1] == 0) -100
                    else if (xlim[1] > 0) floor(log10(xlim[1]))
                    else - floor(log10(-xlim[1])),
                    if (xlim[2] == 0) -100
                    else if (xlim[2] > 0) ceiling(log10(xlim[2]))
                    else -ceiling(log10(-xlim[2]))) 
                if (is.null(xtck)) 
                    xtck <- c(1:10 %o% 10^((xpow)[1]:(xpow)[2]))
                ## log x-axis with nicer 10^xpow labels
                lapply(xax, axis, at = 10^(xpow[1]:xpow[2]),
                       labels = sapply((xpow)[1]:(xpow)[2], # or -xpow[1]?
                                       function(i) {
                                           as.expression(bquote(10^ .(i)))}),
                       las = las)
            } else {
                ## non-log x-axis
                lapply(xax, axis, las = las)
                ## find stepsize used in default axis
                stepsize <- abs(diff(axTicks(1)[1:2])) / xntck
                if (is.null(xtck)) 
                    ## add xntck ticks between
                    xtck <- seq(from = min(axTicks(xax[1])) - stepsize * xntck,
                                to = max(axTicks(1)) + stepsize * xntck,
                                by = stepsize)
            }
            ## minor x-axis tick marks
            lapply(xax, axis, at = xtck, labels = FALSE, tcl = -.2)
        }
        if ((length(yax) == 1 && yax %in% c(2, 4)) || identical(yax, c(2, 4))) {
            ## same for y-axis
            if (grepl("y", log)) {
                ## the log10 range of powers that need something done
                ypow <- c(
                    if (ylim[1] == 0) -100
                    else if (ylim[1] > 0) floor(log10(ylim[1]))
                    else - floor(log10(-ylim[1])),
                    if (ylim[2] == 0) -100
                    else if (ylim[2] > 0) ceiling(log10(ylim[2]))
                    else -ceiling(log10(-ylim[2]))) 
                if (is.null(ytck)) 
                    ytck <- c(1:10 %o% 10^((ypow)[1]:(ypow)[2]))
                ## log x-axis with nicer 10^xpow labels
                lapply(yax, axis, at = 10^(ypow[1]:ypow[2]),
                       labels = sapply((ypow)[1]:(ypow)[2], # or -xpow[1]?
                                       function(i) {
                                           as.expression(bquote(10^ .(i)))}),
                       las = las)
            } else {
                ## non-log y-axis
                lapply(yax, axis, las = las)
                ## find stepsize used in default axis
                stepsize <- abs(diff(axTicks(yax[1])[1:2])) / yntck
                if (is.null(ytck)) 
                    ## add xntck ticks between
                    ytck <- seq(from = min(axTicks(yax[1])) - stepsize * yntck,
                                to = max(axTicks(yax[1])) + stepsize * yntck,
                                by = stepsize)
            }
            ## minor y-axis tick marks
            lapply(yax, axis, at = ytck, labels = FALSE, tcl = -.2)
        }
        ## add axis labels
        if ((length(xax == 1) && xax %in% c(1, 3)) || identical(xax, c(1, 3)))
            lapply(xax, function(i) {
                mtext(xlab, side = i, line = if (grepl("x", log) || las == 1) 3 else 2)})
        if ((length(yax == 1) && yax %in% c(2, 4)) || identical(yax, c(2, 4)))
            lapply(yax, function(i) {
                mtext(ylab, side = i, line = if (grepl("y", log) || las == 1) 3 else 2)})
        if (GTS) {
            addGTS(agedir = agedir, Era = F, Period = T, Epoch = T, Age = F, frac = GTSfrac)
        }
    }
    
    ## add corner ABC
    if(!is.null(abc)) {
        if (is.null(abcadj)) abcadj <- 0.04 * abs(diff(par("usr")[1:2])) 
        addABC(abc, abcadj)
    }
}


errorBarsPlot <- function(age, var = NULL, error, 
                          width = diff(range(age)) / 100,
                          col = "#BEBEBEE6",
                          agedir = "h") {
    if (is.data.frame(error) || is.matrix(error) && ncol(error) == 2) {
        if (!(nrow(error) == 1 || nrow(error) == length(var)))
            warning("Number of rows in error not equal to var length, recycling")
        if (agedir == "h")
            arrows(age, error[, 1], age, error[,2], col = col, length = 0.05,
                   angle = 90, code = 3)
        else if (agedir == "v")
            arrows(error[, 1], age, error[, 2], age, col = col, length = 0.05,
                   angle = 90, code = 3)
    } else {
        if (!(length(error) == length(var) || length(error) == 1))
            warning("Length of error not equal to var length, recycling")
        if (agedir == "h") {
            arrows(x0 = age, y0 = var - error, x1 = age,
                   y1 = var + error, col = col, length = 0.05,
                   angle = 90, code = 3)
        }
        else if (agedir == "v") {
            arrows(x0 = var - error, y0 = age, x1 = var + error,
                   y1 = age, col = col, length = 0.05, angle = 90, code = 3)
        }
    }
}

errorAreaPlot <- function(age, var = NULL, error, 
                          col = "#BEBEBE4D", agedir = "h") {
    if (!is.numeric(error) && is.null(var))
        stop("Provide either dataframe/matrix of low and high error values or vector of relative error values.")
    ##  error is a dataframe/matrix with 2 columns for negative and positive
    ##  absolute error values
    if (is.data.frame(error) || is.matrix(error) && ncol(error) == 2) {
        ##  TODO: support for NA values in age when specifying strict errorvalues
        if (nrow(error) != length(var))
            warning("Number of rows in error not equal to var length, recycling")
        x <- c(age, rev(age))
        y <- c(error[,1], error[,2])
    } else {
        if (anyNA(var) | anyNA(age)) {
            enc <- rle(!is.na(var))             # calculate amount of non-NA polygons
            endIdxs <- cumsum(enc$lengths)      # lengths of polygons
            for (i in seq_along(enc$lengths)){  # for each polygon
                if(enc$values[i]){              # for non-na regions
                    endIdx <- endIdxs[i]
                    startIdx <- endIdx - enc$lengths[i] + 1
                    
                    subdat <- var[startIdx:endIdx]
                    subsd <- error[startIdx:endIdx]
                    subage <- age[startIdx:endIdx]
                    
                    x <- c(subdat - subsd, rev(subdat + subsd))
                    y <- c(subage, rev(subage))
                }
            }
        } else {
            x <- c(age, rev(age))
            y <- c(var - error, rev(var + error))
        }
    }
    polygon(x = if (agedir == "h") x else y, y = if (agedir == "h") y else x,
            col = col, border = NA)
}

##  Takes a dataframe of one or multiple variable(s) to  create a (set of) plot(s)
stratPlot.data.frame <- function(var, # dataframe
                                 age = NULL, # optional vector 
                                 agedir = "h",  # "v", "ver", "vertical" or "h" "hor" "horizontal" 
                                 pol = F, bar = F,        # polygon/bar
                                 gapsize = NULL,  # lines not drawn for timesteps > gapsize
                                 oneplot = F, # logical, if TRUE plot all variables in the same plot
                                 genframe = T, # show plots in same window
                                 add = F, error = NULL,    # vector of errors to plot (note: relative values!)
                                 stacked = F, # logical, calculate cumulative sum for vars
                                 ..., xax = if (agedir == "h") 1 else 3, # default position of x-axis
                                 log = "",
                                 yax = 2,         # default position of yaxis
                                 las = 1,         # default direction of axis labels
                                 mar = "auto",    # generated based on xax and yax
                                 ylab = NULL, xlab = NULL, xlim = NULL,
                                 ylim = NULL, bty = "n", col = "black",
                                 lwd = 1, lty = 1, type = "o", pch = 16,
                                 errortype = "bars", errorcol = "#BEBEBEE6",
                                 pol0 = NULL, polcol = "#4682BEE6", border = NULL,
                                 legend = NULL) {
    ## subset numeric columns
    ## var <- var[, sapply(var, is.numeric)]
    ##  TODO: do this as option?

    ##  check var and age
    if (!is.null(age)){
        if (length(age) != nrow(var)) {
            stop("Length of age is unequal to nrow(var)")
        }
        ##  TODO add gapmaker support!
        message("Assuming only variables in dataframe")
    } else { # only var is provided
        ##  find column that has age/depth
        depthcol <- grep("depth", names(var), ignore.case = TRUE)
        if (length(depthcol) > 1) warning("multiple depth columns found, using first")
        agecol <- grep("age|time", names(var), ignore.case = TRUE)
        if (length(agecol) > 1) warning("multiple age columns found, using first")
        ## depthcol found but agecol isn't
        if (length(depthcol) >= 1 && length(agecol) == 0) {
            ##  check gapsize
            if (!is.null(gapsize)) {
                var <- gapMaker(var, gapsize = gapsize, varname = depthcol[1])
            }
            ## extract age/depth from var
            age <- var[, depthcol[1]]
            var <- var[, -depthcol[1]]

            ##  overwrite agelab if default
            if (agedir == "h") {
                if (is.null(xlab)) xlab <- "Depth (mbsf)"
            } else if (agedir == "v") {
                if (is.null(ylab)) ylab <- "Depth (mbsf)"
            }
        ## agecol found but depthcol isn't
        } else if (length(depthcol) == 0 && length(agecol) >= 1) {
            ##  check gapsize
            if (!is.null(gapsize)) {
                var <- gapMaker(var, gapsize = gapsize, varname = agecol[1])
            }
            ## extract age/depth from var
            age <- var[, agecol[1]]
            var <- var[, -agecol[1]]
        ## both agecol and depthcol found, using agecol
        } else if (length(depthcol) > 0 && length(agecol) > 0) {
            ##  TODO: interactive selection of desired age
            ##  check gapsize
            if (!is.null(gapsize)) {
                var <- gapMaker(var, gapsize = gapsize, varname = agecol[1])
            }
            ## extract age/depth from var
            age <- var[, agecol[1]]  # for now we just use age if both are available
            ##  omit depth and age
            var <- var[, - c(depthcol[1], agecol[1])] 
        } else { # no depthcol, no agecol found: use first column
            ##  check gapsize
            if (!is.null(gapsize)) {
                var <- gapMaker(var, gapsize = gapsize, varname = agecol[1])
            }
            ## extract age/depth from var
            age  <- var[,  1]
            var  <- var[, -1]
            message("Assuming age or depth in first column of var")
        }
        
    }

    ##  number of variables after extraction of age
    if (is.vector(var)) nvar <- 1 else nvar <- ncol(var)

    ##  parsing of x- and ylab 
    if (agedir == "h") {
        if (!is.null(ylab)) {
            ## TODO: parse ylab in similar manner as xlab, with proper expression check
            if (is.character(ylab) && length(ylab) > 1 && length(ylab) != nvar){ 
                stop("Incorrect length of ylab")
            }
            if (class(ylab) == "formula"){
                if (length(ylab == 1)) {
                    ylab <- as.expression(ylab)
                } else {
                    lapply(ylab, as.expression)
                }
            }
        } else {
            message("Using varnames as ylabs")
            ylab <- names(var)
        }
        if (length(ylab) > 1 && length(ylab) == nvar) {
            ylabs <- ylab
        }
    } else { # agedir = v
        if (!is.null(xlab)) {
            if(length(xlab) > 1 && length(xlab) != nvar){ 
                stop("Incorrect length of xlab")
            }
            if (class(xlab) == "formula"){
                if(length(xlab == 1)) {
                    xlab <- as.expression(xlab)
                } else {
                    xlabs <- lapply(xlab, as.expression)
                }
            }
        } else {
            message("Using varnames as xlabs")
            xlabs <- names(var)
        }
        if (length(xlab) > 1 && length(xlab) == nvar) {
            xlabs <- xlab
        }
    }

    ##  specific plotting variables can also be defined for all variables in var
    if (length(type) > 1) {
        if (length(type) == nvar) {
            types <- type
        } else stop("Incorrect length of type")
    } 
    
    if (length(pch) > 1) {
        if (length(pch) == nvar) {
            pchs <- pch
        } else stop("Incorrect length of pch")
    }

    if (length(col) > 1) {
        if (length(col) == nvar) {
            cols <- col
        } else stop("Incorrect length of col")
    } else cols <- NULL
    
    if (length(lty) > 1) {
        if (length(lty) == nvar) {
            ltys <- lty
        } else stop("Incorrect length of lty")
    }
    
    if (length(lwd) > 1) {
        if (length(lwd) == nvar) {
            lwds <- lwd
        } else stop("Incorrect length of lwd")
    }

    if (length(pol) > 1) {
        if (length(pol) == nvar) {
            pols <- pol
        } else stop("Incorrect length of pol")
    }

    if (length(polcol) > 1) {
        if (length(polcol) == nvar) {
            polcols <- polcol
        } else stop("Incorrect length of fillcol")
    }

    if (length(bar) > 1) {
        if (length(bar) == nvar) {
            bars <- bar
        } else stop("Incorrect length of bar")
    }

    if (is.list(ylim)){
        if (length(ylim) == nvar) {
            ylims <- ylim
        } else stop("Incorrect length of ylim")
    }
    ##  TODO: also do this for bty?, errortype, errorcol, pol0 and border?
    if (length(log) > 1) {
        if (length(log) == nvar) {
            logs <- log
        } else stop("Incorrect length of log")
    }

   
    ##  set up the plotting region
    ##  last is to check if it wasn't a df with only 2 columns
    if (!oneplot && genframe && is.data.frame(var)) {  
        if (agedir == "h") {
            par(mfrow = c(nvar, 1))
        } else {
            par(mfrow = c(1, nvar))
        }
    }
    
    ##  call stratPlot.numeric, multiple times if necessary
    for (i in seq_len(nvar)) {
        stratPlot(if (nvar == 1) { var } else if (stacked) { rowSums(var[, 1:i]) } else { var[, i] },
                  age = age, agedir = agedir,
                  pol = if (exists("pols")) pols[i] else pol,
                  ## pol = pol[i],
                  bar = if (exists("bars")) bars[i] else bar,
                  add = if (is.null(add)) { if (oneplot && i != 1) TRUE else FALSE } else { add },
                  error = error, xax = xax, yax = yax, mar = mar, ## TODO: change exists check for something else b/c it currently uses the global space
                  ylab = if (exists("ylabs")) ylabs[i] else ylab,
                  xlab = if (exists("xlabs")) xlabs[i] else xlab,
                  xlim = if (oneplot && age == "v") range(var, na.rm = TRUE) else xlim,
                  ylim = if (oneplot && age == "h") range(var, na.rm = TRUE) else if (exists("ylims")) ylims[[i]] else ylim,
                  bty = bty, lty = if (exists("ltys")) ltys[i] else lty,
                  col = if (!is.null(cols)) cols[i] else col,
                  lwd = if (exists("lwds")) lwds[i] else lwd,
                  type = if (exists("types")) types[i] else type,
                  pch = if (exists("pchs")) pchs[i] else pch,
                  log = if (exists("logs")) logs[i] else log,
                  errortype = errortype, errorcol = errorcol, pol0 = pol0,
                  polcol = polcol, border = border, ...)
    }
    if (!is.null(legend)) {
        legend("topright", legend = legend, col = if(exists("cols")) cols else col,
               lty = if (exists("ltys")) ltys else lty,
               lwd = if (exists("lwds")) lwds else lwd,
               pch = if (exists("pchs")) pchs else pch)
    }
}

stratPlot.list <- function(ls, ...) {
    lapply(ls, stratPlot, ...)
}

##  subset dataframe to range of age/depth
subsetRange <- function(dat, min, max, column = "depth") {
    return(dat[dat[, column] > min & dat[, column] < max, ])
}

##  insert empty rows between values that differ more than gapsize
gapMaker <- function(df, gapsize = .5, varname = "age") {
    toolarge <- which(diff(df[, varname]) > gapsize)
    df <- insertEmpty(df, toolarge)
    return(df)
}

##  insert empty rows in df after afterrow
insertEmpty <- function(df, afterrow) {
    ##  create indices for the data order
    df$id <- seq_len(nrow(df))
    df[max(df$id) + seq_along(afterrow), ] <- NA
    df$id[is.na(df$id)] <- afterrow + .5
    df <- df[order(df$id), ]
    df <- df[ , !names(df) %in% "id"]
    return(df)
}

## add large a/b/c in top left corner of current plot
addABC <- function(char = "A", xadj = 0.04 * abs(diff(par("usr")[1:2])),
                    yadj = 0.04 * abs(diff(par("usr")[3:4])), cex = 2) {
    ## TODO: also work for log axes
    xpos <- par("usr")[1] + xadj
    ypos <- par("usr")[4] + yadj
    mtext(char, 3, at = xpos, cex = cex)
    ## text(xpos, ypos, char, cex = cex, xpd = NA)
}

addGTS <- function(age, frac = 0.05, agelim = range(age), agedir = "h", Era = F, Period = F, Epoch = T, Age = F ) {
    GTS <- read.csv("~/Dropbox/DepthPlotter/GTS_colours.csv")
    GTS$hex <- rgb(GTS$R, GTS$G, GTS$B, maxColorValue = 255)
    GTS$mean <- (GTS$end - GTS$start) / 2 + GTS$start

    if (Era)    Eras <- GTS[GTS$type == "Era", ]
    if (Period) Periods <- GTS[GTS$type == "Period", ]
    if (Epoch)  Epochs <- GTS[GTS$type == "Epoch", ]
    if (Age)    Ages <- GTS[GTS$type == "Age", ]
    
    ## new reference frame so that y axis is 0,1
    mar <- par(no.readonly = TRUE)$mar   
    par(new = T, mar = mar)
    plot(if (agedir == "h") agelim else c(0, 1),
         if (agedir == "h") c(0, 1) else agelim,
         xaxs = if (agedir == "h") "r" else "i",
         yaxs = if (agedir == "h") "i" else "r",
         xlim = if (agedir == "h") range(age) else c(0, 1),
         ylim = if (agedir == "h") c(0, 1) else rev(range(age)),
         type = "n", axes = F, xlab = "", ylab = "")
    rect(if (agedir == "h") Epochs$start else 0, if (agedir == "h") 0 else Epochs$start,
         if (agedir == "h") Epochs$end else frac, if (agedir == "h") frac else Epochs$end, col = Epochs$hex)
    text(if (agedir == "h") Epochs$mean else mean(c(0, frac)), if (agedir == "h") mean(c(0, frac)) else Epochs$mean,
         labels = Epochs$name, srt = if (agedir == "h") 0 else 90)
}

darker <- function(color = col, factor=1.4){
    col <- col2rgb(color)
    col <- col/factor
    col <- rgb(t(col), maxColorValue=255)
    col
}

lighter <- function(color = col, factor=1.4){
    col <- col2rgb(color)
    col <- col*factor
    col <- rgb(t(as.matrix(apply(col, 1, function(x) if (x > 255) 255 else x))), maxColorValue=255)
    col
}

