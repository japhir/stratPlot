# stratPlotter
The stratPlot function allows you to easily create depth and age profile plots
in R. 

## Usage

Use your spreadsheat editing skills (or R) to create a .csv that holds one
sample per row with the variables of interest as separate columns. Make sure to
include depth or age information in one of the columns. 

Get it into R using `data <- read.csv("pathtofile.csv")`

The simplest way to create a single plot is to give depth/age and var vectors of
equal length, and provide a value for `xlab`. 

Note that if you want to use superscipts and subscripts in the x-axis label, you
can do so by providing `xlab` with a formula. For example:
`Label~with~nice~formatting~(H[2]*O~m^{-2}~mu*M)` will result in: Label with
nice formatting (H<sub>2</sub>O m<sup>-2</sup> μM).

You can also provide `var` with a matrix or dataframe with the variables of
interst. The function tries to automatically find a column with `depth` or `age`
information and use that to plot the other variables. You can also specify it
with `depthcol`. They can be added to a single plot with `oneplot = TRUE`, can
be stacked with `stacked = TRUE` (useful for cumulative sum plots) or each
create a new plot (default).

When you create multiple plots, make sure there is room for the plots (via
`par(mfrow = c(1,5))` for example). `xlab` can now also be entered as vector of
characters or a list of formulae.

By default var omits non-numeric data (such as sample names etc.). You can also
subset your dataframe, you can use `sscols` to provide column indices of the
variables of interest.

Setting the `pb` variable to `"PB"`, `"P"` or `"B"` sets the plotting type to
polygon with bars, polygon or bars respectively.

Other configurations are also possible, just look at the function arguments and
see what they do :).

## Example calls

```R
# dinocyst data example
set.seed(1)
dinos <- data.frame(code = paste0("IJK", 1:10),
                    depth = seq(600, 800, length.out = 10),
	                age = 41:50,
					Dinospecies1 = rnorm(10, 5, 3),
					Dinospecies2 = rnorm(10, 10, 5),
					Dinospecies3 = rnorm(10, 25, 20))
par(mfrow = c(1, 3))
SuperPlot(dinos, pb = "PB", xlim = c(0, 60))

# data with known error values
set.seed(1)
temp <- data.frame(age = 41:50, 
	               temp = rnorm(10, 30, 5),
				   error = rnorm(10, 2.5, 1))
SuperPlot(temp$temp, temp$age, xlab = Temperature~(degree~C), 
          ylab = "Age (Mya)", error = temp$error)
```
