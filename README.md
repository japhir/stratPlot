# SuperPlotter (formerly DepthPlotter)
The SuperPlot function allows you to easily create depth and age profile plots in R. 

## Usage
The simplest way to create a single plot is to give depth and var vectors of equal length, 
and provide a value for `xlab`. Note that if you want to use superscipts and subscripts in 
the x-axis label, you can do so by providing `xlab` with a formula. For example: `Label~with~nice~formatting~(H[2]*O~m^{-2}~mu*M)` will result in: Label with nice formatting (H<sub>2</sub>O m<sup>-2</sup> μM).

You can also provide `var` with a matrix or dataframe of multiple variables. `xlab` can 
now also be entered as vector of characters or a list of formulae.

It is also possible to provide one dataframe/matrix with the depth or age values in it. By default, the function tries to look up the depth and age columns by rowname.
You can also specify it with `depthcol`. If you want to easily 
subset your dataframe, you can use `sscols` to provide column indices of the variables of
interest.

Setting the `pb` variable to `"PB"`, `"P"` or `"B"` sets the plotting type to polygon with bars, polygon or bars respectively.

Other configurations are also possible, just look at the code and see what it does :).
