# DepthPlotter
The DepthPlotter function allows you to easily create depth profile plots in R. 

## Usage
The simplest way to create a single plot is to give depth and var vectors of equal length, 
and provide a value for `xlab`. Note that if you want to use superscipts and subscripts in 
the x-axis label, you can do so by providing `xlab` with a formula, such as: `Label~with~nice~formatting~(H[2]*O~m^{-2}~mu*M)`.

You can also provide `var` with a matrix or dataframe of multiple variables. `xlab` can 
now also be entered as vector of characters or a list of formulae.

It is also possible to provide one dataframe/matrix with the depth-values in it. By 
default, the function assumes that your dataframe has the depth information in the first 
column. If this is not the case you can specify it with `depthcol`. If you want to easily 
subset your dataframe, you can use `sscols` to provide column indices of the variables of
interest.

Other configurations are also possible, just look at the code and see what it does :).