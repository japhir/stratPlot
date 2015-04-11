# DepthPlotter

The DepthPlotter function allows you to easily create depth profile plots in R. 

The simplest way to create a single plot is to give depth and var vectors of equal length, and provide a value for xlab. 

Note that if you want to use superscipts and subscripts in the x-axis label, you can do so by providing xlab with something like: "expression(your~plot~with~nice~formatting~(m^{-2})~mu)".

You can also provide var with a matrix or dataframe of multiple variables. xlab can now also be entered as a list of x-axis labels.

You can also provide just one dataframe/matrix with the depth-values in it. By default, the function assumes that your dataframe has the depth information in the first column. If this is not the case you can specify it with depthcol. 

Other configurations are also possible, just look at the code and see what it does :).

