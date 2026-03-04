# Get all available pre-defined discount functions

Get a list of all of the available pre-defined discount functions.

## Usage

``` r
get_available_discount_functions()
```

## Value

A character vector containing the names of the avialable pre-defined
discount functions.

## Examples

``` r
# \donttest{
get_available_discount_functions()
#>  [1] "hyperbolic"                 "nonlinear-time-hyperbolic" 
#>  [3] "exponential"                "nonlinear-time-exponential"
#>  [5] "absolute-stationarity"      "relative-stationarity"     
#>  [7] "power"                      "nonlinear-time-power"      
#>  [9] "arithmetic"                 "nonlinear-time-arithmetic" 
#> [11] "inverse-q-exponential"      "scaled-exponential"        
#> [13] "scaled-hyperbolic"          "fixed-cost"                
#> [15] "dual-systems-exponential"   "additive-utility"          
#> [17] "model-free"                 "constant"                  
# }
```
