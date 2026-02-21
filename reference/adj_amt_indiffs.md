# Indifference points from adjusting amount procedure

Compute indifference points for data from an adjusting amount procedure
(also called a "titrating procedure").

## Usage

``` r
adj_amt_indiffs(data, block_indic = "del", order_indic = NULL)
```

## Arguments

- data:

  A dataframe where each row corresponds to a binary choice, with at
  least columns `val_imm`, `val_del`, and `imm_chosen`, along with a
  block indicator and (if applicable) an order indicator.

- block_indic:

  Column name of the block indicator—i.e., the column that will identify
  a block of trials for which an indifference point should be computed.
  If unspecified, defaults to `'del'`, which assumes that each block
  corresponds to a different delay.

- order_indic:

  Column name of the order indicator—i.e., the column that specifies the
  order in which trials were completed. Sorting by this column within a
  block should sort the rows in chronological order. If unspecified, the
  rows are assumed to already be in chronological order.

## Value

A dataframe with two columns: one for the block indicator and another
for the corresponding indifference point.

## Examples

``` r
# \donttest{
data("adj_amt_sim")
adj_amt_indiffs(adj_amt_sim)
#>   del   indiff
#> 1   7 0.984375
#> 2  30 0.859375
#> 3  90 0.046875
#> 4 180 0.453125
#> 5 360 0.015625
adj_amt_indiffs(adj_amt_sim, block_indic = 'del', order_indic = 'trial_idx')
#>   del   indiff
#> 1   7 0.984375
#> 2  30 0.859375
#> 3  90 0.046875
#> 4 180 0.453125
#> 5 360 0.015625
# }
```
