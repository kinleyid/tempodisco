# Check for invariant responding

Check whether participants always chose the immediate reward or always
chose the delayed reward.

## Usage

``` r
invariance_checks(data, warn = FALSE)
```

## Arguments

- data:

  A `data.frame` with columns `val_imm`, `val_del` and `del_chosen`,
  representing data from a single participant.

- warn:

  Logical: give a warning for invariant responding?

## Value

Named vector specifying whether the participant chose only immediate
rewards (`all_imm`) or chose all delayed rewards (`all_del`).

## Examples

``` r
# \donttest{
# On a model
data("td_bc_single_ptpt")
attention_checks(td_bc_single_ptpt)
#>      imm_0 del_eq_imm 
#>          0          0 
# }
```
