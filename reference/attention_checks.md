# Test for failed attention checks

Check whether participants failed attention checks, either choosing an
immediate reward of 0 or choosing a delayed reward equal in face value
to an immediate reward. If the participant was never offered either
choice, a warning is given.

## Usage

``` r
attention_checks(data, warn = FALSE, ppn = FALSE)
```

## Arguments

- data:

  A `data.frame` with columns `val_imm`, `val_del` and `del_chosen`,
  representing data from a single participant.

- warn:

  Logical: give a warning for failed attention checks?

- ppn:

  Logical: return proportions of attention checks participant failed,
  versus absolute numbers?

## Value

Named vector counting the number of times the participant chose an
immediate reward of 0 (`imm_0`) or chose a delayed reward equal in face
value to an immediate reward (`del_eq_imm`).

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
