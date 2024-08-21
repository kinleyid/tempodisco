
lib_path <- 'C:/Users/isaac/Projects/ddDesidModels'
roxygen2::roxygenise(lib_path)
devtools::load_all(lib_path)

# Generate data
indiff_fns <- list(
  'normal' = function(df) plogis(rnorm(nrow(df)) + qlogis(1 / (1 + 0.001*(df$del)))), # Normal with a little noise
  'random' = function(df) runif(nrow(df)) # Random
)
datasets <- list()
for (ptpt in names(indiff_fns)) {
  df <- data.frame(del = exp(1:10))
  df$indiff <- indiff_fns[[ptpt]](df)
  datasets[[ptpt]] <- df
}

# Get all possible combinations of inputs
arg_vals <- list(discount_function = eval(formals(td_fn)[['predefined']]))
custom_discount_function <- list(
  name = 'custom',
  fn = function(data, p) 1 / (1 + log(data$del*p['m'] + 1)),
  par_starts = list(m = c(0.001, 0.1)),
  par_lims = list(m = c(0, Inf)),
  ED50 = function(p) 'non-analytic'
)
arg_vals[['discount_function']] <- c(arg_vals[['discount_function']], 'custom')
arg_combos <- do.call(expand.grid, c(arg_vals, list(stringsAsFactors = F)))

for (ptpt in names(datasets)) {
  cat(sprintf('Dataset: %s\n', ptpt))
  df <- datasets[[ptpt]]
  
  arg_combo_idx <- 1
  while (arg_combo_idx <= nrow(arg_combos)) {
    arg_combo <- list(discount_function = arg_combos[arg_combo_idx, ])
    arg_combo_idx <- arg_combo_idx + 1
    cat(sprintf('Arguments:\n'))
    print(arg_combo)
    
    arg_combo <- as.list(unlist(arg_combo))
    if (arg_combo$discount_function == 'custom') {
      arg_combo$discount_function <- custom_discount_function
    }
    mod <- do.call(td_ipm, c(list(data = df), arg_combo))
    
    # random generics
    print(mod)
    ED50(mod)
    AUC(mod)
    BIC(mod)
    AIC(mod)
    AIC(mod, k = 5)
    logLik(mod)
    coef(mod)
    residuals(mod, type = 'pearson')
    residuals(mod, type = 'response')
    
    # plotting
    plot(mod)

    # prediction
    fitted(mod)
    predict(mod, newdata = data.frame(del = 0:1000), type = 'indiff')
  }
}
