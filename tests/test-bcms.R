
lib_path <- 'C:/Users/isaac/Projects/ddDesidModels'
roxygen2::roxygenise(lib_path)
devtools::load_all(lib_path)

# Generate data
p_imm <- list(
  'normal' = function(df) plogis( 2*( df$val_imm - df$val_del* 1 / (1 + 0.001*(df$del)) ) ), # Normal
  'all-imm' = function(df) 1, # All immediate chosen
  'all-del' = function(df) 0, # All delayed chosen
  'random' = function(df) runif(nrow(df)) # Random
)
datasets <- list()
for (ptpt in names(p_imm)) {
  df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
  df$imm_chosen <- runif(nrow(df)) < p_imm[[ptpt]](df)
  datasets[[ptpt]] <- df
}

# Get all possible combinations of inputs
arg_vals <- list(fixed_ends = c(T, F), fit_err_rate = c(T, F))
for (arg in c('discount_function', 'choice_rule')) {
  arg_vals[[arg]] <- eval(formals(td_bcm)[[arg]])
}
custom_discount_function <- list(
  name = 'custom',
  fn = function(data, p) 1 / (1 + log(data$del*p['m'] + 1)),
  par_starts = list(m = c(0.001, 0.1)),
  par_lims = list(m = c(0, Inf)),
  ED50 = function(p) 'non-analytic'
)
arg_vals[['discount_function']] <- c(arg_vals[['discount_function']], 'custom')
arg_combos <- do.call(expand.grid, c(arg_vals, list(stringsAsFactors = F)))

# Only need to test the "all" discount function once
all_idx <- which(arg_combos$discount_function == 'all')
arg_combos <- arg_combos[-all_idx[-1], ]

for (ptpt in names(datasets)) {
  cat(sprintf('Dataset: %s\n', ptpt))
  df <- datasets[[ptpt]]
  
  # Default call
  td_bcm(df)
  
  arg_combo_idx <- 1
  while (arg_combo_idx <= nrow(arg_combos)) {
    arg_combo <- arg_combos[arg_combo_idx, ]
    arg_combo_idx <- arg_combo_idx + 1
    cat(sprintf('Arguments:\n'))
    print(arg_combo)
    
    arg_combo <- as.list(unlist(arg_combo))
    if (arg_combo$discount_function == 'custom') {
      arg_combo$discount_function <- custom_discount_function
    }
    
    mod <- do.call(td_bcm, c(list(data = df), arg_combo))
    
    # random generics
    print(mod)
    ED50(mod)
    AUC(mod)
    BIC(mod)
    AIC(mod)
    AIC(mod, k = 5)
    logLik(mod)
    coef(mod)
    residuals(mod, type = 'deviance')
    residuals(mod, type = 'pearson')
    residuals(mod, type = 'response')
    
    # plotting
    plot(mod)
    plot(mod, type = 'summary')
    plot(mod, type = 'endpoints')
    plot(mod, type = 'endpoints', del = 100, val_del = 50)
    plot(mod, type = 'scores')
    
    # prediction
    fitted(mod)
    predict(mod, type = 'link')
    predict(mod, type = 'response')
    predict(mod, newdata = data.frame(del = 0:1000), type = 'indiff')
  }
}
