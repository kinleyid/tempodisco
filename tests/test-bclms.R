
lib_path <- 'C:/Users/isaac/Projects/ddDesidModels'
roxygen2::roxygenise(lib_path)
devtools::load_all(lib_path)

# Generate data
p_imm <- list(
  'normal' = function(df) plogis( 1*( df$val_imm - df$val_del* 1 / (1 + 0.001*(df$del)) ) ) # Normal
  # 'all-imm' = function(df) 1, # All immediate chosen
  # 'all-del' = function(df) 0, # All delayed chosen
  # 'random' = function(df) runif(nrow(df)) # Random
)
datasets <- list()
for (ptpt in names(p_imm)) {
  df <- data.frame(val_imm = seq(1, 99, length.out = 10), val_del = 100, del = rep(exp(1:10), each=10))
  df$imm_chosen <- runif(nrow(df)) < p_imm[[ptpt]](df)
  datasets[[ptpt]] <- df
}

# Get all possible combinations of inputs
arg_vals <- list(model = eval(formals(tdbclm)[['model']]))
arg_combos <- do.call(expand.grid, c(arg_vals, list(stringsAsFactors = F)))

for (ptpt in names(datasets)) {
  cat(sprintf('Dataset: %s\n', ptpt))
  df <- datasets[[ptpt]]
  
  arg_combo_idx <- 1
  while (arg_combo_idx <= nrow(arg_combos)) {
    arg_combo <- list(model = arg_combos[arg_combo_idx, ])
    arg_combo_idx <- arg_combo_idx + 1
    cat(sprintf('Arguments:\n'))
    print(arg_combo)
    
    arg_combo <- as.list(unlist(arg_combo))
    
    mod <- do.call(tdbclm, c(list(data = df), arg_combo))
    
    # random generics
    print(mod)
    ED50(mod)
    AUC(mod)
    BIC(mod)
    AIC(mod)
    AIC(mod, k = 5)
    logLik(mod)
    coef(mod)
    coef(mod, df_par = T)
    residuals(mod, type = 'deviance')
    residuals(mod, type = 'pearson')
    residuals(mod, type = 'response')
    
    # plotting
    plot(mod, type = 'summary')
    plot(mod, type = 'endpoints')
    plot(mod, type = 'scores')

    # prediction
    fitted(mod)
    predict(mod, newdata = data.frame(del = 0:1000), type = 'indiff')
  }
}
