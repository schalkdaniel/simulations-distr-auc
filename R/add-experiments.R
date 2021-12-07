fAlgo = function(job, data, instance, base_seed, nf, margin, i) {

  alpha = 0.05
  auc_emp = 1

  df_roc  = generateROCData(i, base_seed)
  auc_emp = logitAUC(df_roc, unlogit = TRUE)
  nsim    = nrow(df_roc)
  npos    = sum(df_roc$truth)
  method  = c("Bootstrap Empirical", "Bootstrap ROC-GLM", "DeLong Empirical", "DeLong ROC-GLM")

  e = try({
    df_roc_glm = df_roc

    x = sort(df_roc_glm$score)
    dfs = numeric(nsim - 2 * margin)
    for (k in seq_along(x)[-c(seq_len(margin), nsim - rev(seq_len(margin)) + 1)]) {
      dfs[k - margin] = mean(abs(x[k] - c(x[k - margin], x[k + margin])))
    }
    df_roc_glm$score = df_roc_glm$score + runif(nsim, -sd(dfs) * nf, sd(dfs) * nf)
    df_roc_glm$score = ifelse(df_roc_glm$score < 0, 0, df_roc_glm$score)
    df_roc_glm$score = ifelse(df_roc_glm$score > 1, 1, df_roc_glm$score)

    noise = sd(dfs) * nf

    auc_roc = rocGLMlogitAUC(df_roc_glm, ind = seq_len(nsim), unlogit = TRUE)
    auc_roc_params = attr(auc_roc, "params")

    df_aucs = data.frame(auc_emp = auc_emp, auc_roc = auc_roc, auc_roc_param1 = auc_roc_params[1],
      auc_roc_param2 = auc_roc_params[2], n = nsim, npos = npos, threshold = 0.5, noise = noise,
      noise_factor = nf, margin = margin)

    boot_emp = boot::boot(data = df_roc, statistic = logitAUC, R = 1000L, stype = "i")
    boot_app = boot::boot(data = df_roc_glm, statistic = rocGLMlogitAUC, R = 1000L, stype = "i")

    var_auc = deLongVar(scores = df_roc$score, truth = df_roc$truth)
    ci_emp = pepeCI(log(auc_emp / (1 - auc_emp)), alpha, var_auc)
    ci_app = pepeCI(log(auc_roc / (1 - auc_roc)), alpha, var_auc)

    lauc_emp = log(auc_emp / (1 - auc_emp))
    lauc_roc = log(auc_roc / (1 - auc_roc))

    df_cis = data.frame(
      method  = method,
      lower   = c(
        quantile(boot_emp$t, alpha / 2, na.rm = TRUE),
        quantile(boot_app$t, alpha / 2, na.rm = TRUE),
        ci_emp[1],
        ci_app[1]),
      upper   = c(
        quantile(boot_emp$t, 1 - alpha / 2, na.rm = TRUE),
        quantile(boot_app$t, 1 - alpha / 2, na.rm = TRUE),
        ci_emp[2],
        ci_app[2]),
      log_auc = c(lauc_emp, lauc_roc, lauc_emp, lauc_roc))

    list(aucs = df_aucs, cis = df_cis)
  }, silent = TRUE)

  if (class(e) == "try-error") {
    df_aucs = data.frame(auc_emp = auc_emp, auc_roc = NA, auc_roc_param1 = NA, auc_roc_param2 = NA,
      n = nsim, npos = npos, threshold = 0.5, noise = noise, noise_factor = nf, margin = margin)

    df_cis = data.frame(
      method  = method,
      lower   = rep(NA, 4),
      upper   = rep(NA, 4),
      log_auc = rep(NA, 4)
    )

    out = list(aucs = df_aucs, cis = df_cis)
  } else {
    out = e
  }
  return(out)
}

addProblem("dummy")
addAlgorithm(name = "auc-values", fun = fAlgo)
addExperiments(algo.design = list('auc-values' = expand.grid(
  base_seed  = BASE_SEED,
  nf         = NOISE_FACTOR,
  margin     = MARGIN,
  i          = seq_len(REPETITIONS))))
