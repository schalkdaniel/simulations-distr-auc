generateROCData = function(i, seed) {
  seed_k = seed + i

  # Simulate data:

  set.seed(seed_k)
  nsim = sample(x = seq(from = 100L, to = 2500L), size = 1L)
  npos = nsim

  set.seed(seed_k)
  scores = runif(n = nsim, min = 0, max = 1)
  truth = ifelse(scores > 0.5, 1, 0)

  seed_add_gamma = 0L
  while ((npos / nsim > 0.9) || (npos / nsim < 0.1)) {

    set.seed(seed_k + seed_add_gamma)
    shuffle = runif(n = 1L, min = 0, max = 1)
    nshuffle = trunc(nsim * shuffle)

    set.seed(seed_k + seed_add_gamma)
    idx_shuffle = sample(x = seq_len(nsim), size = nshuffle)

    set.seed(seed_k + seed_add_gamma)
    truth[idx_shuffle] = sample(x = c(0,1), size = nshuffle, replace = TRUE)
    npos = sum(truth)

    seed_add_gamma = seed_add_gamma + 1L
  }

  return(data.frame(score = scores, truth = truth))
}


probitDevianceStop = function (y, X, beta, beta_old)
{

#   browser()
  dev = -2 * log(probitLikelihood(y, X, beta))
  dev_old = -2 * log(probitLikelihood(y, X, beta_old))

  # from ?glm.control:
  out = abs(dev - dev_old) / (abs(dev) + 0.1)
  return (out)
}

probitLikelihood = function (y, X, beta, w = NULL)
{
  eta = X %*% beta

  if (is.null(w)) {
    w = rep(1, times = nrow(X))
  }
  lh = pnorm(eta)^y * (1 - pnorm(eta))^(1 - y)
  prod(lh^w)
}
probitDeviance = function (y, X, beta)
{
  -2 * log(probitLikelihood(y, X, beta))
}


calculateLambda = function (y, X, beta)
{
  eta = X %*% beta
  q = 2 * y - 1
  qeta = q * eta

  return ((dnorm(qeta) * q) / (pnorm(qeta)))
}

calcU = function (tset, placement_values)
{
  tset_sorted = sort(tset)
  out = vapply(X = tset, FUN.VALUE = integer(length(placement_values)),
    FUN = function (t) { ifelse(placement_values <= t, 1L, 0L) })
    return (out)
}

dataRocGLM = function (U, tset)
{
  roc_glm_data = data.frame(
    y = rep(c(0, 1), times = length(tset)),
    x = rep(qnorm(tset), each = 2L),
    w = as.vector(apply(U, 2, function (x) c(sum(x == 0), sum(x == 1)))))
  return (roc_glm_data)
}


rocData = function (score, truth)
{

  checkmate::assertNumeric(x = score, any.missing = FALSE, len = length(truth))
  checkmate::assertIntegerish(x = truth, lower = 0, upper = 1, any.missing = FALSE, len = length(score))

  label = truth[order(score, decreasing = TRUE)]

  n_pos = sum(label == 1)
  n_neg = sum(label == 0)

  tpr = c(0, cumsum(label == 1) / n_pos)
  fpr = c(0, cumsum(label == 0) / n_neg)

  return (list(tpr = tpr, fpr = fpr))
}

integrateBinormal = function (params)
{
  temp = function (x) pnorm(params[1] + params[2] * qnorm(x))
  int = integrate(f = temp, lower = 0, upper = 1)
  return (int$value)
}

rocGLMlogitAUC = function (data, ind = NULL, unlogit = FALSE)
{
  if (is.null(ind[1])) ind = seq_len(nrow(data))

  scores = data$score[ind]
  truth = data$truth[ind]

  Fn_global = ecdf(scores[truth == 0])
  Sn_global = function (x) 1 - Fn_global(x)

  thresh_set = seq(0, 1, length.out = 30L)
  pv_global = Sn_global(scores[truth == 1])
  U_global = calcU(thresh_set, pv_global)

  roc_data_global = dataRocGLM(U = U_global, tset = thresh_set)
  roc_data_global = roc_data_global[is.finite(roc_data_global$x),]

  y_global = roc_data_global$y
  X_global = model.matrix(y ~ x, data = roc_data_global)
  w_global = roc_data_global$w

  my_roc_glm_global = tryCatch(
    expr = {probitRegr(y = y_global, X = X_global, w = w_global)},
    error = function (e) return ("fail")
  )
  if (! is.character(my_roc_glm_global)) {
    auc = integrateBinormal(my_roc_glm_global$parameter)
    attr(auc, "params") = my_roc_glm_global$parameter
    if (unlogit) return (auc)
    return(log(auc / (1 - auc)))
  } else {
    return (NA)
  }
}

logitAUC = function (data, ind = NULL, unlogit = FALSE)
{
  if (is.null(ind[1])) ind = seq_len(nrow(data))
  scores = data$score[ind]
  truth = data$truth[ind]
  emp_auc = mlr::measureAUC(probabilities = scores, truth = truth, negative = 0, positive = 1)
  if (unlogit) (return (emp_auc))
  return (log(emp_auc / (1 - emp_auc)))
}

logitToAUC = function (x) 1 / (1 + exp(-x))
toLogit = function (x) log(x / (1 - x))

deLongVar = function (scores, truth) {
  # survivor functions for diseased and non diseased:
  s_d = function (x) 1 - ecdf(scores[truth == 1])(x)
  s_nond = function (x) 1 - ecdf(scores[truth == 0])(x)

  # Variance of empirical auc after DeLong:
  var_auc = var(s_d(scores[truth == 0])) / sum(truth == 0) + var(s_nond(scores[truth == 1])) / sum(truth == 1)

  return (var_auc)
}

pepeCI = function (logit_auc, alpha, var_auc)
{
  logit_auc + c(-1, 1) * qnorm(1 - alpha/2) * sqrt(var_auc) / (logitToAUC(logit_auc) * (1 - logitToAUC(logit_auc)))
}


