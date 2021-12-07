
updateParam = function (beta, X, lambda, w = NULL)
{
  # browser()
  W = diag(as.vector(lambda * (X %*% beta + lambda)))
  if (! is.null(w[1])) {
    X = diag(sqrt(w)) %*% X
    lambda = diag(sqrt(w)) %*% lambda
  }
  return (beta + solve(t(X) %*% W %*% X) %*% t(X) %*% lambda)
}

probitRegr = function (y, X, w = NULL, beta_start = 0, stop_tol = 1e-8, iter_max = 25L, trace = FALSE)
{
  if (length(beta_start) == 1) beta_start = rep(beta_start, ncol(X))
  if (is.vector(beta_start)) beta_start = cbind(beta_start)

  beta = beta_start
  iter = 0L

  deviance = numeric(iter_max)
  deviance[1] = probitDeviance(y, X, beta)

  if (trace) cat("\n")

  while (iter <= iter_max) {

    beta_start = beta
    beta = updateParam(beta_start, X = X, lambda = calculateLambda(y = y, X = X, beta = beta_start), w = w)

    iter = iter + 1L
    deviance[iter + 1L] = probitDeviance(y, X, beta)

    if (trace) cat("Deviance of iter", iter, "=", round(deviance[iter + 1L], digits = 4L), "\n")

    if (probitDevianceStop(y = y, X = X, beta = beta, beta_old = beta_start) < stop_tol) { if (trace) { cat("\n"); break; } }
  }
  out = list(iter = iter, parameter = beta, deviance = deviance[seq_len(iter + 1L)])
  return (out)
}

predictProbit = function (mod)
{
  x = seq(0, 1, 0.01)
  y = pnorm(mod$parameter[1] + mod$parameter[2] * qnorm(x))

  return(list(x = x, y = y))
}

