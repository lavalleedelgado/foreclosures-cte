*! version 1.1, Patrick Lavallee Delgado, October 2023

capture program drop miiv
program define miiv, eclass sortpreserve

  * Parse arguments.
  syntax anything [if] [pweight/], [absorb(varname numeric) vce(string asis)]

  * Parse estimation strategy.
  gettoken estimator anything : anything
  capture assert inlist("`estimator'", "2sls", "gmmopt", "gmm3sls", "giv")
  if _rc {
    display as error "`estimator' not a valid estimator"
    error 198
  }

  * Parse instrumental variables syntax.
  _iv_parse `anything'
  local yvar `s(lhs)'
  local Xvar `s(endog)' `s(exog)'
  local Zvar `s(inst)' `s(exog)'

  * Check for multiply imputed data.
  quietly mi query
  local style `r(style)'
  local m `r(M)'
  capture assert `m'
  if _rc {
    display as error "data not mi set"
    error 119
  }

  * Convert to flong data.
  if "`style'" != "flong" {
    mi convert flong, clear
    confirm variable _mi_m
  }

  quietly {

  * Evaluate weights if given.
  tempvar wvar
  gen `wvar' = 1
  if "`weight'" == "pweight" {
    replace `wvar' = `exp'
  }

  * Set constant fixed effect if not available.
  if "`absorb'" == "" {
    tempvar `absorb'
    gen `absorb' = 1
  }

  * Mark estimation sample.
  tempvar touse
  mark `touse' `if' [`weight' = `exp']
  markout `touse' `yvar' `Xvar' `Zvar' `wvar' `absorb'

  * Drop panels with no variation in outcome.
  tempvar sd
  bysort _mi_m `absorb': egen `sd' = sd(`yvar') if `touse'
  replace `touse' = 0 if !`sd'

  * Expand fixed effect.
  fvrevar ibn.`absorb'
  local Jvar `r(varlist)'

  * Run estimation.
  tempname bout Vout Wout Bout dfout ciout Nout
  mata: __miivreg()

  * Post results.
  ereturn post `bout' `Vout', esample(`touse')
  ereturn matrix W_mi = `Wout'
  ereturn matrix B_mi = `Bout'
  ereturn matrix df_mi = `dfout'
  ereturn matrix ci_mi = `ciout'
  ereturn scalar N = `Nout'

  * Post other information.
  ereturn local cmd "miiv"
  ereturn local cmdline "`0'"
  ereturn local devar "`yvar'"

  }

  * Print coefficient table.
  _coef_table, dfmatrix(e(df_mi)) cimatrix(e(ci_mi)) coeftitle("`yvar'")

  * Return data to original shape.
  if "`style'" != "flong" {
    mi convert `style', clear
  }


end

mata:

  void __miivreg() {

    // Get estimator.
    est = st_local("estimator")

    // Get data.
    y = st_data(., st_local("yvar"))
    c = J(rows(y), 1, 1)
    X = st_data(., st_local("Xvar")), c
    Z = st_data(., st_local("Zvar")), c
    w = st_data(., st_local("wvar"))
    J = st_data(., st_local("Jvar"))

    // Get sample and dataset selectors.
    S = st_data(., st_local("touse"))
    M = st_data(., "_mi_m")

    // Sort on fixed effect.
    idx = order(J, range(cols(J), 1, 1)')
    y = y[idx, .]
    X = X[idx, .]
    Z = Z[idx, .]
    w = w[idx, .]
    J = J[idx, .]
    S = S[idx, .]
    M = M[idx, .]

    // Mark missingness in original data.
    S0 = select(X, M :== 0)
    for (i = 1; i <= rows(S0); i++) {
      for (j = 1; j <= cols(S0); j++) {
        S0[i, j] = 1 - missing(S0[i, j])
      }
    }

    // Initialize estimates.
    blist = asarray_create("real", 1)
    Vlist = asarray_create("real", 1)
    nu = colsum(S0)

    // Consider each dataset.
    m = max(M)
    for (i = 1; i <= m; i++) {

      // Update degrees of freedom.
      Sm = select(S, M :== i)
      dfm = colsum(S0 :* Sm)
      for (j = 1; j <= cols(nu); j++) {
        nu[j] = min((nu[j], dfm[j]))
      }

      // Select observations.
      Sm = S :& M :== i
      ym = select(y, Sm)
      Xm = select(X, Sm)
      Zm = select(Z, Sm)
      wm = select(w, Sm)
      Jm = select(J, Sm)

      // Weight observations.
      wm = wm / sum(wm) * rows(wm)
      ym = ym :* wm
      Xm = Xm :* wm
      Zm = Zm :* wm

      // Drop empty fixed effects.
      ok = colsum(Jm) :> 0
      Jm = select(Jm, ok)

      // Run regression.
      __ivreg(est, ym, Xm, Zm, Jm, b = ., V = .)
      asarray(blist, i, b)
      asarray(Vlist, i, V)
    }

    // Completed-data coefficient vector.
    qbar = asarray(blist, 1)
    for (i = 2; i <= m; i++) {
      qbar = qbar + asarray(blist, i)
    }
    qbar = qbar / m

    // Within-imputation variance-covariance matrix.
    Ubar = J(rows(qbar), rows(qbar), 0)
    for (i = 1; i <= m; i++) {
      Ubar = Ubar + asarray(Vlist, i)
    }
    Ubar = Ubar / m

    // Between-imputation variance-covariance matrix.
    B = J(rows(qbar), rows(qbar), 0)
    for (i = 1; i <= m; i++) {
      q = asarray(blist, i)
      B = B + (q - qbar) * (q - qbar)' / (m - 1)
    }

    // Total variance.
    T = Ubar + (1 + (1 / m)) * B

    // Approximate fraction of missing information.
    k = cols(T)
    gamma_hat = (1 + (1 / m)) * trace(B * invsym(T)) / k

    // Large sample degrees of freedom.
    nu_large = (m - 1) * gamma_hat ^ (-2)

    // Small sample correction.
    nu_small = J(1, k, 0)
    for (i = 1; i <= k; i++) {
      nu_obs = (nu[i] + 1) / (nu[i] + 3) * nu[i] * (1 - gamma_hat)
      nu_small[i] = ((1 / nu_large) + (1 / nu_obs)) ^ (-1)
    }

    // Confidence interval.
    se = sqrt(diagonal(T))
    crit = invt(nu_small', 0.975)
    ci = (qbar - se :* crit, qbar + se :* crit)

    // Post estimation results.
    st_matrix(st_local("bout"), qbar')
    st_matrix(st_local("Vout"), T)
    st_matrix(st_local("Wout"), Ubar)
    st_matrix(st_local("Bout"), B)
    st_matrix(st_local("dfout"), nu_small)
    st_matrix(st_local("ciout"), ci')
    st_numscalar(st_local("Nout"), rows(S0))

    // Set coefficient labels.
    coefnames = tokens(st_local("Xvar")), "_cons"
    coefnames = J(cols(coefnames), 1, ""), coefnames'
    vcovs = (st_local("Vout"), st_local("Wout"), st_local("Bout"))
    for (i = 1; i <= cols(vcovs); i++) {
      st_matrixrowstripe(vcovs[i], coefnames)
      st_matrixcolstripe(vcovs[i], coefnames)
    }
    st_matrixcolstripe(st_local("bout"), coefnames)
    st_matrixcolstripe(st_local("dfout"), coefnames)
  }

  void __ivreg(
    string scalar est,
    real colvector y,
    real matrix X,
    real matrix Z,
    real matrix J,
    real rowvector b,
    real matrix V
  ) {

    // Perform within transformation but keep the constant.
    Q = I(rows(J)) - J * invsym(cross(J, J)) * J'
    y = Q * y
    X = Q * X[., range(1, cols(X) - 1, 1)], X[, cols(X)]
    Z = Q * Z[., range(1, cols(Z) - 1, 1)], X[, cols(Z)]

    // Balance the panel.
    j = colsum(J)
    t = max(j)
    n = cols(J)
    for (i = 1; i <= n; i++) {
      a = t - j[i]
      if (a > 0) {
        y = y \ J(a, 1, 0)
        X = X \ J(a, cols(X), 0)
        Z = Z \ J(a, cols(Z), 0)
        J = J \ J(a, 1, e(i, n))
      }
    }

    // Estimate.
    if (est == "2sls") {
      __mi2sls(b, V, y, X, Z)
    }
    else if (est == "gmmopt") {
      __migmmopt(b, V, y, X, Z)
    }
    else if (est == "gmm3sls") {
      __migmm3sls(b, V, y, X, Z, J)
    }
    else if (est == "giv") {
      __migiv(b, V, y, X, Z, J)
    }
    else {
      error(198)
    }
  }

  void __mi2sls(
    real matrix b,
    real matrix V,
    real colvector y,
    real matrix X,
    real matrix Z
  ) {

    XZ = cross(X, Z)
    ZZi = invsym(cross(Z, Z))
    Zy = cross(Z, y)

    Ai = qrinv(XZ * ZZi * XZ')
    b = Ai * (XZ * ZZi * Zy)

    e = y - X * b
    B = XZ * ZZi * __opaccum(Z, e, J) * ZZi * XZ'
    V = Ai * B * Ai
  }

  void __migmmopt(
    real matrix b,
    real matrix V,
    real colvector y,
    real matrix X,
    real matrix Z
  ) {

    __mi2sls(b, V, y, X, Z)
    u_2sls = y - X * b

    XZ = cross(X, Z)
    Zy = cross(Z, y)

    W = qrinv(__opaccum(Z, u_2sls, J))
    V = qrinv(XZ * W * XZ')
    b = V * (XZ * W * Zy)
  }

  void __migmm3sls(
    real matrix b,
    real matrix V,
    real colvector y,
    real matrix X,
    real matrix Z,
    real matrix J
  ) {

    __mi2sls(b, V, y, X, Z)
    u_2sls = y - X * b

    n = cols(J)
    t = max(colsum(J))

    Omega = __opaccum(J(n, 1, I(t)), u_2sls, J)
    Omegai = qrinv(Omega)

    XZ = cross(X, Z)
    Zy = cross(Z, y)

    W = qrinv(__glsaccum(Z, Omega, J) / n)
    V = qrinv(XZ * W * XZ')
    b = V * (XZ * W * Zy)
  }

  void __migiv(
    real matrix b,
    real matrix V,
    real colvector y,
    real matrix X,
    real matrix Z,
    real matrix J
  ) {

    __mi2sls(b, V, y, X, Z)
    u_2sls = y - X * b

    n = cols(J)
    t = max(colsum(J))

    Omega = __opaccum(J(n, 1, I(t)), u_2sls, J)
    Omegai = qrinv(Omega)

    IO = I(n) # Omegai
    XOZ = X' * IO * Z
    ZOZi = qrinv(Z' * IO * Z)
    ZOy = Z' * IO * y

    Ai = qrinv(XOZ * ZOZi * XOZ')
    b = Ai * (XOZ * ZOZi * ZOy)

    u = y - X * b
    B = XOZ * ZOZi * __opaccum(IO * Z, u, J) * ZOZi * XOZ'
    V = Ai * B * Ai
  }

  real matrix __glsaccum(real matrix X, real matrix B, real matrix J) {
    idx = range(1, rows(X), 1)
    A = J(cols(X), cols(X), 0)
    for (j = 1; j <= cols(J); j++) {
      i = select(idx, J[, j])
      A = A + X[i, ]' * B * X[i, ]
    }
    return(A)
  }

  real matrix __opaccum(real matrix X, real colvector e, real matrix J) {
    idx = range(1, rows(X), 1)
    A = J(cols(X), cols(X), 0)
    for (j = 1; j <= cols(J); j++) {
      i = select(idx, J[, j])
      A = A + X[i, ]' * e[i] * e[i]' * X[i, ]
    }
    return(A)
  }

end
