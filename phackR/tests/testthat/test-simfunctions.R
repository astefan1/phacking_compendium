context('Simulation: P-Hacking Works')

test_that("Selective Reporting of DV works", {

  set.seed(2345)
  phack.ambitious <- sim.multDVhack(nobs.group = c(30, 30), nvar = 5, r = 0.3,
                                    effect = 0, heterogeneity = 0,
                                    strategy = "smallest.sig", iter = 100,
                                    alternative = "two.sided", alpha = 0.05)
  set.seed(2345)
  phack.normal <- sim.multDVhack(nobs.group = c(30, 30), nvar = 5, r = 0.3,
                                 effect = 0, heterogeneity = 0,
                                 strategy = "firstsig", iter = 100,
                                 alternative = "two.sided", alpha = 0.05)

  expect_equal(nrow(phack.ambitious), 100)
  expect_equal(phack.ambitious[ ,2], phack.normal[ ,2])
  expect_equal(length(which(phack.ambitious[ ,1] < 0.05)),
               length(which(phack.normal[ ,1] < 0.05)))
  expect_gt(length(which(phack.ambitious[ ,1] < 0.05)),
            length(which(phack.ambitious[ ,2] < 0.05)))
  expect_gt(length(which(phack.normal[ ,1] < 0.05)),
            length(which(phack.normal[ ,2] < 0.05)))

  set.seed(2345)
  phack.ambitious2 <- sim.multDVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3,
                                     effect = 0, heterogeneity = 0,
                                     strategy = "smallest", iter = 100,
                                     alternative = "two.sided", alpha = 0.05)
  set.seed(2345)
  phack.normal2 <- sim.multDVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3,
                                  effect = 0, heterogeneity = 0,
                                  strategy = "firstsig", iter = 100,
                                  alternative = "two.sided", alpha = 0.05)
  expect_gt(length(which(phack.ambitious2[ ,1] < 0.05)),
            length(which(phack.ambitious[ ,1] < 0.05)))
  expect_gt(length(which(phack.normal2[ ,1] < 0.05)),
            length(which(phack.normal[ ,1] < 0.05)))
})

test_that("Selective Reporting of IV works", {

  set.seed(1234)
  phack.ambitious <- sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3,
                                    effect = 0, heterogeneity = 0,
                                    strategy = "smallest.sig", iter = 100,
                                    alternative = "two.sided", alpha = 0.05)
  set.seed(1234)
  phack.normal <- sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3,
                                 effect = 0, heterogeneity = 0,
                                 strategy = "firstsig", iter = 100,
                                 alternative = "two.sided", alpha = 0.05)

  expect_equal(nrow(phack.ambitious), 100)
  expect_equal(phack.ambitious[ ,2], phack.normal[ ,2])
  expect_equal(length(which(phack.ambitious[ ,1] < 0.05)),
               length(which(phack.normal[ ,1] < 0.05)))
  expect_gt(length(which(phack.ambitious[ ,1] < 0.05)),
            length(which(phack.ambitious[ ,2] < 0.05)))
  expect_gt(length(which(phack.normal[ ,1] < 0.05)),
            length(which(phack.normal[ ,2] < 0.05)))

  set.seed(2345)
  phack.ambitious2 <- sim.multIVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3,
                                     effect = 0, heterogeneity = 0,
                                     strategy = "smallest", iter = 100,
                                     alternative = "two.sided", alpha = 0.05)
  set.seed(2345)
  phack.normal2 <- sim.multIVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3,
                                  effect = 0, heterogeneity = 0,
                                  strategy = "firstsig", iter = 100,
                                  alternative = "two.sided", alpha = 0.05)
  expect_gt(length(which(phack.ambitious2[,1] < 0.05)),
            length(which(phack.ambitious[,1] < 0.05)))
  expect_gt(length(which(phack.normal2[,1] < 0.05)),
            length(which(phack.normal[,1] < 0.05)))

})

test_that("Incorrect Rounding works", {

  set.seed(1234)
  phack1 <- sim.roundhack(0.1, iter = 100, alternative = "two.sided",
                          effect = 0, heterogeneity = 0,
                          alpha = 0.05)
  set.seed(1234)
  phack2 <- sim.roundhack(0.06, iter = 100, alternative = "two.sided",
                          effect = 0, heterogeneity = 0,
                          alpha = 0.05)

  expect_equal(nrow(phack1), 100)
  expect_equal(phack1[,2], phack2[,2])
  expect_gt(length(which(phack1[,1] <= 0.05)),
            length(which(phack1[,2] <= 0.05)))
  expect_gt(length(which(phack1[,1] <= 0.05)),
            length(which(phack2[,1] <= 0.05)))

  })

test_that("Optional Stopping works", {

  set.seed(1234)
  optstop1 <- sim.optstop(n.min = 10, n.max = 50, step = 5,
                          effect = 0, heterogeneity = 0,
                          alternative = "two.sided", iter = 100, alpha = 0.05)
  set.seed(1234)
  optstop2 <- sim.optstop(n.min = 20, n.max = 50, step = 5,
                          effect = 0, heterogeneity = 0,
                          alternative = "two.sided", iter = 100, alpha = 0.05)

  expect_equal(nrow(optstop1), 100)
  expect_equal(optstop1[,2], optstop2[,2])
  expect_gt(length(which(optstop1[,1] <= 0.05)),
            length(which(optstop2[,1] <= 0.05)))
  expect_gt(length(which(optstop1[,1] <= 0.05)),
            length(which(optstop1[,2] <= 0.05)))

})

test_that("Outlier Exclusion works", {

  set.seed(1234)
  outexcl1 <- sim.outHack(nobs = 20, which = "random",
                          effect = 0, heterogeneity = 0,
                          strategy = "firstsig", alpha = 0.05, iter = 100)
  set.seed(1234)
  outexcl2 <- sim.outHack(nobs = 20, which = "random",
                          effect = 0, heterogeneity = 0,
                          strategy = "smallest.sig", alpha = 0.05, iter = 100)

  expect_equal(nrow(outexcl1), 100)
  expect_equal(outexcl1[ ,2], outexcl2[ ,2])
  expect_equal(length(which(outexcl1[ ,1] <= 0.05)),
               length(which(outexcl2[ ,1] <= 0.05)))
  expect_gt(length(which(outexcl1[ ,1] <= 0.05)),
            length(which(outexcl1[ ,2] <= 0.05)))

})

test_that("Exploiting Covariates works", {

  set.seed(1234)
  covhack1 <- sim.covhack(nobs.group = 20, ncov = 3, rcov = 0.1, rcovdv = 0.6,
                          effect = 0, heterogeneity = 0,
                          interactions = FALSE, strategy = "firstsig",
                          alpha = 0.05, iter = 100)
  set.seed(1234)
  covhack2 <- sim.covhack(nobs.group = 20, ncov = 3, rcov = 0.1, rcovdv = 0.6,
                          effect = 0, heterogeneity = 0,
                          interactions = FALSE, strategy = "smallest.sig",
                          alpha = 0.05, iter = 100)

  expect_equal(nrow(covhack1), 100)
  expect_equal(covhack1[,2], covhack2[,2])
  expect_equal(length(which(covhack1[,1] <= 0.05)),
               length(which(covhack2[,1] <= 0.05)))
  expect_gt(length(which(covhack1[,1] <= 0.05)),
            length(which(covhack1[,2] <= 0.05)))

})

test_that("Subgroup Analyses work", {

  set.seed(1234)
  subgrhack1 <- sim.subgroupHack(nobs.group = 30, nsubvars = 3,
                                 effect = 0, heterogeneity = 0,
                                 alternative = "two.sided",
                                 strategy = "firstsig", alpha = 0.05,
                                 iter = 100)
  set.seed(1234)
  subgrhack2 <- sim.subgroupHack(nobs.group = 30, nsubvars = 3,
                                 effect = 0, heterogeneity = 0,
                                 alternative = "two.sided",
                                 strategy = "smallest.sig", alpha = 0.05,
                                 iter = 100)

  expect_equal(nrow(subgrhack1), 100)
  expect_equal(subgrhack1[,2], subgrhack2[,2])
  expect_equal(length(which(subgrhack1[,1] <= 0.05)),
               length(which(subgrhack2[,1] <= 0.05)))
  expect_gt(length(which(subgrhack1[,1] <= 0.05)),
            length(which(subgrhack1[,2] <= 0.05)))

})

test_that("Selective Reporting of IV regression works", {

  set.seed(1234)
  reghack1 <- sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3,
                             regression = TRUE, effect = 0, heterogeneity = 0,
                             strategy = "firstsig", iter = 100,
                             alternative = "two.sided", alpha = 0.05)
  set.seed(1234)
  reghack2 <- sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3,
                             regression = TRUE, effect = 0, heterogeneity = 0,
                             strategy = "smallest.sig", iter = 100,
                             alternative = "two.sided", alpha = 0.05)

  expect_equal(nrow(reghack1), 100)
  expect_equal(reghack1[,2], reghack2[,2])
  expect_equal(length(which(reghack1[,1] < 0.05)),
               length(which(reghack2[,1] < 0.05)))
  expect_gt(length(which(reghack1[,1] < 0.05)),
            length(which(reghack1[,2] < 0.05)))

})

test_that("Group-comparison reporting columns are populated", {

  phase1_cols <- c("effect.initial", "effect.final", "se.initial", "se.final",
                   "n.initial", "n.final", "stat.initial", "stat.final",
                   "p.initial", "p.final", "method.initial", "method.final")

  set.seed(2024)
  multdv <- sim.multDVhack(nobs.group = c(30, 30), nvar = 4, r = 0.3,
                           effect = 0, heterogeneity = 0, strategy = "firstsig",
                           iter = 20, alternative = "two.sided", alpha = 0.05)
  set.seed(2024)
  statanalysis <- sim.statAnalysisHack(nobs.group = c(30, 30),
                                       effect = 0, heterogeneity = 0,
                                       strategy = "firstsig",
                                       alternative = "two.sided",
                                       alpha = 0.05, iter = 20)

  expect_true(all(phase1_cols %in% names(multdv)))
  expect_true(all(phase1_cols %in% names(statanalysis)))
  expect_equal(multdv$p.initial, multdv$ps.orig)
  expect_equal(multdv$p.final, multdv$ps.hack)
  expect_equal(statanalysis$p.initial, statanalysis$ps.orig)
  expect_equal(statanalysis$p.final, statanalysis$ps.hack)
  expect_true(all(is.finite(multdv$effect.initial)))
  expect_true(all(is.finite(multdv$effect.final)))
  expect_true(all(is.finite(multdv$se.initial)))
  expect_true(all(is.finite(multdv$se.final)))
  expect_true(all(multdv$method.initial == "t.equal"))
  expect_true(all(nchar(multdv$method.final) > 0))
  expect_true(all(is.finite(statanalysis$effect.initial)))
  expect_true(all(is.finite(statanalysis$effect.final)))
  expect_true(all(nchar(statanalysis$method.initial) > 0))
  expect_true(all(nchar(statanalysis$method.final) > 0))

})

test_that("Group-comparison heterogeneity remains reproducible and non-degenerate", {

  set.seed(2025)
  hetero1 <- sim.multDVhack(nobs.group = c(30, 30), nvar = 4, r = 0.3,
                            effect = 0, heterogeneity = 0.5,
                            strategy = "firstsig", iter = 20,
                            alternative = "two.sided", alpha = 0.05)
  set.seed(2025)
  hetero2 <- sim.multDVhack(nobs.group = c(30, 30), nvar = 4, r = 0.3,
                            effect = 0, heterogeneity = 0.5,
                            strategy = "firstsig", iter = 20,
                            alternative = "two.sided", alpha = 0.05)

  expect_equal(hetero1, hetero2)
  expect_true(all(is.finite(hetero1$effect.initial)))
  expect_true(all(is.finite(hetero1$effect.final)))
  expect_gt(sd(hetero1$effect.initial), 0)
  expect_gt(length(unique(round(hetero1$effect.initial, 8))), 1)

})

test_that("Regression and ANCOVA reporting columns are populated", {

  phase2_cols <- c("effect.initial", "effect.final", "se.initial", "se.final",
                   "n.initial", "n.final", "stat.initial", "stat.final",
                   "p.initial", "p.final", "method.initial", "method.final")

  set.seed(2028)
  multiv_reg_null <- sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3,
                                    effect = 0, heterogeneity = 0,
                                    regression = TRUE, strategy = "firstsig",
                                    iter = 200, alternative = "two.sided",
                                    alpha = 0.05)
  set.seed(2028)
  multiv_reg_effect1 <- sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3,
                                       effect = 0.4, heterogeneity = 0,
                                       regression = TRUE, strategy = "firstsig",
                                       iter = 200, alternative = "two.sided",
                                       alpha = 0.05)
  set.seed(2028)
  multiv_reg_effect2 <- sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3,
                                       effect = 0.4, heterogeneity = 0,
                                       regression = TRUE, strategy = "firstsig",
                                       iter = 200, alternative = "two.sided",
                                       alpha = 0.05)

  expect_equal(multiv_reg_effect1[, 2], multiv_reg_effect2[, 2])
  expect_true(all(phase2_cols %in% names(multiv_reg_effect1)))
  expect_equal(multiv_reg_effect1$p.initial, multiv_reg_effect1$ps.orig)
  expect_equal(multiv_reg_effect1$p.final, multiv_reg_effect1$ps.hack)
  expect_true(all(is.finite(multiv_reg_effect1$effect.initial)))
  expect_true(all(is.finite(multiv_reg_effect1$effect.final)))
  expect_true(all(is.finite(multiv_reg_effect1$se.initial)))
  expect_true(all(is.finite(multiv_reg_effect1$se.final)))
  expect_true(all(nchar(multiv_reg_effect1$method.initial) > 0))
  expect_true(all(nchar(multiv_reg_effect1$method.final) > 0))
  expect_gt(sum(multiv_reg_effect1$ps.orig < 0.05),
            sum(multiv_reg_null$ps.orig < 0.05))

  set.seed(2029)
  covhack_hetero1 <- sim.covhack(nobs.group = 20, ncov = 3, rcov = 0.1,
                                 rcovdv = 0.6, interactions = FALSE,
                                 strategy = "firstsig", effect = 0,
                                 heterogeneity = 0.4, alpha = 0.05, iter = 20)
  set.seed(2029)
  covhack_hetero2 <- sim.covhack(nobs.group = 20, ncov = 3, rcov = 0.1,
                                 rcovdv = 0.6, interactions = FALSE,
                                 strategy = "firstsig", effect = 0,
                                 heterogeneity = 0.4, alpha = 0.05, iter = 20)

  expect_equal(covhack_hetero1, covhack_hetero2)
  expect_true(all(phase2_cols %in% names(covhack_hetero1)))
  expect_equal(covhack_hetero1$p.initial, covhack_hetero1$ps.orig)
  expect_equal(covhack_hetero1$p.final, covhack_hetero1$ps.hack)
  expect_true(all(is.finite(covhack_hetero1$effect.initial)))
  expect_true(all(is.finite(covhack_hetero1$effect.final)))
  expect_gt(sd(covhack_hetero1$effect.initial), 0)
  expect_gt(length(unique(round(covhack_hetero1$effect.initial, 8))), 1)
  expect_true(all(nchar(covhack_hetero1$method.initial) > 0))
  expect_true(all(nchar(covhack_hetero1$method.final) > 0))

})

test_that("Group-comparison non-null effects increase original rejection rates", {

  set.seed(2026)
  nullres <- sim.multDVhack(nobs.group = c(30, 30), nvar = 4, r = 0.3,
                            effect = 0, heterogeneity = 0,
                            strategy = "firstsig", iter = 200,
                            alternative = "two.sided", alpha = 0.05)
  set.seed(2026)
  effectres <- sim.multDVhack(nobs.group = c(30, 30), nvar = 4, r = 0.3,
                              effect = 0.5, heterogeneity = 0,
                              strategy = "firstsig", iter = 200,
                              alternative = "two.sided", alpha = 0.05)

  expect_gt(sum(effectres$ps.orig < 0.05), sum(nullres$ps.orig < 0.05))

})

test_that("Optional stopping sample sizes reflect the selected analysis", {

  set.seed(2027)
  optstop <- sim.optstop(n.min = 10, n.max = 30, step = 5,
                         effect = 0.8, heterogeneity = 0,
                         alternative = "two.sided", iter = 50, alpha = 0.05)

  expect_true(all(optstop$n.initial == 60))
  expect_true(all(optstop$n.final <= optstop$n.initial))
  expect_true(any(optstop$n.final < optstop$n.initial))

})

test_that("Correlation-family reporting columns are populated", {

  phase2_cols <- c("effect.initial", "effect.final", "se.initial", "se.final",
                   "n.initial", "n.final", "stat.initial", "stat.final",
                   "p.initial", "p.final", "method.initial", "method.final")

  set.seed(2028)
  cutoff <- sim.cutoffHack(nobs = 30, strategy = "firstsig",
                           effect = 0, heterogeneity = 0,
                           alpha = 0.05, iter = 20)
  set.seed(2028)
  covhack <- sim.covhack(nobs.group = 30, ncov = 3, rcov = 0.2, rcovdv = 0.5,
                         effect = 0, heterogeneity = 0,
                         interactions = FALSE, strategy = "firstsig",
                         alpha = 0.05, iter = 20)
  set.seed(2028)
  regiv <- sim.multIVhack(nobs.group = 30, nvar = 4, r = 0.2,
                          regression = TRUE, effect = 0, heterogeneity = 0,
                          strategy = "firstsig", alpha = 0.05, iter = 20)

  expect_true(all(phase2_cols %in% names(cutoff)))
  expect_true(all(phase2_cols %in% names(covhack)))
  expect_true(all(phase2_cols %in% names(regiv)))
  expect_equal(cutoff$p.initial, cutoff$ps.orig)
  expect_equal(cutoff$p.final, cutoff$ps.hack)
  expect_equal(covhack$p.initial, covhack$ps.orig)
  expect_equal(covhack$p.final, covhack$ps.hack)
  expect_equal(regiv$p.initial, regiv$ps.orig)
  expect_equal(regiv$p.final, regiv$ps.hack)
  expect_true(all(is.finite(cutoff$effect.initial)))
  expect_true(all(is.finite(cutoff$effect.final)))
  expect_true(all(is.finite(covhack$effect.initial)))
  expect_true(all(is.finite(covhack$effect.final)))
  expect_true(all(is.finite(regiv$effect.initial)))
  expect_true(all(is.finite(regiv$effect.final)))
  expect_true(all(nchar(cutoff$method.final) > 0))
  expect_true(all(nchar(covhack$method.final) > 0))
  expect_true(all(nchar(regiv$method.final) > 0))

})

test_that("Correlation-family heterogeneity remains reproducible and non-degenerate", {

  set.seed(2029)
  hetero1 <- sim.multIVhack(nobs.group = 30, nvar = 4, r = 0.2,
                            regression = TRUE, effect = 0,
                            heterogeneity = 0.5, strategy = "firstsig",
                            alpha = 0.05, iter = 20)
  set.seed(2029)
  hetero2 <- sim.multIVhack(nobs.group = 30, nvar = 4, r = 0.2,
                            regression = TRUE, effect = 0,
                            heterogeneity = 0.5, strategy = "firstsig",
                            alpha = 0.05, iter = 20)

  expect_equal(hetero1, hetero2)
  expect_true(all(is.finite(hetero1$effect.initial)))
  expect_true(all(is.finite(hetero1$effect.final)))
  expect_gt(sd(hetero1$effect.initial), 0)
  expect_gt(length(unique(round(hetero1$effect.initial, 8))), 1)

})

test_that("Empirical data generators match requested initial effects", {

  tol <- 1e-10

  set.seed(2040)
  smd <- phackR:::.sim.data(nobs.group = c(30, 30), effect = 0.5,
                            heterogeneity = 10, empirical = TRUE)
  expect_equal(
    phackR:::.report.twogroup(control = smd[smd[,1] == 1, 2],
                              treatment = smd[smd[,1] == 2, 2])$effect,
    0.5,
    tolerance = tol
  )

  set.seed(2041)
  assoc <- phackR:::.sim.association(nobs = 40, effect = 0.4,
                                     heterogeneity = 10, empirical = TRUE)
  expect_equal(
    phackR:::.report.association(x = assoc[,1], y = assoc[,2])$effect,
    0.4,
    tolerance = tol
  )

  set.seed(2042)
  multdv <- phackR:::.sim.multDV(nobs.group = c(30, 30), nvar = 4, r = 0.3,
                                 effect = 0.5, heterogeneity = 10,
                                 empirical = TRUE)
  expect_equal(
    phackR:::.report.twogroup(control = multdv[multdv[,1] == 1, 2],
                              treatment = multdv[multdv[,1] == 2, 2])$effect,
    0.5,
    tolerance = tol
  )

  set.seed(2043)
  multiv <- phackR:::.sim.multIV(nobs.group = c(30, 30), nvar = 4, r = 0.3,
                                 effect = 0.5, heterogeneity = 10,
                                 empirical = TRUE)
  expect_equal(
    phackR:::.report.twogroup(control = multiv[,1],
                              treatment = multiv[,2])$effect,
    0.5,
    tolerance = tol
  )

  set.seed(2044)
  multiv.reg <- phackR:::.sim.multIV(nobs.group = 40, nvar = 4, r = 0.2,
                                     regression = TRUE, effect = 0.4,
                                     heterogeneity = 10, empirical = TRUE)
  expect_equal(
    phackR:::.report.association(x = multiv.reg[,2],
                                 y = multiv.reg[,1])$effect,
    0.4,
    tolerance = tol
  )

  set.seed(2045)
  covdat <- phackR:::.sim.covariates(nobs.group = c(30, 30), ncov = 3,
                                     rcov = 0.2, rcovdv = 0.5,
                                     effect = 0.5, heterogeneity = 10,
                                     empirical = TRUE)
  expect_equal(
    phackR:::.report.twogroup(control = covdat[covdat[,1] == 1, 2],
                              treatment = covdat[covdat[,1] == 2, 2])$effect,
    0.5,
    tolerance = tol
  )

  set.seed(2046)
  compscore <- phackR:::.sim.compscore(nobs = 40, ncompv = 5, rcomp = 0.5,
                                       effect = 0.4, heterogeneity = 10,
                                       empirical = TRUE)
  expect_equal(
    phackR:::.report.association(x = rowMeans(compscore[, 2:6]),
                                 y = compscore[,1])$effect,
    0.4,
    tolerance = tol
  )

})

test_that("Empirical simulators ignore heterogeneity and expose exact initial effects", {

  empirical.checks <- list(
    sim.compscoreHack = list(
      effect = 0.4,
      call = function(heterogeneity) sim.compscoreHack(
        nobs = 40, ncompv = 5, rcomp = 0.5, ndelete = 1,
        effect = 0.4, heterogeneity = heterogeneity,
        empirical = TRUE, iter = 5)
    ),
    sim.covhack = list(
      effect = 0.5,
      call = function(heterogeneity) sim.covhack(
        nobs.group = 30, ncov = 3, rcov = 0.2, rcovdv = 0.5,
        effect = 0.5, heterogeneity = heterogeneity,
        empirical = TRUE, iter = 5)
    ),
    sim.cutoffHack = list(
      effect = 0.4,
      call = function(heterogeneity) sim.cutoffHack(
        nobs = 40, effect = 0.4, heterogeneity = heterogeneity,
        empirical = TRUE, iter = 5)
    ),
    sim.impHack = list(
      effect = 0.4,
      call = function(heterogeneity) sim.impHack(
        nobs = 50, missing = 0.1, which = 1,
        effect = 0.4, heterogeneity = heterogeneity,
        empirical = TRUE, iter = 5)
    ),
    sim.multDVhack = list(
      effect = 0.5,
      call = function(heterogeneity) sim.multDVhack(
        nobs.group = 30, nvar = 4, r = 0.3,
        effect = 0.5, heterogeneity = heterogeneity,
        empirical = TRUE, iter = 5)
    ),
    sim.multIVhack_ttest = list(
      effect = 0.5,
      call = function(heterogeneity) sim.multIVhack(
        nobs.group = 30, nvar = 4, r = 0.3,
        effect = 0.5, heterogeneity = heterogeneity,
        empirical = TRUE, iter = 5)
    ),
    sim.multIVhack_regression = list(
      effect = 0.4,
      call = function(heterogeneity) sim.multIVhack(
        nobs.group = 40, nvar = 4, r = 0.2, regression = TRUE,
        effect = 0.4, heterogeneity = heterogeneity,
        empirical = TRUE, iter = 5)
    ),
    sim.optstop = list(
      effect = 0.5,
      call = function(heterogeneity) sim.optstop(
        n.min = 10, n.max = 30, step = 5,
        effect = 0.5, heterogeneity = heterogeneity,
        empirical = TRUE, iter = 5)
    ),
    sim.outHack = list(
      effect = 0.4,
      call = function(heterogeneity) sim.outHack(
        nobs = 40, which = 1, effect = 0.4,
        heterogeneity = heterogeneity, empirical = TRUE, iter = 5)
    ),
    sim.roundhack = list(
      effect = 0.5,
      call = function(heterogeneity) sim.roundhack(
        roundinglevel = 0.06, effect = 0.5,
        heterogeneity = heterogeneity, empirical = TRUE, iter = 5)
    ),
    sim.statAnalysisHack = list(
      effect = 0.5,
      call = function(heterogeneity) sim.statAnalysisHack(
        nobs.group = 30, effect = 0.5,
        heterogeneity = heterogeneity, empirical = TRUE, iter = 5)
    ),
    sim.subgroupHack = list(
      effect = 0.5,
      call = function(heterogeneity) sim.subgroupHack(
        nobs.group = 30, nsubvars = 3, effect = 0.5,
        heterogeneity = heterogeneity, empirical = TRUE, iter = 5)
    ),
    sim.varTransHack = list(
      effect = 0.4,
      call = function(heterogeneity) sim.varTransHack(
        nobs = 40, transvar = "xy", effect = 0.4,
        heterogeneity = heterogeneity, empirical = TRUE, iter = 5)
    )
  )

  for(i in seq_along(empirical.checks)){
    spec <- empirical.checks[[i]]
    set.seed(2046 + i)
    hetero0 <- spec$call(0)
    set.seed(2046 + i)
    hetero10 <- spec$call(10)

    expect_equal(
      hetero10,
      hetero0,
      info = paste("heterogeneity ignored:", names(empirical.checks)[i])
    )
    expect_equal(
      hetero10$effect.initial,
      rep(spec$effect, 5),
      tolerance = 1e-10,
      info = paste("initial effect:", names(empirical.checks)[i])
    )
  }

})

test_that("Correlation-family non-null effects increase original rejection rates", {

  set.seed(2030)
  nullres <- sim.cutoffHack(nobs = 30, strategy = "firstsig",
                            effect = 0, heterogeneity = 0,
                            alpha = 0.05, iter = 200)
  set.seed(2030)
  effectres <- sim.cutoffHack(nobs = 30, strategy = "firstsig",
                              effect = 0.4, heterogeneity = 0,
                              alpha = 0.05, iter = 200)

  expect_gt(sum(effectres$ps.orig < 0.05), sum(nullres$ps.orig < 0.05))

})

test_that("One-sided group-comparison tests follow the positive-effect direction", {

  set.seed(2031)
  dat <- phackR:::.sim.data(nobs.group = 40, effect = 0.8, heterogeneity = 0)
  control <- dat[dat[,1] == 1, 2]
  treatment <- dat[dat[,1] == 2, 2]

  report.t.greater <- phackR:::.report.twogroup(control = control,
                                                treatment = treatment,
                                                method = "t.equal",
                                                alternative = "greater")
  report.t.less <- phackR:::.report.twogroup(control = control,
                                             treatment = treatment,
                                             method = "t.equal",
                                             alternative = "less")
  report.w.greater <- phackR:::.report.twogroup(control = control,
                                                treatment = treatment,
                                                method = "wilcox",
                                                alternative = "greater")
  report.y.greater <- phackR:::.report.twogroup(control = control,
                                                treatment = treatment,
                                                method = "yuen",
                                                trim = 0.2,
                                                alternative = "greater")
  report.y.less <- phackR:::.report.twogroup(control = control,
                                             treatment = treatment,
                                             method = "yuen",
                                             trim = 0.2,
                                             alternative = "less")

  expect_gt(report.t.greater$stat, 0)
  expect_gt(report.t.greater$effect, 0)
  expect_lt(report.t.greater$p, report.t.less$p)
  expect_lt(report.w.greater$p, 0.05)
  expect_lt(report.y.greater$p, report.y.less$p)

})

test_that("One-sided simulators reward positive effects with alternative greater", {

  set.seed(2032)
  greater <- sim.multDVhack(nobs.group = 30, nvar = 4, r = 0.3,
                            effect = 0.8, heterogeneity = 0,
                            strategy = "firstsig", iter = 200,
                            alternative = "greater", alpha = 0.05)
  set.seed(2032)
  less <- sim.multDVhack(nobs.group = 30, nvar = 4, r = 0.3,
                         effect = 0.8, heterogeneity = 0,
                         strategy = "firstsig", iter = 200,
                         alternative = "less", alpha = 0.05)

  expect_gt(sum(greater$ps.orig < 0.05), sum(less$ps.orig < 0.05))
  expect_gt(mean(greater$effect.initial), 0)
  expect_gt(mean(greater$ds.orig), 0)

})

test_that("One-sided regression IV tests follow positive associations", {

  set.seed(2033)
  greater <- sim.multIVhack(nobs.group = 40, nvar = 4, r = 0.2,
                            regression = TRUE, effect = 0.5,
                            heterogeneity = 0, strategy = "firstsig",
                            alternative = "greater", alpha = 0.05, iter = 200)
  set.seed(2033)
  less <- sim.multIVhack(nobs.group = 40, nvar = 4, r = 0.2,
                         regression = TRUE, effect = 0.5,
                         heterogeneity = 0, strategy = "firstsig",
                         alternative = "less", alpha = 0.05, iter = 200)

  expect_gt(sum(greater$ps.orig < 0.05), sum(less$ps.orig < 0.05))
  expect_gt(mean(greater$effect.initial), 0)

})

test_that("Scale Redefinition works", {

  set.seed(1234)
  scaledef1 <- sim.compscoreHack(nobs = 30, ncompv = 10, rcomp = 0.5,
                                 effect = 0, heterogeneity = 0,
                                 ndelete = 5, strategy = "firstsig",
                                 alpha = 0.05, iter = 100)
  set.seed(1234)
  scaledef2 <- sim.compscoreHack(nobs = 30, ncompv = 10, rcomp = 0.5,
                                 effect = 0, heterogeneity = 0,
                                 ndelete = 5, strategy = "smallest.sig",
                                 alpha = 0.05, iter = 100)

  expect_equal(nrow(scaledef1), 100)
  expect_equal(scaledef1[,2], scaledef2[,2])
  expect_equal(length(which(scaledef1[,1] <= 0.05)),
               length(which(scaledef2[,1] <= 0.05)))
  expect_gt(length(which(scaledef1[,1] <= 0.05)),
            length(which(scaledef1[,2] <= 0.05)))

})

test_that("Exploiting arbitraray cutoffs works", {

  set.seed(1234)
  arbitCutoff1 <- sim.cutoffHack(nobs = 30, strategy = "firstsig",
                                 effect = 0, heterogeneity = 0, alpha = 0.05,
                                 iter = 100)
  set.seed(1234)
  arbitCutoff2 <- sim.cutoffHack(nobs = 30, strategy = "smallest.sig",
                                 effect = 0, heterogeneity = 0,
                                 alpha = 0.05, iter = 100)

  expect_equal(nrow(arbitCutoff1), 100)
  expect_equal(arbitCutoff1[,2], arbitCutoff2[,2])
  expect_equal(length(which(arbitCutoff1[,1] <= 0.05)),
               length(which(arbitCutoff2[,1] <= 0.05)))
  expect_gt(length(which(arbitCutoff1[,1] <= 0.05)),
            length(which(arbitCutoff1[,2] <= 0.05)))

})

test_that("Exploiting statistical analysis options works", {

  set.seed(1234)
  arbitStats1 <- sim.statAnalysisHack(nobs.group = 30, strategy = "firstsig",
                                      effect = 0, heterogeneity = 0,
                                      alternative = "two.sided", alpha = 0.05,
                                      iter = 100)
  set.seed(1234)
  arbitStats2 <- sim.statAnalysisHack(nobs = 30, strategy = "smallest.sig",
                                      effect = 0, heterogeneity = 0,
                                      alternative = "two.sided", alpha = 0.05,
                                      iter = 100)

  expect_equal(nrow(arbitStats1), 100)
  expect_equal(arbitStats1[,2], arbitStats2[,2])
  expect_equal(length(which(arbitStats1[,1] <= 0.05)),
               length(which(arbitStats2[,1] <= 0.05)))
  expect_gt(length(which(arbitStats1[,1] <= 0.05)),
            length(which(arbitStats1[,2] <= 0.05)))

})

test_that("Exploiting variable transformations works", {

  set.seed(1234)
  varT1 <- sim.varTransHack(nobs = 30, transvar = "xy",
                            effect = 0, heterogeneity = 0,
                            strategy = "firstsig", alpha = 0.05, iter = 100)
  set.seed(1234)
  varT2 <- sim.varTransHack(nobs = 30, transvar = "xy",
                            effect = 0, heterogeneity = 0,
                            strategy = "smallest.sig", alpha = 0.05, iter = 100)

  expect_equal(nrow(varT1), 100)
  expect_equal(varT1[,2], varT2[,2])
  expect_equal(length(which(varT1[,1] <= 0.05)),
               length(which(varT2[,1] <= 0.05)))
  expect_gt(length(which(varT1[,1] <= 0.05)),
            length(which(varT1[,2] <= 0.05)))

})

test_that("Exploiting missing value imputation works", {

  set.seed(1234)
  misval1 <- sim.impHack(nobs = 30, missing = 0.1, which = "random",
                         effect = 0, heterogeneity = 0,
                         strategy = "firstsig", alpha = 0.05, iter = 100)
  set.seed(1234)
  misval2 <- sim.impHack(nobs = 30, missing = 0.1, which = "random",
                         effect = 0, heterogeneity = 0,
                         strategy = "smallest.sig", alpha = 0.05, iter = 100)

  expect_equal(nrow(misval1), 100)
  expect_equal(misval1[,2], misval2[,2])
  expect_equal(length(which(misval1[,1] <= 0.05)),
               length(which(misval2[,1] <= 0.05)))
  expect_gt(length(which(misval1[,1] <= 0.05)),
            length(which(misval1[,2] <= 0.05)))

})
