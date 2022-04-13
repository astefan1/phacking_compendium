context('Simulation: P-Hacking Works')

test_that("Selective Reporting of DV works", {

  set.seed(2345)
  phack.ambitious <- sim.multDVhack(nobs.group = c(30, 30), nvar = 5, r = 0.3,
                                    strategy = "smallest.sig", iter = 100,
                                    alternative = "two.sided", alpha = 0.05)
  set.seed(2345)
  phack.normal <- sim.multDVhack(nobs.group = c(30, 30), nvar = 5, r = 0.3,
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
                                     strategy = "smallest", iter = 100,
                                     alternative = "two.sided", alpha = 0.05)
  set.seed(2345)
  phack.normal2 <- sim.multDVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3,
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
                                    strategy = "smallest.sig", iter = 100,
                                    alternative = "two.sided", alpha = 0.05)
  set.seed(1234)
  phack.normal <- sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3,
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
                                     strategy = "smallest", iter = 100,
                                     alternative = "two.sided", alpha = 0.05)
  set.seed(2345)
  phack.normal2 <- sim.multIVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3,
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
                          alpha = 0.05)
  set.seed(1234)
  phack2 <- sim.roundhack(0.06, iter = 100, alternative = "two.sided",
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
                          alternative = "two.sided", iter = 100, alpha = 0.05)
  set.seed(1234)
  optstop2 <- sim.optstop(n.min = 20, n.max = 50, step = 5,
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
                          strategy = "firstsig", alpha = 0.05, iter = 100)
  set.seed(1234)
  outexcl2 <- sim.outHack(nobs = 20, which = "random",
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
                          interactions = FALSE, strategy = "firstsig",
                          alpha = 0.05, iter = 100)
  set.seed(1234)
  covhack2 <- sim.covhack(nobs.group = 20, ncov = 3, rcov = 0.1, rcovdv = 0.6,
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
                                 alternative = "two.sided",
                                 strategy = "firstsig", alpha = 0.05,
                                 iter = 100)
  set.seed(1234)
  subgrhack2 <- sim.subgroupHack(nobs.group = 30, nsubvars = 3,
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

test_that("Scale Redefinition works", {

  set.seed(1234)
  scaledef1 <- sim.compscoreHack(nobs = 30, ncompv = 10, rcomp = 0.5,
                                 ndelete = 5, strategy = "firstsig",
                                 alpha = 0.05, iter = 100)
  set.seed(1234)
  scaledef2 <- sim.compscoreHack(nobs = 30, ncompv = 10, rcomp = 0.5,
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
  arbitCutoff1 <- sim.cutoffHack(nobs = 30, strategy = "firstsig", alpha = 0.05,
                                 iter = 100)
  set.seed(1234)
  arbitCutoff2 <- sim.cutoffHack(nobs = 30, strategy = "smallest.sig",
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
                                      alternative = "two.sided", alpha = 0.05,
                                      iter = 100)
  set.seed(1234)
  arbitStats2 <- sim.statAnalysisHack(nobs = 30, strategy = "smallest.sig",
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
                            strategy = "firstsig", alpha = 0.05, iter = 100)
  set.seed(1234)
  varT2 <- sim.varTransHack(nobs = 30, transvar = "xy",
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
                         strategy = "firstsig", alpha = 0.05, iter = 100)
  set.seed(1234)
  misval2 <- sim.impHack(nobs = 30, missing = 0.1, which = "random",
                         strategy = "smallest.sig", alpha = 0.05, iter = 100)

  expect_equal(nrow(misval1), 100)
  expect_equal(misval1[,2], misval2[,2])
  expect_equal(length(which(misval1[,1] <= 0.05)),
               length(which(misval2[,1] <= 0.05)))
  expect_gt(length(which(misval1[,1] <= 0.05)),
            length(which(misval1[,2] <= 0.05)))

})
