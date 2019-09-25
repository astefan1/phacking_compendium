context('Simulation: P-Hacking Works')

test_that("Selective Reporting of DV works", {

  phack.ambitious <- sim.multDVhack(nobs.group = c(30, 30), nvar = 5, r = 0.3, ambitious = TRUE, iter = 500, alternative = "two.sided", alpha = 0.05, seed = 2345)
  phack.normal <- sim.multDVhack(nobs.group = c(30, 30), nvar = 5, r = 0.3, ambitious = FALSE, iter = 500, alternative = "two.sided", alpha = 0.05, seed = 2345)

  expect_equal(nrow(phack.ambitious), 500)
  expect_equal(phack.ambitious[,2], phack.normal[,2])
  expect_equal(length(which(phack.ambitious[,1] < 0.05)), length(which(phack.normal[,1] < 0.05)))
  expect_gt(length(which(phack.ambitious[,1] < 0.05)), length(which(phack.ambitious[,2] < 0.05)))
  expect_gt(length(which(phack.normal[,1] < 0.05)), length(which(phack.normal[,2] < 0.05)))

  phack.ambitious2 <- sim.multDVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3, ambitious = TRUE, iter = 500, alternative = "two.sided", alpha = 0.05, seed = 2345)
  phack.normal2 <- sim.multDVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3, ambitious = FALSE, iter = 500, alternative = "two.sided", alpha = 0.05, seed = 2345)
  expect_gt(length(which(phack.ambitious2[,1] < 0.05)), length(which(phack.ambitious[,1] < 0.05)))
  expect_gt(length(which(phack.normal2[,1] < 0.05)), length(which(phack.normal[,1] < 0.05)))
})

test_that("Selective Reporting of IV works", {

  phack.ambitious <- sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3, ambitious = TRUE, iter = 500, alternative = "two.sided", alpha = 0.05, seed = 1234)
  phack.normal <- sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3, ambitious = FALSE, iter = 500, alternative = "two.sided", alpha = 0.05, seed = 1234)

  expect_equal(nrow(phack.ambitious), 500)
  expect_equal(phack.ambitious[,2], phack.normal[,2])
  expect_equal(length(which(phack.ambitious[,1] < 0.05)), length(which(phack.normal[,1] < 0.05)))
  expect_gt(length(which(phack.ambitious[,1] < 0.05)), length(which(phack.ambitious[,2] < 0.05)))
  expect_gt(length(which(phack.normal[,1] < 0.05)), length(which(phack.normal[,2] < 0.05)))

  phack.ambitious2 <- sim.multIVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3, ambitious = TRUE, iter = 500, alternative = "two.sided", alpha = 0.05, seed = 2345)
  phack.normal2 <- sim.multIVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3, ambitious = FALSE, iter = 500, alternative = "two.sided", alpha = 0.05, seed = 2345)
  expect_gt(length(which(phack.ambitious2[,1] < 0.05)), length(which(phack.ambitious[,1] < 0.05)))
  expect_gt(length(which(phack.normal2[,1] < 0.05)), length(which(phack.normal[,1] < 0.05)))

})

test_that("Incorrect Rounding works", {

  phack1 <- sim.roundhack(0.1, iter = 500, alternative = "two.sided", alpha = 0.05, seed = 1234)
  phack2 <- sim.roundhack(0.06, iter = 500, alternative = "two.sided", alpha = 0.05, seed = 1234)

  expect_equal(nrow(phack1), 500)
  expect_equal(phack1[,2], phack2[,2])
  expect_gt(length(which(phack1[,1] <= 0.05)), length(which(phack1[,2] <= 0.05)))
  expect_gt(length(which(phack1[,1] <= 0.05)), length(which(phack2[,1] <= 0.05)))

  })

test_that("Optional Stopping works", {

  optstop1 <- sim.optstop(n.min = 10, n.max = 50, step = 5, alternative = "two.sided", iter = 500, alpha = 0.05, seed = 1234)
  optstop2 <- sim.optstop(n.min = 20, n.max = 50, step = 5, alternative = "two.sided", iter = 500, alpha = 0.05, seed = 1234)

  expect_equal(nrow(optstop1), 500)
  expect_equal(optstop1[,2], optstop2[,2])
  expect_gt(length(which(optstop1[,1] <= 0.05)), length(which(optstop2[,1] <= 0.05)))
  expect_gt(length(which(optstop1[,1] <= 0.05)), length(which(optstop1[,2] <= 0.05)))

})

test_that("Outlier Exclusion works", {

  outexcl1 <- sim.outHack(nobs = 20, which = "random", ambitious = FALSE, alpha = 0.05, iter = 500, seed = 1234)
  outexcl2 <- sim.outHack(nobs = 20, which = "random", ambitious = TRUE, alpha = 0.05, iter = 500, seed = 1234)

  expect_equal(nrow(outexcl1), 500)
  expect_equal(outexcl1[,2], outexcl2[,2])
  expect_equal(length(which(outexcl1[,1] <= 0.05)), length(which(outexcl2[,1] <= 0.05)))
  expect_gt(length(which(outexcl1[,1] <= 0.05)), length(which(outexcl1[,2] <= 0.05)))

})

test_that("Exploiting Covariates works", {

  covhack1 <- sim.covhack(nobs.group = 20, ncov = 3, rcov = 0.1, rcovdv = 0.6, interactions = FALSE, ambitious = FALSE, alpha = 0.05, iter = 500, seed = 1234)
  covhack2 <- sim.covhack(nobs.group = 20, ncov = 3, rcov = 0.1, rcovdv = 0.6, interactions = FALSE, ambitious = TRUE, alpha = 0.05, iter = 500, seed = 1234)

  expect_equal(nrow(covhack1), 500)
  expect_equal(covhack1[,2], covhack2[,2])
  expect_equal(length(which(covhack1[,1] <= 0.05)), length(which(covhack2[,1] <= 0.05)))
  expect_gt(length(which(covhack1[,1] <= 0.05)), length(which(covhack1[,2] <= 0.05)))

})

test_that("Subgroup Analyses work", {

  subgrhack1 <- sim.subgroupHack(nobs.group = 30, nsubvars = 3, alternative = "two.sided", ambitious = FALSE, alpha = 0.05, iter = 500, seed = 1234)
  subgrhack2 <- sim.subgroupHack(nobs.group = 30, nsubvars = 3, alternative = "two.sided", ambitious = TRUE, alpha = 0.05, iter = 500, seed = 1234)

  expect_equal(nrow(subgrhack1), 500)
  expect_equal(subgrhack1[,2], subgrhack2[,2])
  expect_equal(length(which(subgrhack1[,1] <= 0.05)), length(which(subgrhack2[,1] <= 0.05)))
  expect_gt(length(which(subgrhack1[,1] <= 0.05)), length(which(subgrhack1[,2] <= 0.05)))

})

test_that("Scale Redefinition works", {

  scaledef1 <- sim.compscoreHack(nobs = 30, ncompv = 10, rcomp = 0.5, ndelete = 5, ambitious = FALSE, alpha = 0.05, iter = 500, seed = 1234)
  scaledef2 <- sim.compscoreHack(nobs = 30, ncompv = 10, rcomp = 0.5, ndelete = 5, ambitious = TRUE, alpha = 0.05, iter = 500, seed = 1234)

  expect_equal(nrow(scaledef1), 500)
  expect_equal(scaledef1[,2], scaledef2[,2])
  expect_equal(length(which(scaledef1[,1] <= 0.05)), length(which(scaledef2[,1] <= 0.05)))
  expect_gt(length(which(scaledef1[,1] <= 0.05)), length(which(scaledef1[,2] <= 0.05)))

})
