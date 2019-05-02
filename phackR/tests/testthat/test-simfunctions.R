context('Simulation: P-Hacking Works')

test_that("Selective Reporting of DV works", {

  phack.ambitious <- sim.multDVhack(nobs.group = c(30, 30), nvar = 5, r = 0.3, ambitious = TRUE, iter = 1000, alternative = "two.sided", seed = 2345)
  phack.normal <- sim.multDVhack(nobs.group = c(30, 30), nvar = 5, r = 0.3, ambitious = FALSE, iter = 1000, alternative = "two.sided", seed = 2345)

  expect_equal(nrow(phack.ambitious), 1000)
  expect_equal(phack.ambitious[,2], phack.normal[,2])
  expect_equal(length(which(phack.ambitious[,1] < 0.05)), length(which(phack.normal[,1] < 0.05)))
  expect_gt(length(which(phack.ambitious[,1] < 0.05)), length(which(phack.ambitious[,2] < 0.05)))
  expect_gt(length(which(phack.normal[,1] < 0.05)), length(which(phack.normal[,2] < 0.05)))

  phack.ambitious2 <- sim.multDVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3, ambitious = TRUE, iter = 1000, alternative = "two.sided", seed = 2345)
  phack.normal2 <- sim.multDVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3, ambitious = FALSE, iter = 1000, alternative = "two.sided", seed = 2345)
  expect_gt(length(which(phack.ambitious2[,1] < 0.05)), length(which(phack.ambitious[,1] < 0.05)))
  expect_gt(length(which(phack.normal2[,1] < 0.05)), length(which(phack.normal[,1] < 0.05)))
})

test_that("Selective Reporting of IV works", {

  phack.ambitious <- sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3, ambitious = TRUE, iter = 1000, alternative = "two.sided", seed = 1234)
  phack.normal <- sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3, ambitious = FALSE, iter = 1000, alternative = "two.sided", seed = 1234)

  expect_equal(nrow(phack.ambitious), 1000)
  expect_equal(phack.ambitious[,2], phack.normal[,2])
  expect_equal(length(which(phack.ambitious[,1] < 0.05)), length(which(phack.normal[,1] < 0.05)))
  expect_gt(length(which(phack.ambitious[,1] < 0.05)), length(which(phack.ambitious[,2] < 0.05)))
  expect_gt(length(which(phack.normal[,1] < 0.05)), length(which(phack.normal[,2] < 0.05)))

  phack.ambitious2 <- sim.multIVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3, ambitious = TRUE, iter = 1000, alternative = "two.sided", seed = 2345)
  phack.normal2 <- sim.multIVhack(nobs.group = c(30, 30), nvar = 15, r = 0.3, ambitious = FALSE, iter = 1000, alternative = "two.sided", seed = 2345)
  expect_gt(length(which(phack.ambitious2[,1] < 0.05)), length(which(phack.ambitious[,1] < 0.05)))
  expect_gt(length(which(phack.normal2[,1] < 0.05)), length(which(phack.normal[,1] < 0.05)))

})

test_that("Incorrect Rounding works", {

  phack1 <- sim.roundhack(0.1, iter = 1000, alternative = "two.sided", seed = 1234)
  phack2 <- sim.roundhack(0.06, iter = 1000, alternative = "two.sided", seed = 1234)

  expect_equal(nrow(phack1), 1000)
  expect_equal(phack1[,2], phack2[,2])
  expect_gt(length(which(phack1[,1] <= 0.05)), length(which(phack1[,2] <= 0.05)))
  expect_gt(length(which(phack1[,1] <= 0.05)), length(which(phack2[,1] <= 0.05)))

  })

test_that("Optional Stopping works", {

  optstop1 <- sim.optstop(n.min = 10, n.max = 50, step = 5, alternative = "two.sided", iter = 1000, seed = 1234)
  optstop2 <- sim.optstop(n.min = 20, n.max = 50, step = 5, alternative = "two.sided", iter = 1000, seed = 1234)

  expect_equal(nrow(optstop1), 1000)
  expect_equal(optstop1[,2], optstop2[,2])
  expect_gt(length(which(optstop1[,1] <= 0.05)), length(which(optstop2[,1] <= 0.05)))
  expect_gt(length(which(optstop1[,1] <= 0.05)), length(which(optstop1[,2] <= 0.05)))

})
