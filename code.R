library(mvtnorm)
library(tidyverse)
set.seed(15)

alpha = 0.05
r = 1000
n = 200
reject_match = 0
fail_match = 0
y1_capture = 0
y2_capture = 0
y3_capture = 0
manova_reject = 0
anova_reject = 0

for (i in 1:r){
  x <- seq(0, 32 * pi, length.out = n)+rnorm(200, sd=0.1)
  y_11 <- sin(x)+rnorm(200, sd=0.1)
  y_12 <- cos(x)+rnorm(200, sd=0.1)
  y_13 <- sin(2*x)+rnorm(200, sd=0.1)
  X1 <- cbind(x, y_11, y_12, y_13)


  y_21 <- sin(x)+rnorm(200, mean=0.12, sd=0.1)
  y_22 <- cos(x)+rnorm(200, mean=0.12, sd=0.1)
  y_23 <- sin(2*x)+rnorm(200, mean=0.12, sd=0.1)
  X2 <- cbind(x, y_21, y_22, y_23)


  dat <- as.data.frame(rbind(X1, X2)) |>
    mutate(group = factor(rep(c(1, 2), each = n)))

  colnames(dat) <- c("X", "Y1", "Y2", "Y3", "group")

  manova_res <- manova(cbind(Y1, Y2, Y3) ~ group, data = dat)
  out_summary <- summary(manova_res, test = "Wilks")

  manova_p_value = out_summary$stats[, "Pr(>F)"][[1]]

  y1_test = t.test(X1[, 2], X2[, 2], var.equal = TRUE)
  y1_pvalue = y1_test$p.value
  if (y1_test$conf[[1]] <= -0.12 && y1_test$conf[[2]] >= -0.12){
    y1_capture = y1_capture + 1
  }

  y2_test = t.test(X1[, 3], X2[, 3], var.equal = TRUE)
  y2_pvalue = y2_test$p.value
  if (y2_test$conf[[1]] <= -0.12 && y2_test$conf[[2]] >= -0.12){
    y2_capture = y2_capture + 1
  }

  y3_test = t.test(X1[, 4], X2[, 4], var.equal = TRUE)
  y3_pvalue = y3_test$p.value

  if (y3_test$conf[[1]] <= -0.12 && y3_test$conf[[2]] >= -0.12){
    y3_capture = y3_capture + 1
  }

  count = 0
  if (manova_p_value <= alpha){
    manova_reject = manova_reject+1
    if (y1_pvalue <= alpha/3){
      count = count + 1
    }
    if (y2_pvalue <= alpha/3){
      count = count + 1
    }
    if (y3_pvalue <= alpha/3){
      count = count + 1
    }

    if (count >= 2){
      reject_match = reject_match + 1
      anova_reject = anova_reject + 1
    }
  }

  else{
    if (y1_pvalue > alpha/3){
      count = count + 1
    }
    if (y2_pvalue > alpha/3){
      count = count + 1
    }
    if (y3_pvalue > alpha/3){
      count = count + 1
    }

    if (count >= 2){
      fail_match = fail_match + 1
    } else{ anova_reject = anova_reject + 1 }
  }

}

paste0("Proportion of matching results: ", (fail_match+reject_match)/1000)
test1_matching = (fail_match+reject_match)/1000

paste0("Proportion of matching fail to reject: ", (fail_match)/1000)
test1_matching_fail = fail_match/1000

paste0("Proportion of matching reject: ", (reject_match)/1000)
test1_matching_reject = reject_match/1000

paste0("Proportion of MANOVA reject: ", (manova_reject)/1000)
test1_manova_reject = manova_reject/1000

paste0("Proportion of ANOVA majority reject: ", (anova_reject)/1000)
test1_anova_reject = anova_reject/1000

paste0("Proportion of mismatched results: ", (1000-fail_match+reject_match)/1000)
test1_mismatched = (1000-fail_match+reject_match)/1000

paste0("Proportion of True Y1 Value: ", (y1_capture)/1000)
test1_y1_true = y1_capture/1000

paste0("Proportion of True Y2 Value: ", (y2_capture)/1000)
test1_y2_true = y2_capture/1000

paste0("Proportion of True Y3 Value: ", (y3_capture)/1000)
test1_y3_true = y3_capture/1000


fit <- lm(
  cbind(Y1, Y2, Y3) ~ X + group,
  data = dat
)

E <- residuals(fit)

library(scatterplot3d)

group_factor <- factor(dat$group)

group_colors <- c("#B10DC9", "#85144b")[as.integer(group_factor)]

scatterplot3d(
  x = dat$Y1,
  y = dat$Y2,
  z = dat$Y3,
  color = group_colors,
  pch = 19,
  xlab = "Y1",
  ylab = "Y2",
  zlab = "Y3",
  main = "Three-Dimensional Scatterplot"
)

legend(
  "topright",
  legend = levels(group_factor),
  col = c("#B10DC9", "#85144b"),
  pch = 19,
  bty = "n",
  title = "Group"
)

par(mfrow = c(3, 1))

acf(E[, "Y1"], main = "ACF of y1 residuals")
acf(E[, "Y2"], main = "ACF of y2 residuals")
acf(E[, "Y3"], main = "ACF of y3 residuals")

library(mvtnorm)
library(tidyverse)
set.seed(15)
alpha = 0.05
r = 1000
n = 200
reject_match = 0
fail_match = 0
y1_capture = 0
y2_capture = 0
y3_capture = 0
manova_failreject = 0
anova_failreject = 0

for (i in 1:r){
  x <- seq(0, 32 * pi, length.out = n)+rnorm(200, sd=1)
  y_11 <- sin(x)+rnorm(200, sd=1)
  y_12 <- cos(x)+rnorm(200, sd=1)
  y_13 <- sin(2*x)+rnorm(200, sd=1)
  X1 <- cbind(x, y_11, y_12, y_13)


  y_21 <- sin(x)+rnorm(200, sd=1)
  y_22 <- cos(x)+rnorm(200, sd=1)
  y_23 <- sin(2*x)+rnorm(200, sd=1)
  X2 <- cbind(x, y_21, y_22, y_23)


  dat <- as.data.frame(rbind(X1, X2)) |>
    mutate(group = factor(rep(c(1, 2), each = n)))

  colnames(dat) <- c("X", "Y1", "Y2", "Y3", "group")

  manova_res <- manova(cbind(Y1, Y2, Y3) ~ group, data = dat)
  out_summary <- summary(manova_res, test = "Wilks")

  manova_p_value = out_summary$stats[, "Pr(>F)"][[1]]

  y1_test = t.test(X1[, 2], X2[, 2], var.equal = TRUE)
  y1_pvalue = y1_test$p.value
  if (y1_test$conf[[1]] <= 0 && y1_test$conf[[2]] >= 0){
    y1_capture = y1_capture + 1
  }

  y2_test = t.test(X1[, 3], X2[, 3], var.equal = TRUE)
  y2_pvalue = y2_test$p.value
  if (y2_test$conf[[1]] <= 0 && y2_test$conf[[2]] >= 0){
    y2_capture = y2_capture + 1
  }

  y3_test = t.test(X1[, 4], X2[, 4], var.equal = TRUE)
  y3_pvalue = y3_test$p.value

  if (y3_test$conf[[1]] <= 0 && y3_test$conf[[2]] >= 0){
    y3_capture = y3_capture + 1
  }

  count = 0

  if (manova_p_value <= alpha){
    if (y1_pvalue <= alpha/3){
      count = count + 1
    }
    if (y2_pvalue <= alpha/3){
      count = count + 1
    }
    if (y3_pvalue <= alpha/3){
      count = count + 1
    }

    if (count >= 2){
      reject_match = reject_match + 1
    } else { anova_failreject = anova_failreject + 1 }
  }

  else{
    manova_failreject = manova_failreject+1
    if (y1_pvalue > alpha/3){
      count = count + 1
    }
    if (y2_pvalue > alpha/3){
      count = count + 1
    }
    if (y3_pvalue > alpha/3){
      count = count + 1
    }

    if (count >= 2){
      fail_match = fail_match + 1
      anova_failreject = anova_failreject+1
    }
  }

}


paste0("Proportion of matching results: ", (fail_match+reject_match)/1000)
test2_matching = (fail_match+reject_match)/1000

paste0("Proportion of matching fail to reject: ", (fail_match)/1000)
test2_matching_fail = fail_match/1000

paste0("Proportion of MANOVA fail to reject: ", (manova_failreject)/1000)
test2_manova_fail = manova_failreject/1000

paste0("Proportion of ANOVA majority fail to reject: ", (anova_failreject)/1000)
test2_anova_fail = anova_failreject/1000

paste0("Proportion of matching reject: ", (reject_match)/1000)
test2_matching_reject = reject_match/1000

paste0("Proportion of mismatched results: ", (1000-fail_match+reject_match)/1000)
test2_mismatched = (1000-fail_match+reject_match)/1000

paste0("Proportion of True Y1 Value: ", (y1_capture)/1000)
test2_y1_true = y1_capture/1000

paste0("Proportion of True Y2 Value: ", (y2_capture)/1000)
test2_y2_true = y2_capture/1000

paste0("Proportion of True Y3 Value: ", (y3_capture)/1000)
test2_y3_true = y3_capture/1000


fit <- lm(
  cbind(Y1, Y2, Y3) ~ X + group,
  data = dat
)

E <- residuals(fit)

library(scatterplot3d)

group_factor <- factor(dat$group)

group_colors <- c("#0057B8", "#007A73")[as.integer(group_factor)]

scatterplot3d(
  x = dat$Y1,
  y = dat$Y2,
  z = dat$Y3,
  color = group_colors,
  pch = 19,
  xlab = "Y1",
  ylab = "Y2",
  zlab = "Y3",
  main = "Three-Dimensional Scatterplot"
)

legend(
  "topright",
  legend = levels(group_factor),
  col = c("#0057B8", "#007A73"),
  pch = 19,
  bty = "n",
  title = "Group"
)

par(mfrow = c(3, 1))


acf(E[, "Y1"], main = "ACF of y1 residuals")
acf(E[, "Y2"], main = "ACF of y2 residuals")
acf(E[, "Y3"], main = "ACF of y3 residuals")

