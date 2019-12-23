# Assuming RStudio as IDE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));

# Ex 2

cat("\n\n** Exercise 2 **\n\n");
expend <- read.table("expenditure.txt", header = TRUE);
expend.model <-  lm(Y ~ (X1 + X2 + X3), expend);
#plot(expend.model); # Linearity and normality assumptions seem reasonable, but homoskedasticity assumption doesn't

# Ex 3

cat("\n\n** Exercise 3 **\n\n");
cars <- read.table("cars.txt", header = TRUE);
cars.n <- nrow(cars);
cars.X <- cbind(rep(1, cars.n), as.matrix(cars[1:3]));
cars.hat.mat <- cars.X %*% solve(t(cars.X) %*% cars.X) %*% t(cars.X);

cars.p <- ncol(cars.X);
cars.residuals <- cars$e;
cars.sse <- sum(cars.residuals ^ 2);
cars.mse <- (cars.sse / (cars.n - cars.p));
cars.studres <- cars.residuals * sqrt((cars.n - cars.p - 1) / (cars.sse * (1 - diag(cars.hat.mat)) - cars.residuals^2))

cars.alpha.bonf <- 0.01 / cars.n;
j <- 1;
for (i in cars.studres) {
  if (abs(i) > qt(1 - cars.alpha.bonf/2, cars.n - cars.p - 1)) {
    cat(j, "-th unit is an outlier\n");
  }
  j <- j + 1;
} # LexusHS250h_2.4L is probably an outlier

cars.cook <- cars.residuals^2 / (cars.p * cars.mse) * diag(cars.hat.mat) / (1 - diag(cars.hat.mat))^2;
j <- 1;
for (i in cars.cook) {
  
  if (i > qf(0.5, cars.p, cars.n - cars.p)) {
    cat(j, "-th unit is influential\n");
  } 
  j <- j + 1;
} # No influential units
