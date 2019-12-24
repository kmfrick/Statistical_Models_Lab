# Assuming RStudio as IDE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));

# Exercise 2

cat("\n\n***Exercise 2***\n\n");

seeds <- read.table("seeds3.txt", header = TRUE);
seeds.n <- nrow(seeds);
seeds.X <- cbind(rep(1, seeds.n), seeds$Treatment == "TreatA", seeds$Treatment == "TreatB");
seeds.p <- ncol(seeds.X);
seeds.model <- lm(Weight ~ (Treatment == "TreatA") + (Treatment == "TreatB"), seeds);
print(summary(seeds.model));
# R-squared is low, so the kind of treatment applied does not explain much

seeds.est.A <- predict(seeds.model, newdata = data.frame(Treatment = "TreatA"));
seeds.est.B <- predict(seeds.model, newdata = data.frame(Treatment = "TreatB"));
cat("Est. weight for seeds treated A = ", seeds.est.A, "\n");
cat("Est. weight for seeds treated B = ", seeds.est.B, "\n");
cat("Est. seed weight difference between treatments = ", abs(seeds.est.A - seeds.est.B), "\n");

seeds.sse <- sum(seeds.model$residuals ^ 2);
seeds.ssto <- sum((seeds$Weight - mean(seeds$Weight))^2);
seeds.ssr <- seeds.ssto - seeds.sse;
seeds.mse <- seeds.sse / (seeds.n - seeds.p);
seeds.msr <- seeds.ssr / (seeds.p - 1);
seeds.Foss <- seeds.msr / seeds.mse;

cat("p-val = ", pf(seeds.Foss, seeds.p - 1, seeds.n - seeds.p, lower.tail = FALSE), "\n");

# Exercise 3

cat("\n\n***Exercise 3***\n\n");

#load("Es3.RData");

cat.n <- 213;
cat.mse <- S[1,1];
cat.mean.y <- samp.m[1];

cat.svar.mat <- as.matrix(S[4:6, 4:6]);
cat.scov.vec <- as.vector(S[4:6, 1]);
cat.svar.vec <- diag(cat.svar.mat);
cat.p <- ncol(cat.svar.mat) + 1;
cat.mean.x.vec <- samp.m[4:6];
cat.dev.sq.x.vec <- diag(cat.svar.mat) * (cat.n - 1);
cat.br <- solve(cat.svar.mat) %*% cat.scov.vec;
cat.b0 <- cat.mean.y - t(cat.mean.x.vec) %*% cat.br;
cat.b <- c(cat.b0, cat.br);
cat.R.sq <- t(cat.scov.vec) %*% solve(cat.svar.mat) %*% cat.scov.vec / cat.mse;
cat.R.sq.red <- t(cat.scov.vec[-2]) %*% solve(cat.svar.mat[-2, -2]) %*% cat.scov.vec[-2] / cat.mse;
cat.Fpart <- (cat.R.sq - cat.R.sq.red) / (1 - cat.R.sq) * (cat.n - cat.p) / (cat.p - (cat.p - 1));

cat("The estimated average difference in Y between units showing category III and units showing category I is", cat.br[4]);
cat("p-value (beta_4 = 0) = ", pf(cat.Fpart, 1, cat.n - cat.p, lower.tail = FALSE));

cat.svar.mat.full <- as.matrix(S[2:6, 2:6]);
cat.scov.vec.full <- as.vector(S[2:6, 1]);
cat.svar.vec.full <- diag(cat.svar.mat.full);
cat.p.full <- ncol(cat.svar.mat.full) + 1;
cat.mean.x.vec.full <- samp.m[2:6]
cat.dev.sq.x.vec.full <- diag(cat.svar.mat.full) * (cat.n - 1);
cat.br.full <- solve(cat.svar.mat.full) %*% cat.scov.vec.full;
cat.b0.full <- cat.mean.y - t(cat.mean.x.vec.full) %*% cat.br.full;
cat.b.full <- c(cat.b0.full, cat.br.full);
cat.R.sq.full <- t(cat.scov.vec.full) %*% solve(cat.svar.mat.full) %*% cat.scov.vec.full / cat.mse;
cat.R.sq.red.full <- t(cat.scov.vec.full[-4]) %*% solve(cat.svar.mat.full[-4, -4]) %*% cat.scov.vec.full[-4] / cat.mse;
cat.Fpart.full <- (cat.R.sq.full - cat.R.sq.red.full) / (1 - cat.R.sq.full) * (cat.n - cat.p.full) / (cat.p.full - (cat.p.full - 1));

cat("[FULL MODEL] p-value (beta_4 = 0) = ", pf(cat.Fpart.full, 1, cat.n - cat.p.full, lower.tail = FALSE));
