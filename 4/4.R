# Assuming RStudio as IDE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Ex 2

cat("\n\n***Exercise 2***\n\n")

house <- read.table("house2.txt",header=TRUE)
house.model <- lm(Price ~ (Taxes + Beds + Baths + Size), house)
house.model.reduced <- lm(Price ~ (Taxes + Size), house)

house.r.sq.full <- summary(house.model)$r.squared
house.r.sq.red <- summary(house.model.reduced)$r.squared

house.n <- length(house$Price)
house.p <- nrow(as.matrix(house.model$coefficients)) - 1
house.q <- nrow(as.matrix(house.model.reduced$coefficients)) - 1
partial.f <- (house.r.sq.full - house.r.sq.red) / (1 - house.r.sq.full) * (house.n - house.p) / (house.p - house.q)

cat("Foss = ", partial.f, "\n")
cat("p-value = ", pf(partial.f, 1, house.n - house.p, lower.tail=FALSE))

# Ex 3

cat("\n\n***Exercise 3***\n\n")
cat("Y <- Lab\nX[1..3] <- Tws, Pst, Asw\n")

ship <- read.table("shipdept.txt", header=TRUE)

ship.vif.tws <- 1 / (1 - summary(lm(ship$Tws ~ (ship$Pst + ship$Asw)))$r.squared)
ship.vif.pst <- 1 / (1 - summary(lm(ship$Pst ~ (ship$Tws + ship$Asw)))$r.squared)
ship.vif.asw <- 1 / (1 - summary(lm(ship$Asw ~ (ship$Pst + ship$Tws)))$r.squared)
cat ("VIF(Tws) = ", ship.vif.tws, "\n")
cat ("VIF(Pst) = ", ship.vif.pst, "\n")
cat ("VIF(Asw) = ", ship.vif.asw, "\n")

ship.n <- nrow(ship)
ship.I <- diag(1, ship.n, ship.n) # Identity matrix of size n
ship.p <- ncol(ship) + 1
ship.XR <- as.matrix(ship)
ship.X <- cbind(rep(1, ship.n), ship.XR)
ship.mean.lab <- 93.550
ship.svar.lab <- 362.576
ship.scov.lab.tws <- 24.337
ship.scov.lab.pst <- 0.133
ship.scov.lab.asw <- -85.150

ship.scov.vec <- c(ship.scov.lab.tws, ship.scov.lab.pst, ship.scov.lab.asw)
ship.svar.mat <- var(ship.XR)
ship.mean.x.vect <- c(mean(ship$Tws), mean(ship$Pst), mean(ship$Asw))

ship.b <- solve(ship.svar.mat) %*% ship.scov.vec
ship.b0 <- ship.mean.lab - t(ship.mean.x.vect) %*% ship.b
ship.b.vec <- c(ship.b0, ship.b)
ship.R.sq <- t(ship.scov.vec) %*% solve(ship.svar.mat) %*% ship.scov.vec / ship.svar.lab

ship.Foss <- ship.R.sq * (ship.n - ship.p) / ((1 - ship.R.sq) * (ship.p - 1))
cat("Foss = ", ship.Foss, "\n")

cat("p-value = ", paste(format(pf(ship.Foss, ship.p - 1, ship.n - ship.p, lower.tail = FALSE), digits=3)), "\n")

ship.lab.hat <- ship.X %*% ship.b.vec
ship.err <- ship.lab.hat - ship.mean.lab

ship.ssr <- t(ship.err) %*% ship.err
ship.ssto <- ship.ssr / ship.R.sq
ship.sse <- ship.ssto - ship.ssr
ship.mse <- ship.sse / (ship.n - ship.p)
ship.mean.tws <- mean(ship$Tws)
ship.mean.pst <- mean(ship$Pst)
ship.mean.asw <- mean(ship$Asw)
ship.dev.tws <- t(as.vector(ship$Tws) - ship.mean.tws) %*% as.vector(ship$Tws) - ship.mean.tws
ship.dev.pst <- t(as.vector(ship$Pst) - ship.mean.pst) %*% as.vector(ship$Pst) - ship.mean.pst
ship.dev.asw <- t(as.vector(ship$Asw) - ship.mean.asw) %*% as.vector(ship$Asw) - ship.mean.asw
ship.svar.b <- as.numeric(ship.mse) * solve(t(ship.X) %*% ship.X)
ship.toss.vec <- ship.b / sqrt(diag(ship.svar.b)[2:4])

for (i in seq(1,3)) {
  cat("Pr(beta[", i, "] = 0) = ", 2 * pt(abs(ship.toss.vec[i]), ship.n - ship.p, lower.tail = FALSE), "\n")
}

# Ex 4

cat("\n\n***Exercise 4***\n\n")

model.n = 22;
model.p = 3;

cat("H0 true ~ F(", model.p - 1, ", ", model.n - model.p, ")\n");
model.t0 = 1.234;
model.t1 = 7.435;
model.serr.b0 = 126.767;
model.serr.b1 = 1.759;
model.serr.b2 = 2.963;

model.b0 = model.t0 * model.serr.b0;
model.b1 = model.t1 * model.serr.b1;
model.b2 = 16.795;

model.Foss = 40.16;

model.pr = pf(model.Foss, model.p - 1, model.n - model.p, lower.tail = FALSE);
cat("Pr(beta0 = beta1 = beta2 = 0) = ", model.pr, "\n");

model.pr.b0 = 2 * pt(model.t0, model.n - model.p, lower.tail = FALSE);
model.pr.b1 = 2 * pt(model.t1, model.n - model.p, lower.tail = FALSE);
model.t2 = model.b2 / model.serr.b2;
model.pr.b2 = 2 * pt(model.t2, model.n - model.p, lower.tail = FALSE);

cat("Pr(beta0 = 0) = ", model.pr.b0, "\n")
cat("Pr(beta1 = 0) = ", model.pr.b1, "\n")
cat("Pr(beta2 = 0) = ", model.pr.b2, "\n")

model.r.sq = (model.Foss * (model.p - 1) / (model.n - model.p)) / (1 + (model.Foss * (model.p - 1) / (model.n - model.p)));
cat("R^2 = ", model.r.sq, "\n");
