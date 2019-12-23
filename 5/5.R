# Assuming RStudio as IDE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));

# Ex 2

cat("\n\n** Exercise 2 **\n\n");
house <- read.table("house2.txt", header = TRUE);
house.X <- cbind(rep(1, house.n), house$Size, house$Beds, house$Taxes);
house.n <- nrow(house.X);
house.p <- ncol(house.X);
house.model <- lm(Price ~ (Size + Beds + Taxes), house);
house.xh <- c(1, 250, 3, 4);
house.mse <- sum(house.model$residuals ^ 2) / (house.n - house.p);
house.b <- as.numeric(house.model$coefficients);
house.y.h.calc <- t(house.xh) %*% house.b;
house.alpha <- 0.01;
house.XTX.inv <- solve(t(house.X) %*% house.X);
house.y.h.pred <- sqrt(house.mse * (1 + t(house.xh) %*% house.XTX.inv %*% house.xh)) * qt(1 - house.alpha / 2, house.n - house.p);
cat("y_h = ", house.y.h.calc, "\n");
cat("[", house.y.h.calc - house.y.h.pred, "< Y_h < ", house.y.h.calc + house.y.h.pred, "]\n");

# Ex 3

cat("\n\n** Exercise 3 **\n\n");
ship <- read.table("shipdept.txt", header = TRUE);
ship.n <- nrow(ship);
ship.p <- ncol(ship) + 1;
ship.X <- cbind(rep(1, ship.n), as.matrix(ship));
ship.b <- c(97.787, 5.750, 8.034, -1.828);
ship.R.sq <- 0.818;
ship.xh <- c(1, 6.5, 0.5, 15)
ship.y.h.calc <- t(ship.xh) %*% ship.b;
cat("E[y_h] = ", ship.y.h.calc, "\n")
ship.lab.hat <- ship.X %*% ship.b;
ship.mean.lab <- 93.550;
ship.err <- ship.lab.hat - ship.mean.lab;
ship.ssr <- t(ship.err) %*% ship.err;
ship.ssto <- ship.ssr / ship.R.sq;
ship.sse <- ship.ssto - ship.ssr;
ship.mse <- ship.sse / (ship.n - ship.p);
ship.XTX.inv <- solve(t(ship.X) %*% ship.X);
ship.alpha <- 0.05;
ship.y.h.conf <- sqrt(ship.mse * (t(ship.xh) %*% ship.XTX.inv %*% ship.xh)) * qt(1 - ship.alpha / 2, ship.n - ship.p);
cat("[", ship.y.h.calc - ship.y.h.conf, "< Y_h < ", ship.y.h.calc + ship.y.h.conf, "]\n")