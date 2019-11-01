# Assuming RStudio as IDE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Ex 2

cat("\n\n***Exercise 2***\n\n")
chol <- read.table("chol.txt",header=TRUE)

chol.model <- lm(CHOL ~ (Age + BMI), chol)
chol.b0 <- chol.model$coefficients[1]
chol.b1 <- chol.model$coefficients[2]
chol.b2 <- chol.model$coefficients[3]
cat("b0 = ", chol.b0, "\n")
cat("b1 = ", chol.b1, "\n")
cat("b2 = ", chol.b2, "\n")

chol.no.age <- lm(CHOL ~ Age, chol)$residuals
chol.no.age.eff.on.bmi <- lm(BMI ~ Age, chol)$residuals
chol.model.no.age <- lm(chol.no.age ~ chol.no.age.eff.on.bmi - 1)

chol.no.bmi <- lm(CHOL ~ BMI, chol)$residuals
chol.no.bmi.eff.on.age <- lm(Age ~ BMI, chol)$residuals
chol.model.no.bmi <- lm(chol.no.bmi ~ chol.no.bmi.eff.on.age - 1)

chol.fwl.b2 <- chol.model.no.bmi$coefficients[1]
chol.fwl.b1 <- chol.model.no.age$coefficients[1]

cat("[FWL] Age: b1 = ", chol.b1, "\n")
cat("[FWL] BMI: b2 = ", chol.b2, "\n")

# Ex 3

cat("\n\n***Exercise 3***\n\n")
house <- read.table("house2.txt",header=TRUE)
house.model <- lm(Price ~ (Taxes + Beds + Baths + Size), house)
cat("Intercept: b0 = ", house.model$coefficients[1], "\n")
cat("Taxes: b1 = ", house.model$coefficients[2], "\n")
cat("Beds: b2 = ", house.model$coefficients[3], "\n")
cat("Baths: b3 = ", house.model$coefficients[4], "\n")
cat("Size: b4 = ", house.model$coefficients[5], "\n")

house.price.no.bedsbathssize <- lm(Price ~ (Beds + Baths + Size), house)$residuals
house.taxes.no.bedsbathsize <- lm(Taxes ~ (Beds + Baths + Size), house)$residuals

house.b1 <- lm(house.price.no.bedsbathssize ~ house.taxes.no.bedsbathsize - 1)$coefficients[1]
cat("[FWL] Taxes: b1 = ", house.b1, "\n")

# Ex 4

cat("\n\n***Exercise 4***\n\n")
ship <- read.table("shipdept.txt", header=TRUE)
ship.mean.lab <- 93.550
ship.svar.lab <- 362.576
ship.scov.lab.tws <- 24.337
ship.scov.lab.pst <- 0.133
ship.scov.lab.asw <- -85.150

ship.scov.vec <- rbind(ship.scov.lab.tws, ship.scov.lab.pst, ship.scov.lab.asw)
ship.svar.mat <- var(as.matrix(ship))
ship.mean.x.vect <- rbind(mean(ship$Tws), mean(ship$Pst), mean(ship$Asw))

ship.b <- solve(ship.svar.mat) %*% ship.scov.vec
ship.b0 <- ship.mean.lab - t(ship.mean.x.vect) %*% ship.b
cat("[FWL] b3 = ", ship.b[3], "\n")
cat("[FWL] b2 = ", ship.b[2], "\n")
cat("[FWL] b1 = ", ship.b[1], "\n")
cat("[FWL] b0 = ", ship.b0, "\n")