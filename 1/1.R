# Assuming RStudio as IDE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Ex 2

cat("\n\n***Exercise 2***\n\n")
grad.ni <- c(5, 5, 4, 3)
grad.r <- 4
grad.mean.y.grp <- c(32.660, 28.720, 24.150, 20.567)
grad.svar <- c(7.963, 12.277, 8.603, 8.423)
grad.n <- sum(grad.ni)
grad.mean.y <- mean(grad.mean.y.grp)
grad.msb <- sum((grad.ni * (grad.mean.y.grp - grad.mean.y)^2)) / (ship.r - 1)
grad.msw <- sum((grad.ni - 1) * grad.svar) / (grad.n - grad.r)

grad.Foss <- grad.msb / grad.msw

cat("Foss = ", grad.Foss, "\n")
cat("p-value = ", pf(grad.Foss, 1, grad.n - 2, lower.tail=FALSE), "\n")


# Ex 3

cat("\n\n***Exercise 3***\n\n")

seeds2 <- read.table("seeds2.txt",header=TRUE)
print(summary(aov(seeds2$Weight ~ seeds2$Group)))

print(t.test(seeds2$Weight[1:20], seeds2$Weight[21:40],var.equal=TRUE))
