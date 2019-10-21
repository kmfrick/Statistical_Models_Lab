# Assuming RStudio as IDE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Ex 2

cat("\n***Exercise 2***\n")

chol <- read.table("chol.txt", header=TRUE)
chol.y <- chol$CHOL
chol.x1 <- chol$Age
chol.model1 <- lm(chol.y ~ chol.x1)
print(summary(chol.model1))
print(anova(chol.model1))

# Ex 3

cat("\n***Exercise 3***\n")

auto.n <- 8
auto.sum.x <- 90
auto.sum.y <- 474
auto.sum.xy <- 4739
auto.sum.x.sq <- 1396
auto.sum.y.sq <- 29642
auto.mean.x <- auto.sum.x / auto.n
auto.mean.x.sq <- auto.sum.x.sq / auto.n
auto.mean.y <- auto.sum.y / auto.n
auto.mean.y.sq <- auto.sum.y.sq / auto.n
auto.mean.xy <- auto.sum.xy / auto.n
auto.var.x <- auto.mean.x.sq - auto.mean.x ^ 2
auto.var.y <- auto.mean.y.sq - auto.mean.y ^ 2
auto.cov.xy <- auto.mean.xy - auto.mean.x * auto.mean.y

auto.b1 <- auto.cov.xy / auto.var.x
auto.b0 <- auto.mean.y - auto.b1 * auto.mean.x

auto.x <- seq(0, auto.mean.x * 2, auto.mean.x * 2)
auto.y <- auto.b0 + auto.b1 * auto.x
plot(auto.x, auto.y, xlab="Driving exp (yrs)", ylab="Insr prem (USD)")
abline(auto.b0, auto.b1)

auto.r.sq <- auto.cov.xy ^ 2 / (auto.var.x * auto.var.y)

auto.sstot <- auto.sum.y.sq - auto.n * auto.mean.y ^ 2
auto.ssr <- auto.r.sq * auto.sstot
auto.sse <- auto.sstot - auto.ssr
auto.mse <- auto.sse / (auto.n - 2)

auto.t <- auto.b1 / sqrt(auto.mse / (auto.sum.x.sq - auto.n * auto.mean.x ^ 2))
auto.f <- auto.t ^ 2
cat(paste("b0 = ", format(auto.b0, digits=3)), "\n")
cat(paste("b1 = ", format(auto.b1, digits=3)), "\n")
cat(paste("R-squared = ", format(auto.r.sq, digits=3)), "\n")
cat(paste("toss = ", format(auto.t, digits=3)), "\n")
cat(paste("Foss = ", format(auto.f, digits=3)), "\n")
cat(paste("p-value = ", format(pf(auto.f, 1, auto.n - 2, lower.tail=FALSE), digits = 3)), "\n")

# Ex 4

cat("\n***Exercise 4***\n")

slrm.b0 <- 0.721
slrm.b1 <- -1.788
slrm.mse <- 0.4384
slrm.n <- 8 + 2

slrm.var.b0 <- 0.139
slrm.var.b1 <- 0.260

slrm.toss <- slrm.b1 / slrm.var.b1
slrm.Foss <- slrm.toss ^ 2

slrm.p <- pf(slrm.Foss, 1, slrm.n - 2, lower.tail=FALSE)
cat(paste("p-value = ", format(slrm.p, digits = 3)), "\n")
slrm.R.sq <- (slrm.Foss / (slrm.n - 2)) / (1 + (slrm.Foss / (slrm.n - 2)))
cat(paste("R-squared = ", format(slrm.R.sq, digits = 3)), "\n")