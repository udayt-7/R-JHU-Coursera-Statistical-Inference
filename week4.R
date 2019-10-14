A <- c(140,138,150,148,135)
B <- c(132,135,151,146,130)

t.test(B,A,alternative="two.sided",paired=TRUE)


CI <- 1100 + c(-1,1)*qt(0.975,8)*30/sqrt(9)
CI


pbinom(2, size = 4, prob = 0.5, lower.tail = FALSE)
chisq.test(c(3,1),p=c(0.5,0.5))


ppois(10, lambda = 0.01 * 1787)


n1 <- n2 <- 9
x1 <- -3 ##treated
x2 <- 1 ##placebo
s1 <- 1.5 ##treated
s2 <- 1.8 ##placebo
s <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2))
ts <- (x1 - x2)/(s * sqrt(1/n1 + 1/n2))
2 * pt(ts, n1 + n2 - 2)


pnorm(1.645 * 0.004, mean = 0.01, sd = 0.004, lower.tail = FALSE)



ceiling((4 * (qnorm(0.95) - qnorm(0.1)))^2)
