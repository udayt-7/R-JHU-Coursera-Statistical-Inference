mu <- 1100
si <- 30
n <- 9
CI <- mu + c(-1,1)*si*qt(0.975,df=n-1)/sqrt(n)
CI

mu <- -2
n <- 9
sig <- mu*sqrt(n)/qt(0.975,df=n-1)
sig

quantile = 0.95

n_y <- 9
n_x <- 9
sd_y <- 1.5
sd_x <- 1.8
m_y <- -3
m_x <- 1

??_p <- sqrt(((n_x - 1) * sd_x^2 + (n_y - 1) * sd_y^2)/(n_x + n_y - 2))

confidenceInterval <-  m_y - m_x + c(-1, 1) * qt(quantile, df=n_y+n_x-2) * ??_p * (1 / n_x + 1 / n_y)^.5
round(confidenceInterval,3)

quantile = 0.975

n_y <- 100
n_x <- 100
sd_y <- 0.5
sd_x <- 2
m_y <- 4
m_x <- 6

??_p <- sqrt(((n_x - 1) * sd_x^2 + (n_y - 1) * sd_y^2)/(n_x + n_y - 2))

confidenceInterval <-  m_y - m_x + c(-1, 1) * qnorm(quantile) * ??_p * (1 / n_x + 1 / n_y)^.5
round(confidenceInterval,2)
