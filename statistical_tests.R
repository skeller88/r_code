##### One-Way ANOVA
samples <- c(a, b, c, ...)
means <- sapply(samples, mean)
ss <- sapply(samples, function(x) sum( (x - mean(x))^2 ))
ss_b <- 0
ss_w <- 0

df_b <- length(samples) - 1
df_w <- sum(sapply(samples, length)) - length(samples)

x_g <- mean(a, b, c)

### Same sample sizes
ss_b <- sum((means - x_g)^2) * length(samples[[1]])
ss_w <- sum(sapply(samples, function(x) sum((x - mean(x))^2)))

### different samples sizes
# TODO - multiply each sum * length of the sample "n"
ss_b <- sum( (means - x_g)^2 * length(samples[[n]]) )
ss_w <- sum(sapply(samples, function(x) sum((x - mean(x))^2)))

ms_b <- ss_b / df_b
ms_w <- ss_w / df_w

f <- ms_b / ms_w

# f-critical value
qf(alpha_level, df_b, df_w)

##### Chi-squared
### Goodness-of-fit
# observed
o <- c(40, 60)
# expected probabilities
e <- c(.3, .7)

chisq.test(o, e)

### Independence
# a, b, and c are different groups
a
b
c

tbl <- cbind(a, b, c)
chisq.test(tbl)
cramersV(tbl)

##### Effect sizes

# cohen's d
d <- u_d / s

# cohen's d for multiple comparisons
(x_1 - x_2) / sqrt(ms_w)

# eta^2
ss_b / (ss_b + ss_w)

# r^2 - coefficient of determination
r_squared <- t^2 / (t^2 + df)

# r - correlation coefficient
r <- cov(x, y) / (sd(x) * sd(y))

### multiple comparison tests

## Tukey's HSD
# q-critical value

# then calculate
q * sqrt(ms_w / n)

##### Regression
b = r * (sd(y) / sd(x))

##### t-tests
### Dependent samples t-test
# variables
x_bar
u
s
n
df <- n - 1

# based on direction and alpha level
# t-critical value for one-tail
t_crit <- abs(qt(1 - alpha_level/2, df))
# two-tail
t_crit <- abs(qt(1 - alpha_level, df))

# hypothesis test
u_d <- x_bar - u

sem <- s / sqrt(n)

margin <- t_crit * sem
t <- (u_d - 0) / sem

# p-value - one-tailed
# **************not sure why lower.tail=FALSE is necessary
pt(t, df, lower.tail=FALSE)
# p-value - two-tailed
2*pt(t, df, lower.tail=FALSE)

### Independent samples t-test
# Similar to above steps, but there are two samples. So the following changes occur:
u_d <- x_bar_1 - x_bar_2
s_d <- sqrt(s_1^2 + s_2^2)

# df <- (n_1 - 1) + (n_2 - 1)
# can also use the smaller of n_1 and n_2
df <- n_1 + n_2 - 2

# if sample sizes are the same
sem <- s_d / sqrt(n)

# if they are different, pool the variances to 
# correct for that difference
s_p <- (ss_1 + ss_2) / (df_1 + df_2)
sem <- sqrt( (s_p / n_1) + (s_p / n_2) )

t <- ((x_bar_1 - x_bar_2) - (u_1 - u_2)) / sem

