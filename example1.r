library(ggplot2)
library(ggrepel)

m = rnorm(n=1, mean=0, sd=1)
b = rnorm(n=1, mean=0, sd=100)

X <- 1:100
Y <- sapply(X, function(x){rnorm(n=1,mean=m*x + b,sd=5)})

fit <- lm(Y~X)
m <- fit$coefficients[2]
b <- fit$coefficients[1]

R <- fit$residuals

alpha = (1 - 0.95)/2
q = quantile(R, prob = c(alpha,1-alpha))
is.outlier = R < q[1] | R > q[2]
df <- data.frame(X, Y, R, is.outlier)

p <- ggplot(data = df, aes(X, Y, color = abs(R))) + 
     geom_abline(slope = m, intercept = b, color='dodgerblue4') +
     geom_point() + 
     geom_label_repel(data = subset(df,is.outlier), 
                      mapping = aes(X, Y, label = X), 
                      nudge_x = 1)
