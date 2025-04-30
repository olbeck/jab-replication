### Code for JaB for linear regression

library(dplyr)
library(ggplot2)
library(JaB)
### JaB for Regression ----------------------------------------
c( "#9c179e", "#22a884", "#f0f921", "#fca636" ,"#414487", "#e16462"  )
set.seed(540)
n <- 20
p <- 2

#Y = 1 + 2X + e
beta_0 <- 1
beta_1 <- 2

X <- rnorm(n, 2, 1)
e_norm <- rnorm(n, 0, sd = 0.5625^(1/2))
Y <- beta_0 + beta_1*X + e_norm

dat_orig <- data.frame(X, Y)

plot.orig <-
  ggplot() +
  geom_point(data = dat_orig, aes(x=X,y=Y), color = "blue") +
  geom_smooth(data = dat_orig, aes(x=X,y=Y), method='lm', formula= y~x, se = F, color = "blue") +
  theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"))
plot.orig +
  theme_minimal() +
  xlim(-0.25, 4.5) +
  ylim(0, 10)


n <- n+1
Y1 <- c(Y, 5)
X <- c(X, 3.5)

dat_new <-  data.frame(X,Y1)

plot.orig +
  geom_point(data = data.frame(x=3.5, y=5), aes(x=x,y=y), color = "#e16462")  +
  geom_smooth(data = dat_new, aes(x=X,y=Y1), method='lm', formula= y~x, se = F, color = "#e16462") +
  theme_minimal()+
  ylab("Y")+
  xlim(-0.25, 4.5) +
  ylim(0, 10)


mod0 <- lm(Y~X, data=dat_orig)
summary(mod0)
confint(mod0)

mod1 <- lm(Y1 ~ X, data = dat_new)
summary(mod1)
confint(mod1)

dffits(mod1 )
abs(dffits(mod1)) > 2 * sqrt(3/16)
result1 <- jab_lm(mod1,
                  stat = "dffits",
                  quant.lower = 0.04,
                  quant.upper = 0.96,
                  B=3100)



result1 %>%
  filter(row.ID != 21) %>%
  summarise(lower.avg = mean(lower),
            upper.avg = mean(upper),
            mean.dffits = mean(orig))

result1 %>%
  ggplot() +
  geom_segment(aes(x = row.ID, y = lower, yend = upper), color = "black") +
  geom_point(aes(x = row.ID, y = orig, color = influential), size = 3) +
  # ggtitle("Result of JaB Algorithm") +
  xlab("Index") +
  ylab("DFFITS") +
  theme_minimal()+
  scale_color_manual(name = "Flagged as \nInfluential",
                     values = c( "TRUE" =  "#22a884", "FALSE" = "#414487"),
                     breaks = c(TRUE, FALSE),
                     labels = c("True", "False")) +
  theme(axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black")) +
  ylim(-2, 1)



dat_new$dffits <- dffits(mod1)
dat_new$dffits.inf <- abs(dat_new$dffits) >2*sqrt(2/21)
dat_new$ID <- 1:21
dat_new %>%
  ggplot() +
  geom_point(aes(x=ID, y = dffits, color = dffits.inf), size = 3, ) +
  scale_color_manual(name = "Flagged as \nInfluential",
                     values = c( "TRUE" =  "#22a884", "FALSE" = "#414487"),
                     breaks = c(TRUE, FALSE),
                     labels = c("True", "False"))+
  geom_hline(yintercept = 2*sqrt(2/21), col = "black")+
  geom_hline(yintercept = -2*sqrt(2/21), col = "black") +
  ylab("DFFITS") +
  xlab("Index")+
  theme_minimal() +
  ylim(-2, 1)



mod2 <- lm(Y1 ~ X, data = dat_new)
summary(mod2)



#### bootstrap Example

B <- 3100
Xboot <- matrix(sample(1:n, n*B, replace = TRUE), nrow = B, ncol = n)
Gboot <- matrix(NA, nrow=B, ncol = n)
for(b in 1:B){

  x.temp <- X[Xboot[b, ]]
  y.temp <- Y1[Xboot[b, ]]

  mod.temp <- lm(y.temp ~ x.temp)
  Gboot[b, ] <- dffits(mod.temp)

}

plot(x.temp, y.temp, pch = 19)

data.frame(x.temp, y.temp) %>%
  ggplot(aes(x.temp, y.temp))+
  geom_jitter() +
  geom_smooth(method='lm', formula= y~x, se = F, col = "black") +
  theme_minimal()+
  theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) +
  ylim(0, 10) +
  xlim(0, 4.25)

has.21 <- apply(Xboot, 1, function(x) any(x == 21))

X21 <- Xboot[!has.21, ]
Gamma21 <- Gboot[!has.21, ]
Gamma <- c(Gamma21)


data.frame(Gamma) %>%
  ggplot() +
  geom_histogram(aes(x = Gamma, y = ..density..), bins = 28)+
  theme_minimal() +
  theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) +
  ylab("Density")+
  xlim(c(-2,2))+
  geom_vline(xintercept = quantile(Gamma, 0.04), col = "red", size = 3) +
  geom_vline(xintercept = quantile(Gamma, 0.96), col = "red", size = 3) +
  geom_vline(xintercept = dat_new$dffits[21], col = "blue", size = 3)





############

