{r echo=TRUE}
library(tidyverse)
library(broom)
library(foreign)
library(GGally)
library(viridis)
library(caret)
library(ROCR)
library(arm)
library(AER)
library(fastDummies)


## Q3.4a


df3 <- read.dta("child.iq.dta")

head(df3)



mod.fit3 <- lm(ppvt ~ momage, data = df3)

out3 <- tidy(mod.fit3)
out3


**Fitted regression mode:** $\hat{childtest}$ = `r round(out3$estimate[1],3)` + `r round(out3$estimate[2],3)` $*momage$
  
  
broom::glance(mod.fit3)



aug <- augment(mod.fit3, data = df3)
head(aug)



#plot of ppvt vs momage variable for mod1
ggplot(df3, aes(x = momage, y = ppvt)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(
    title = "Plot1: Plot of child test score vs momage",
    x = "Mom age",
    y = "Child test score"
  ) +
  theme_bw()

#plot of residual vs predictor variable for mod1
ggplot(aug) +
  geom_point(aes(
    x = momage,
    y = .resid
  )) +
  labs(
    title = "Plot2: Plot of Residuals vs momage",
    x = "Mom age",
    y = "Residuals"
  ) +
  theme_bw()


#plot of residual vs fitted values for mod1
ggplot(aug) +
  geom_point(aes(
    x = .fitted,
    y = .resid
  )) +
  labs(
    title = "Plot3: Plot of Residuals vs fits for child test score vs momage",
    x = "Fitted values of child test score",
    y = "Residuals"
  ) +
  theme_bw()


#qqplot of residuals for mod1
ggplot(aug, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = I('blue')) +
  labs(
    title = "Plot4: Normal plot of residuals") +
  theme_bw()


Plot 1 shows that assumption of linearity seemed somewhat valid although the tails are not perfectly normal. Based upon plot 2 and 3 the assumption of independence of error is fairly valid, because the residuals are centered around zero, albeit a few outliers. The assumption of constant variance is also valid as the they are no systematic difference to the distribution of the residuals in plot 1 and 2.

**Slope coefficient:** $\hat{B_1}$ = $momage$ shows that a 1year increase in the mothers age will result in a `round(out3$estimate[2],3)` increase in the child's test score on average.

**Recommendation:** Based upon plot 1, I would recommend mothers should give birth in their late 20s. However, mother's age alone can't conclusively predict child's test score. Other factors not included in the model could also be fundamental in predicting a child's test score. But based on the estimate of this model alone we could conclude mother's (\>= 20years) is a good predictor of child's test score.

## Q3.4b


mod.fit4 <- lm(ppvt ~ momage + educ_cat, data = df3)

out4 <- tidy(mod.fit4)
out4


$Momage$ = (`round(out4$estimate[2],3)` ). This shows that on average, a 1year increase in mother's age will result in a `round(out4$estimate[2],3)` increase in child's test score

$Educ$\_${cat}$ = (`round(out4$estimate[3],3)`). This shows that changes in educational category will result in an `round(out4$estimate[3],3)` increase in child's test score on average.


glance(mod.fit4)



aug2 <- augment(mod.fit4, data = df3)
head(aug2)



#plot of residual vs predictor variable for mod1
ggplot(aug2) +
  geom_point(aes(
    x = momage,
    y = .resid
  )) +
  labs(
    title = "Plot2: Plot of Residuals vs momage",
    x = "Mom age",
    y = "Residuals"
  ) +
  theme_bw()


#plot of residual vs fitted values for mod1
ggplot(aug2) +
  geom_point(aes(
    x = .fitted,
    y = .resid
  )) +
  labs(
    title = "Plot3: Plot of Residuals vs fits for child test score vs momage",
    x = "Fitted values of child test score",
    y = "Residuals"
  ) +
  theme_bw()


#qqplot of residuals for mod1
ggplot(aug2, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = I('blue')) +
  labs(
    title = "Plot4: Normal plot of residuals") +
  theme_bw()


colours = c('#025464','#E57C23','#E8AA42','#F8F1A4')
plot(df3$momage, df3$ppvt, xlab="Mother age", ylab="Child test score", col=colours, pch=20)
for (i in 1:4) {
  curve(cbind(1, x, i) %*% coef(mod.fit4), add=TRUE, col=colours[i])
}



The addition of `educ_cat` the model has an improved amount explained variability in child's age from (1% to 4%). This new model shows a slight improvement over the old model although the `momage` is not statistically significant the coefficient of did reduce from 0.84 to 0.34, while experiencing an increase in its standard error from 0.379 to 0.398. The model diagnostics is fairly the same, I would recommend no changes to my initial conclusion that mothers should give birth in their late 20s.

## Q3.4c


new1_df3 <- df3 |>
  mutate(
    educ_class = case_when(
      educ_cat == 1 ~ "No high school",
      between(educ_cat, 2, 4) ~ "High school and above"
    )
  )

new_df3 <- dummy_cols(.data = new1_df3, select_columns = "educ_class") |>
  rename(
    "High_school_above" = "educ_class_High school and above",
    "No_high_school" = "educ_class_No high school"
  )

head(new_df3)



mod.fit5 <- lm(ppvt ~ momage + High_school_above + I(momage*High_school_above), data = new_df3)

out5 <- tidy(mod.fit5)
out5



glance(mod.fit5)



ggplot(new_df3, aes(x = momage, y = ppvt, 
                    col = educ_class)) +
  geom_smooth(method = "lm", se = F) + 
  geom_point(aes(alpha = educ_class)) +
  scale_x_continuous(breaks = new_df3$momage) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(title = "Scatterplot with Regression Line",
       x = "Mother's age",
       y = "Child test score")


## Q3.4d


sample_1 <- new_df3 |>
  slice_head(n = 200)

sample_2 <- new_df3 |>
  slice(201:400)



mod.fit6 <- lm(ppvt ~ momage + educ_cat, data = sample_1)

out6 <- tidy(mod.fit6)
out6



glance(mod.fit6)


#### Prediction


predictions <- as.data.frame(x = predict(mod.fit6, sample_2)) |>
  rename(
    "pred" = "predict(mod.fit6, sample_2)"
  ) |> cbind(sample_1)

head(predictions)



ggplot(predictions,aes(x = pred)) +
  geom_point(aes(y = ppvt), shape = 21, size = 2.5) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "Predicted", y = "Observed") +
  theme_bw()


## Q4.4a


pollution <- read.dta("http://www.stat.columbia.edu/~gelman/arm/examples/pollution/pollution.dta")

head(pollution)



pollution |>
  ggplot(aes(x = nox, y = mort)) +
  geom_point(alpha = 1, color = "darkorange") +
  theme_bw() +
  labs(
    x = "Nitric Oxide",
    y = "Mortality rate",
    title = "Scatter plot between Mortality and Nitric Oxide"
  )


The plot does quiet illustrate that a linear regression will fit these data well.


mod.fit10 <- lm(mort ~ nox, data = pollution)

out10 <- tidy(mod.fit10)
out10



glance(mod.fit10)



aug6 <- augment(mod.fit10, pollution)

#plot of residual vs fitted values for mod1
ggplot(aug6) +
  geom_point(aes(
    x = .fitted,
    y = .resid
  )) +
  labs(
    title = "Plot1: Plot of Residuals vs fits",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_bw()


#qqplot of residuals for mod1
ggplot(aug6, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = I('blue')) +
  labs(
    title = "Plot2: Normal plot of residuals") +
  theme_bw()

pollution |>
  ggplot(aes(x = nox, y = mort)) +
  geom_point(alpha = 1, color = "darkorange") +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  labs(
    x = "Nitric Oxide",
    y = "Mortality rate",
    title = "Scatter plot between Mortality and Nitric Oxide"
  )


The plot of residuals vs fit shows that variance are not constant as this is quiet different as well from the scatter plot of mortality rate and nitric oxide. There is no distinct pattern in the distribution of the residuals. However, the assumption of independence of errors seemed fairly valid as majority of the residuals are centered around zero. The assumption of normality of residuals appears fairly valid as well.

## 4.4b


mod.fit11 <- lm(log(mort) ~ log(nox), data = pollution)
out11 <- tidy(mod.fit11)
out11



glance(mod.fit11)



aug7 <- augment(mod.fit11, pollution)

#plot of residual vs fitted values for mod1
ggplot(aug7) +
  geom_point(aes(
    x = .fitted,
    y = .resid
  )) +
  labs(
    title = "Plot1: Plot of Residuals vs fits",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_bw()


#qqplot of residuals for mod1
ggplot(aug7, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = I('blue')) +
  labs(
    title = "Plot2: Normal plot of residuals") +
  theme_bw()

pollution |>
  ggplot(aes(x = log(nox), y = log(mort))) +
  geom_point(alpha = 1, color = "darkorange") +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  labs(
    x = "Nitric Oxide",
    y = "Mortality rate",
    title = "Scatter plot between Mortality and Nitric Oxide"
  )


The log transformation, significantly improved the performance of this model. They was an increase in the amount of variability explained in the model from 1% to 9% after log transformation. Similarly the distribution of residual were consistent with the assumption of constant variance and independence of errors.

## Q4.4c

Slope(Nitric Oxide) = A 1% increase in Nitric oxide is associated with a 1% average change in mortality rate.

## Q4.4d


pollution |>
  ggpairs(columns = c("mort","nox", "so2", "hc"))



mod.fit12 <- lm(log(mort) ~ log(nox) + log(so2) + log(hc), data = pollution)
out12 <- tidy(mod.fit12)
out12



glance(mod.fit12)



aug8 <- augment(mod.fit12, pollution)

#plot of residual vs fitted values for mod1
ggplot(aug8) +
  geom_point(aes(
    x = .fitted,
    y = .resid
  )) +
  labs(
    title = "Plot1: Plot of Residuals vs fits",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_bw()


#qqplot of residuals for mod1
ggplot(aug8, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = I('blue')) +
  labs(
    title = "Plot2: Normal plot of residuals") +
  theme_bw()


$log(nox) Nitric Oxide$ : This implies that a 1% increase in Nitric oxide is associated with a 6% average increase in mortality rate. While a 1% increase in $Sulfur oxide$ is associated with a 1% increase in mortality rate on average. Meanwhile, a 1% increase in $hydrocarbons$, is associated with a 6% decrease in mortality rate on average. The $intercept$ on the hand implies that the average mortality rate for an individual not exposed to nitric oxide, sulfur oxide or hydrocarbons is 6.8 on average.

## Q4.4e



train <- pollution |>
  slice(1:30)

test <- pollution |>
  slice(31:60)


mod.fit13 <- lm(log(mort) ~ log(nox) + log(so2) + log(hc), data = train)

pred <- predict(mod.fit13, test)
outpred <- data.frame(pred = exp(pred), observed=test$mort)

outpred |>
  ggplot(aes(x = observed, y = pred)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  labs(
    title = "Scatter plot of testdata vs predictions",
    x = "observed",
    y = "predictions"
  )

