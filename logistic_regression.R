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


## Q5.5a

This question analyzes data about the gender healthindex and age of a certain group of person and if they have taken a flushot

```{r}
flushot <- read_csv("flushot.csv")

head(flushot)
```

Data discription

```{r}
flushot |>
  ggplot(aes(x = factor(flushot), y = age, fill = factor(flushot))) +
  geom_boxplot(alpha = .4) +
  scale_fill_viridis(option = "D", discrete = T) +
  theme_bw() +
  labs(
    title = "Distribution of flushot across each group",
    x = "Flushot",
    y = "age"
  ) +
  theme(
    legend.position = "none"
  )
```

## Q5.4b

```{r}
flulogitmod0 <- glm(flushot ~ 1, data = flushot, family = binomial(link = "logit"))

summary(flulogitmod0)
```

```{r}
flulogitmod1 <- glm(flushot ~ age, data = flushot, family = binomial(link = "logit"))

summary(flulogitmod1)
```

```{r}
flulogitmod2 <- glm(flushot ~ age + healthindex, data = flushot, family = binomial(link = "logit"))

summary(flulogitmod2)
```

```{r}
flulogitmod3 <- glm(flushot ~ age + healthindex + males, data = flushot, family = binomial(link = "logit"))

summary(flulogitmod3)
```

```{r}
flulogitmod4 <- glm(flushot ~ age + healthindex + males + I(age*males) +  I(healthindex*males), data = flushot, family = binomial(link = "logit"))

summary(flulogitmod4)
```

## Q5.4ci

I will be using the $flulogitmod3$
  
  The fitted model is $ln(\frac{\pi_i}{1-\pi_i})$ = $-6.867 + 0.121*age - 0.047*healthindex - 2.151*males + 0.046*(age*male) - 0.011*(healthindex*males)$
  
  Age: $e^{(0.145)}$ = `r exp(0.145)`$exp(0.121) - 1)*$$100)$ = `r (exp(0.145) - 1) * 100` this implies a one year increase in the health index is associated with a `r round((exp(0.145) - 1)*100,2)`% increase in the odds of males of a given health index receiving a flu shot
  
  Health index: $e^{(-0.053)}$ = `r exp(-0.053)` $exp(-0.053) - 1)*100)$ = `r (exp(-0.053) - 1) * 100` this implies a one unit increase in health index is associated with a `r round((exp(0.121) - 1)*100,2)`% decrease in the odds of males of a given age receiving a flu shot
  
  Male: $e^{(-0.344)}$ = `r exp(0.344)` $exp(0.344) - 1)*100)$ = `r (exp(0.344) - 1) * 100` this implies that the odds a male getting a flu shot `r round((exp(0.344) - 1)*100,2)`% higher than a female getting a flu shot given they are of the same age and have the same health index.
  
  ##Q5.4cii
  
  The error rate of the fitted model is:
    
    ```{r}
  pearsonresid <- augment(data = flushot, flulogitmod3, type.predict = "response",
                          type.residuals = "pearson")
  
  
  predicted_class <- factor(ifelse(pearsonresid$.fitted >= 0.5, 1, 0))
  
  true_labels <- factor(pearsonresid$flushot)
  
  confusion_matrix <- confusionMatrix(data = predicted_class, reference = true_labels)
  
  (Error_rate_true <- 1 - confusion_matrix$overall[[1]])
  
  ```
  
  The error rate of the null model is:
    
    ```{r}
  pearsonresid_null <- augment(data = flushot, flulogitmod0, type.predict = "response", type.residuals = "pearson")
  
  predicted_class_null <- factor(ifelse(pearsonresid_null$.fitted >= 0.5, 1, 0))
  
  true_labels_null <- factor(pearsonresid_null$flushot)
  
  confusion_matrix_null <- confusionMatrix(data = predicted_class_null, reference = true_labels_null)
  
  (Error_rate_true_null <- 1 - confusion_matrix_null$overall[[1]])
  ```
  
  ## Q5.4ciii
  
  There is a significant difference in the residual deviance of the null model $154.01$ and the fitted model $109.04$. They is a significant improvement in the quality of the fitted model as compared to the null model.
  
  ## Q5.4civ
  
  ```{r}
  #random dataset
  
  set.seed(1234)
  newdat <- data.frame(flushot = round(runif(50, 0, 1)),
                       age = round(runif(50, 48, 84)),
                       healthindex = round(runif(50, 20, 85)),
                       males = round(runif(50, 0,1))
  )
  ```
  
  ```{r}
  logit_pred <- predict(flulogitmod3, newdata=newdat, type="response")
  
  
  predicted_class_pred <- factor(ifelse(logit_pred >= 0.5, 1, 0))
  
  true_labels_pred <- factor(newdat$flushot)
  
  confusion_matrix_pred <- confusionMatrix(data = predicted_class_pred, reference = true_labels_pred)
  
  confusion_matrix_pred
  ```
  