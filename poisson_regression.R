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

## Q6.1a

```{r}
risky_behav <- read.dta("http://www.stat.columbia.edu/~gelman/arm/examples/risky.behavior/risky_behaviors.dta", convert.factors=TRUE)

risky_behav$fupacts <- round(risky_behav$fupacts)
risky_behav$couples <- factor(risky_behav$couples)
risky_behav$women_alone <- factor(risky_behav$women_alone)

head(risky_behav)
```

```{r}
mod.poi1 <- glm(fupacts ~ women_alone, family=poisson, data=risky_behav)

display(mod.poi1)

# Estimate the deviance 
deviance_residuals <- residuals(mod.poi1, type = "deviance")

# Calculate the Deviance-to-Df Ratio
deviance_df_ratio <- deviance(mod.poi1) / df.residual(mod.poi1)

print("---------------------------++++++--------------------------------")

# 3. Check for Overdispersion
if (deviance_df_ratio > 1) {
  cat("Overdispersion detected. Deviance-to-Df Ratio:", deviance_df_ratio, "\n")
}

plot(deviance_residuals)
```

They are indications of over dispersion in the model.

## Q6.1b

```{r}
mod.poi2 <- glm(fupacts ~ women_alone + sex + bupacts + couples + bs_hiv, family=poisson, data=risky_behav)

display(mod.poi2)
```

```{r}
# Estimate the deviance 
deviance_residuals_poi2 <- residuals(mod.poi2, type = "deviance")

# Calculate the Deviance-to-Df Ratio
deviance_df_ratio_poi2 <- deviance(mod.poi2) / df.residual(mod.poi2)

print("---------------------------++++++--------------------------------")

# 3. Check for Overdispersion
if (deviance_df_ratio_poi2 > 1) {
  cat("Overdispersion detected. Deviance-to-Df Ratio:", deviance_df_ratio_poi2, "\n")
}

plot(deviance_residuals_poi2)
```

The second model fit the data much better than the first model. However they exist some levels of overdispersion in the model but much less compared to the first model.

## Q6.1c

```{r}
mod.poi3 <- glm(fupacts ~ women_alone + sex + bupacts + couples + bs_hiv, family=quasipoisson, data=risky_behav)

display(mod.poi3)
```

From this model, we could conclude that there is an effect of the intervention in reducing the number of unprotected sex. For iinstance, in case were women alone received intervention, unprotected sex reduced by $e^{-0.66}$ = `r (exp(-0.66)-1)*100` %
