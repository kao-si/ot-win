
library(tidyverse)
library(fixest)
library(broom)
library(modelsummary)
library(modelr)

## Load data ====

dat <- read_rds("Data_HP.rds")

# Transform variable 'hwin' to a numeric variable 'nhwin' (for feglm)
dat <- dat %>% mutate(nhwin = case_when(hwin == "Host Won" ~ 1,
                                        hwin == "Host Lost" ~ 0))

## Model-free evidence ====

mod0 <- glm(hwin ~ hforced, family = binomial, dat)

tidy(mod0)

# Graphic evidence
dat_count <- dat %>%
  group_by(hforced, hwin) %>%
  summarize(count = n()) %>%
  ungroup(hwin) %>%
  mutate(percentage = count / sum(count) * 100)

dat_count %>%
  ggplot(aes(hforced, count, fill = hwin)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage), "%")),
            position = position_dodge(width = 0.9), vjust = -1) +
  ylim(0, 500) +
  scale_fill_brewer(palette = "Paired")

## Number of ties in Q4 ====

mod1_1 <- feglm(nhwin ~ hforced * tie4q, family = binomial, dat)
mod1_2 <- feglm(nhwin ~ hforced * tie4q | host, family = binomial, dat)
mod1_3 <- feglm(nhwin ~ hforced * tie4q | host + visitor,
family = binomial, dat)
mod1_4 <- feglm(nhwin ~ hforced * tie4q | host + visitor + season,
family = binomial, dat)

modelsummary(
    list(mod1_1, mod1_2, mod1_3, mod1_4),
    stars = TRUE,
    gof_omit = "R2 Adj.|R2 Within|R2 Within Adj.|AIC|BIC|RMSE",
    coef_map = c(
        "(Intercept)" = "Constant",
        "hforcedHost Forced to OT1" = "Host Was Forced to OT1",
        "tie4q" = "Number of Ties in Q4",
        "hforcedHost Forced to OT1:tie4q" = "Interaction Effect"
    ),
    statistic = c("({std.error})", "p = {p.value}"),
    output = "markdown"
)

# Interaction plot
dat <- add_predictions(dat, mod1_4, var = "pred1")

dat %>% ggplot() +
geom_smooth(
    method = "glm",
    aes(tie4q, pred1, color = hforced),
    method.args = list(family = "binomial"),
    se = FALSE
) +
labs(
    y = "Host Winning Probability",
    x = "Number of Ties in Q4",
    color = NULL
) +
scale_x_continuous(limits = c(1, 11), breaks = seq(1, 11, by = 1)) +
ggpubr::theme_pubr()

## Number of lead exchanges in Q4 ====

mod2_1 <- feglm(nhwin ~ hforced * ledchg4q, family = binomial, dat)
mod2_2 <- feglm(nhwin ~ hforced * ledchg4q | host, family = binomial, dat)
mod2_3 <- feglm(nhwin ~ hforced * ledchg4q | host + visitor,
family = binomial, dat)
mod2_4 <- feglm(nhwin ~ hforced * ledchg4q | host + visitor + season,
family = binomial, dat)

modelsummary(
    list(mod2_1, mod2_2, mod2_3, mod2_4),
    stars = TRUE,
    gof_omit = "R2 Adj.|R2 Within|R2 Within Adj.|AIC|BIC|RMSE",
    coef_map = c(
        "(Intercept)" = "Constant",
        "hforcedHost Forced to OT1" = "Host Was Forced to OT1",
        "ledchg4q" = "Number of Lead Exchanges in Q4",
        "hforcedHost Forced to OT1:ledchg4q" = "Interaction Effect"
    ),
    statistic = c("({std.error})", "p = {p.value}"),
    output = "markdown"
)

## Score Difference when Forced Team Last Scored in Q4 ====

mod3_1 <- feglm(nhwin ~ hforced * flsdif, family = binomial, dat)
mod3_2 <- feglm(nhwin ~ hforced * flsdif | host, family = binomial, dat)
mod3_3 <- feglm(nhwin ~ hforced * flsdif | host + visitor,
family = binomial, dat)
mod3_4 <- feglm(nhwin ~ hforced * flsdif | host + visitor + season,
family = binomial, dat)

modelsummary(
    list(mod3_1, mod3_2, mod3_3, mod3_4),
    stars = TRUE,
    gof_omit = "R2 Adj.|R2 Within|R2 Within Adj.|AIC|BIC|RMSE",
    coef_map = c(
        "(Intercept)" = "Constant",
        "hforcedHost Forced to OT1" = "Host Was Forced to OT1",
        "flsdif" = "Score Difference when Forced Team Last Scored in Q4",
        "hforcedHost Forced to OT1:flsdif" = "Interaction Effect"
    ),
    statistic = c("({std.error})", "p = {p.value}"),
    output = "markdown"
)

# Interaction plot
dat <- add_predictions(dat, mod3_4, var = "pred3")

dat %>% ggplot() +
geom_smooth(
    method = "glm",
    aes(flsdif, pred3, color = hforced),
    method.args = list(family = "binomial"),
    se = FALSE
) +
labs(
    y = "Host Winning Probability",
    x = "Score Difference when Forced Team Last Scored in Q4",
    color = NULL
) +
scale_x_continuous(limits = c(1, 16), breaks = seq(1, 16, by = 1)) +
ggpubr::theme_pubr()