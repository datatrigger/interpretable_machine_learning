# Packages
library(here)
library(readr)
library(tibble)
library(dplyr)
library(ggplot2)

# Import the training set
df <- read_csv(here('data_r/df_train.csv'))

# Compute the t-statistic of BMI in the full model
lm_full <- lm(charges~., data=df)
summary(lm_full)

# Or compute the F-statistic of the nested models
lm_no_bmi <- lm(charges~.-bmi, data=df)
anova(lm_no_bmi, lm_full)

# The nullity test for the BMI's coefficient are equivalent: t² = F (10.381² = 107.77)

# (Ab)Normality
qqplot <- ggplot(
  tibble(residuals = residuals(lm_full)),
  aes(sample = residuals)
  ) +
  stat_qq() +
  stat_qq_line() +
  xlab("Normal quantiles") +
  ylab("Residual quantiles") +
  ggtitle("Q-Q plot of the residuals for the full linear model") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Homoscedasticity
studentized_residuals <- rstudent(lm_full)

student_res_plot <- ggplot(
  data = df %>% select(charges) %>% mutate(rstudent=studentized_residuals)
  ) +
  geom_point(aes(x=charges, y = rstudent), size = 1) +
  geom_hline(yintercept=2, color = "red") +
  geom_hline(yintercept=-2, color = "red") +
  scale_y_continuous(breaks = seq(-5, 5), limits=c(-3, 5)) +
  xlab("Charges") +
  ylab("Studentized residuals") +
  ggtitle("Studentized residuals of the full linear model") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(qqplot)
print(student_res_plot)

# Percentage of studentized residuals greater than 2
sum(studentized_residuals>2)/length(studentized_residuals)

ggsave("plots_r/qq_plot.png", qqplot, dpi = "retina")
ggsave("plots_r/studentized_residuals.png", student_res_plot, dpi = "retina")
