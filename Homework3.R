#Question 2
library(brms)
library(tidyverse)
recommendations <- read_csv("Desktop/Homework 3-20240229/data/recommendations.csv")
head(recommendations)
summary(recommendations)

#Bayesian linear model
model <- brm(RecommendationFollowed ~ Mode, data = recommendations, family = bernoulli(), chains = 4, cores = 4)
summary(model)
plot(model)

#By fitting this Bayesian linear model, we can investigate the affect of recommendation mode on whether or not a recommendation is followed while considering uncertainty in parameter estimates.
#from the plots we can see that there is evidence suggesting that the mode of recommendation has an effect on recommendation follow-up.
#the probability of a recommendation being followed is higher in auditory mode than visual mode as the estimated effect when the two are campared is -0.49.

#Modality
summary_stats <- recommendations %>%
group_by(Mode) %>%
summarize(mean_competence = mean(Competent), 
mean_intelligence = mean(Intelligent),
mean_thoughtfulness = mean(Thoughtful))

ggplot(recommendations, aes(x = Mode, y = Competent)) + geom_boxplot() +
labs(title = "Distribution of Competence Ratings by Modality")
ggplot(recommendations, aes(x = Mode, y = Intelligent)) + geom_boxplot() +
labs(title = "Distribution of Intelligence Ratings by Modality")
ggplot(recommendations, aes(x = Mode, y = Thoughtful)) +geom_boxplot() +
labs(title = "Distribution of Thoughtfulness Ratings by Modality")

#Based on the summary stats, we can see that the three factors (competence, intelligence, and thoughtfulness) have higher mean values compared to auditory modality.
#the above can also be observed in the boxplots.
#these findings suggest that people find visual recommendations to be more competent, thoughtful and intelligent when compared to audio recommendation.
#these findings do contradict our previous conclusion that audio recommendations were more likely to be followed up.

#Question 3
library(tibble)
library(brms)

student_data <- tibble( student_id = 1:20,
marks = c(72, 68, 44, 73, 63, 56, 80, 43, 65, 75, 15, 58, 84, 70, 78, 55, 62, 67, 48, 82))
priors <- c(prior(normal(), class = "Intercept"), prior(exponential(), class = "sigma"))
model <- brm(marks ~ student_id, data = student_data, prior = priors)
summary(model)

