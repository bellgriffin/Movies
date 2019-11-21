

#-------------------------------------------------------------------------------

# Data Cleaning
#-------------------------------------------------------------------------------
movies <- read_csv('Final Project/movies.csv')

movies %<>% 
  filter(
    genre %in% c(
      'Action', 'Adventure', 'Animation', 'Biography',
      'Comedy', 'Crime', 'Drama', 'Horror'
      ),
    rating %in% c('G', 'NOT RATED', 'PG', 'PG-13', 'R', 'UNRATED')
  )

# View how NA budgets are distributed across the companies
budget_by_company <- movies %>% 
  mutate(
    budget_na = ifelse(budget == 0, 'Empty', 'Full')
  ) %>% 
  group_by(company) %>% 
  count(budget_na) %>%
  pivot_wider(values_from = n, names_from = budget_na) %>% 
  mutate(
    percent_na = sum(Empty, na.rm = TRUE) / sum(Empty, Full, na.rm = TRUE)
  )

budget_filled <- movies %>% 
  filter(budget != 0) %>%
  pull(budget)

budget_dist <- density(budget_filled)

movies <- movies %>% 
  rowwise() %>% 
  mutate(
    budget = ifelse(
      budget == 0,
      sample(budget_filled, 1) + rnorm(1, 0, budget_dist$bw),
      budget
    )
  )

company_counts <- movies %>%
  count(company) %>% 
  mutate(log_company_count = log(n)) %>% 
  select(company, log_company_count)

country_counts <- movies %>% 
  count(country) %>% 
  mutate(log_country_count = log(n)) %>% 
  select(country, log_country_count)
  
movies %<>% 
  left_join(company_counts, by = 'company') %>% 
  left_join(country_counts, by = 'country') %>% 
  mutate(
    log_budget = log(budget),
    log_gross = log(gross),
    log_votes = log(votes)
  )

#-------------------------------------------------------------------------------

# Descriptive Vizualizations
#-------------------------------------------------------------------------------
pairs(select_if(movies, is.numeric))
cor(select_if(movies, is.numeric))

ggplot(filter(movies, rating %in% c('R', 'PG-13', 'PG', 'G')), aes(log(gross), color = rating)) +
  geom_density()

ggplot(filter(movies, genre %in% c('Comedy', 'Drama', 'Action', 'Crime', 'Adventure')), aes(log(gross), color = genre)) +
  geom_density()


#-------------------------------------------------------------------------------

# Model and Summary
#-------------------------------------------------------------------------------
model <- lm(
  log_gross ~ log_budget + log_company_count + log_country_count + genre +
    rating + year + runtime + score + log_votes, movies
)

movies <- movies[-unique(model$na.action),]
summary(model)
movies$residuals <- model$residuals
movies$fitted_values <- model$fitted.values

avPlots(model)

ggplot(movies, aes(log_budget, residuals)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  theme(aspect.ratio = 1)
ggplot(movies, aes(score, residuals)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  theme(aspect.ratio = 1)
ggplot(movies, aes(log_votes, residuals)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  theme(aspect.ratio = 1)
ggplot(movies, aes(log_company_count, residuals)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  theme(aspect.ratio = 1)
ggplot(movies, aes(log_country_count, residuals)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  theme(aspect.ratio = 1)

ggplot(movies, aes(log_budget, fitted_values)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  theme(aspect.ratio = 1)
ggplot(movies, aes(score, fitted_values)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  theme(aspect.ratio = 1)
ggplot(movies, aes(log_votes, fitted_values)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  theme(aspect.ratio = 1)
ggplot(movies, aes(log_company_count, fitted_values)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  theme(aspect.ratio = 1)
ggplot(movies, aes(log_country_count, fitted_values)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  theme(aspect.ratio = 1)


par(pty = 's')
qqnorm(movies$residuals)
qqline(movies$residuals)

par(pty = 's')
hist(movies$residuals, freq = F, breaks = 30)
curve(dnorm(x, mean = 0, sd = sd(movies$residuals)), add = TRUE)

grp <- as.factor(c(rep("lower", floor(dim(movies)[1] / 2)), 
                   rep("upper", ceiling(dim(movies)[1] / 2))))
leveneTest(movies$residuals ~ grp, center = median)

### DFBETAS ###
model.dfbetas <- as.data.frame(dfbetas(model))
model.dfbetas$obs <- 1:length(movies$log_gross)

ggplot(data = model.dfbetas) +
  geom_point(mapping = aes(x = obs, y = abs(log_budget))) +
  geom_hline(mapping = aes(yintercept = 2 / sqrt(length(obs))),
             color = "red", linetype = "dashed") +
  theme_bw() +
  theme(aspect.ratio = 1)

ggplot(data = model.dfbetas) +
  geom_point(mapping = aes(x = obs, y = abs(log_votes))) +
  geom_hline(mapping = aes(yintercept = 2 / sqrt(length(obs))),
             color = "red", linetype = "dashed") +
  theme_bw() +
  theme(aspect.ratio = 1)

ggplot(data = model.dfbetas) +
  geom_point(mapping = aes(x = obs, y = abs(log_country_count))) +
  geom_hline(mapping = aes(yintercept = 2 / sqrt(length(obs))),
             color = "red", linetype = "dashed") +
  theme_bw() +
  theme(aspect.ratio = 1)

ggplot(data = model.dfbetas) +
  geom_point(mapping = aes(x = obs, y = abs(log_company_count))) +
  geom_hline(mapping = aes(yintercept = 2 / sqrt(length(obs))),
             color = "red", linetype = "dashed") +
  theme_bw() +
  theme(aspect.ratio = 1)

ggplot(data = model.dfbetas) +
  geom_point(mapping = aes(x = obs, y = abs(score))) +
  geom_hline(mapping = aes(yintercept = 2 / sqrt(length(obs))),
             color = "red", linetype = "dashed") +
  theme_bw() +
  theme(aspect.ratio = 1)


### DFFITS ###
model.dffits <- data.frame("dffits" = dffits(model))
model.dffits$obs <- 1:length(movies$log_gross)

ggplot(data = model.dffits) +
  geom_point(mapping = aes(x = obs, y = abs(dffits))) +
  geom_hline(mapping = aes(yintercept = 2 * sqrt(6 / length(obs))),
             color = "red", linetype = "dashed") +  
  theme_bw() +
  theme(aspect.ratio = 1)

vif(model)
