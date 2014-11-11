Statistics

```{r}
# Install packages to run statistical analysis

# install.packages("car")
require("car")

plot(dataset$Patents,dataset$Followers)
plot(dataset[,3,7])


car::scatterplotMatrix(dataset[,3:7])

M1 <- lm(Patents ~ Followers + GDP + Population, data = dataset)
summary(M1)

confint(M1)

plot(M1, which = 1)


#Attempt to break follower numbers into categorical data
dataset$Followers_cat <- cut(dataset$Followers,
                breaks = c(25, 49, 74, 99, 124, 149, 174, 199),
                labels = c('< 50', '< 75', '< 100', '< 125', '< 150','< 175', '>= 200'))
summary(dataset$Followers_cat)

M3 <- lm(Patents ~ Followers_cat, data = dataset)
confint(M3)

M5 <- lm(log(Patents) ~ Followers + log(GDP) + log(Population),
data = dataset)
summary(M5)

M6 <- lm(Patents ~ Followers * GDP * Population,
data = dataset)
summary(M6)
plot(M6, which = 1)
plot(M6, which = 2)
```

```{r, echo=TRUE, results='asis'}

# Create different models for stargazer regression output

P1 <- lm(Patents ~ Followers, data = dataset2)
P2 <- lm(Patents ~ Followers + GDP, data = dataset2)
P3 <- lm(Patents ~ Followers + GDP + Population, data = dataset2)

F1 <- lm(Followers ~ Patents, data = dataset2)
F2 <- lm(Followers ~ Patents + GDP, data = dataset2)
F3 <- lm(Followers ~ Patents + GDP + Population, data = dataset2)

labels <- c('(Intercept)', 'Followers', 'GDP per capita', 'Population')

# For PDF output use this code

stargazer::stargazer(P1, P2, P3, covariate.labels = labels,
  title = 'Regression Estimates of Patent Activity',
  digits = 2, type ='latex', header=FALSE)

# For HTML output use this code  

#stargazer::stargazer(M1, M2, M3, covariate.labels = labels,
 # title = 'Regression Estimates of Patent Activity',
 # digits = 2, type ='html')
```
