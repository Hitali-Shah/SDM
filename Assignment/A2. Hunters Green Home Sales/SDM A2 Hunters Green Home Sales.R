#' SDM A2: Hunters Green Home Sales

library(readxl)
df <- read_excel("HuntersGreenHomeSales.xlsx", sheet='Data')
str(df)
colnames(df)=make.names(tolower(colnames(df)))

#' Feature engineering/Data Preprocessing

df$baths <- df$bathsfull + 0.5*df$bathshalf
df$specialsale <- ifelse(df$splsale=='None', 0, 1)
df$tileroof <- ifelse(df$roof=='Tile' | df$roof=='Slate' | df$roof=='Slate, Tile' |
                        df$roof=='Concrete, Tile', 1, 0)
df$privatepool <- ifelse(df$pool=='Private' | df$pool=='Private, Community', 1, 0)
df$communitypool <- ifelse(df$pool=='Community' | df$pool=='Private, Community', 1, 0)
df$year <- as.numeric(format(df$pendingdate,'%Y'))
View(df)

which(! complete.cases(df)) 
colSums(is.na(df))
# We have 311 NA values in spa; hence, it's better to avoid this variable
df <- df[, -c(1:3, 5:7, 10, 13:15, 17, 20, 22:24)]
str(df)

#' Data visualizations

hist(df$pricesold)
hist(df$adom)
hist(log(df$adom))

temp <- df[, c(1:10)]
library(PerformanceAnalytics)
install.packages("PerformanceAnalytics")
chart.Correlation(temp)

#' Regression models

price3 <- lm(pricesold ~ sqft + beds + baths + garages + tileroof + yrblt + 
              privatepool + communitypool + specialsale + year, data=df)
summary(price3)
price1 <- lm(pricesold ~ year, data=df)
price2 <- lm(pricesold ~ sqft + beds + baths + garages + tileroof + yrblt + 
              privatepool + communitypool + year, data=df)

library(stargazer)
stargazer(price1, price2, price3, type='text', single.row=TRUE)

adom1 <- lm(adom_agentdaysonmarket ~ year, data=df)
adom2 <- lm(adom_agentdaysonmarket ~ yrblt + privatepool + communitypool + year, data=df)
adom3 <- lm(adom_agentdaysonmarket ~ yrblt + privatepool + communitypool + specialsale + lppersqft + year, data=df)
stargazer(adom1, adom2, adom3, type='text', single.row=TRUE)

#' Test for assumptions



plot(price3)

plot(adom3)



shapiro.test(price3$res)                        # Shapiro-Wilk's test of multivariate normality
shapiro.test(adom3$res)

bartlett.test(list(price3$res, price3$fit))     # Bartlett's test of homoskedasticity
bartlett.test(list(adom3$res, adom3$fit))

library("car")                                  # Multicollinearity test
vif(price3)
vif(adom3)

library(lmtest)
dwtest(price3)                                  # Durbin-Watson test of autocorrelation
dwtest(adom3)



plot(adom3)
shapiro.test(adom3$res)
bartlett.test(list(adom3$res, adom3$fit))
vif(adom3)
dwtest(adom3)
