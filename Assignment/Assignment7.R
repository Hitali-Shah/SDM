#Pre-Processing
library(rio)
maindata = import("Assignment/Assignment 7/6304 Module 8 Assignment Data Set.xlsx")
set.seed(02273874)
glow = maindata[sample(1:nrow(maindata),150,replace = FALSE),]
colnames(glow) = tolower(make.names(colnames(glow)))
glow$fracture = as.factor(glow$fracture)
glow$priorfrac = as.factor(glow$priorfrac)
glow$menoby45 = as.factor(glow$menoby45)
glow$momfrac = as.factor(glow$momfrac)
glow$armassist = as.factor(glow$armassist)
attach(glow)
glow.out = glm(fracture~.-case, data = glow, family = "binomial")
summary(glow.out)
#Original Error
pred.values = expand.grid(
  priorfrac = unique(glow$priorfrac), age = quantile(glow$age, c(.25,.5,.75,1)),
  weight = quantile(glow$weight, c(.25,.5,.75,1)) , height = quantile(glow$height, c(.25,.5,.75,1)),
  bmi = quantile(glow$bmi, c(.25,.5,.75,1)), menoby45 = unique(glow$menoby45),
  momfrac = unique(glow$momfrac),armassist = unique(glow$armassist)
)
pred.prob = round(predict(glow.out,newdata = pred.values,type ="response"),3)
#Detach the dataset to find the correct error
detach(glow)
pred.values = expand.grid(
  priorfrac = unique(glow$priorfrac), age = quantile(glow$age, c(.25,.5,.75,1)),
  weight = quantile(glow$weight, c(.25,.5,.75,1)) , height = quantile(glow$height, c(.25,.5,.75,1)),
  bmi = quantile(glow$bmi, c(.25,.5,.75,1)), menoby45 = unique(glow$menoby45),
  momfrac = unique(glow$momfrac),armassist = unique(glow$armassist)
)
pred.prob = round(predict(glow.out,newdata = pred.values,type ="response"),3)
#After adding the case column no error
pred.values$case = seq(1,nrow(pred.values),1)
pred.prob = round(predict(glow.out,newdata = pred.values,type ="response"),3)

