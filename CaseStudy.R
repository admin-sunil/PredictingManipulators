library(ISLR)
library(ROSE)
library(readxl)
install.packages("arulesViz")
library(arulesViz)
manipulators <- read_excel("C:/Users/SUNIL/Desktop/IDS-572/Case Study/casestudy.xlsx", sheet=2)
non_manipulators <- read_excel("C:/Users/SUNIL/Desktop/IDS-572/Case Study/casestudy.xlsx", sheet=3)
complete_data <- read_excel("C:/Users/SUNIL/Desktop/IDS-572/Case Study/casestudy.xlsx", sheet=4)
sample_data <- read_excel("C:/Users/SUNIL/Desktop/IDS-572/Case Study/casestudy.xlsx", sheet=5)

summary(manipulators)
par(mar=c(1,1,1,1))
plot(manipulators$DSRI, main='distribution', col='red')
plot(manipulators$GMI)
plot(manipulators$AQI)
plot(manipulators$SGI)
plot(manipulators$DEPI)
plot(manipulators$SGAI)
plot(manipulators$ACCR)
plot(manipulators$LEVI)

benish_model <- glm(manipulators$MANIPULATOR ~ manipulators$DSRI+manipulators$GMI+manipulators$AQI+
                      manipulators$SGI+manipulators$DEPI+manipulators$SGAI+manipulators$ACCR+manipulators$LEVI
                      , data = sample_data, family = "binomial")
summary(benish_model)
plot(benish_model)

b_model <- step(object=benish_model, trace=0)
summary(b_model)

anova(b_model ,test="Chisq")
coefficients(b_model)
complete_data
complete_data$Manipulater <- as.factor(complete_data$Manipulater)
summary(complete_data)


# (4) What measure do you use to evaluate the performance of your logistic regression model? How
# does your model perform on the training and test datasets?


Balanced_data <- ovun.sample(Manipulater ~ DSRI +GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI, data= complete_data, method = "over", N=2500)$data
summary(Balanced_data$Manipulater)




# (5) What is the best probability threshold that can be used to assign instances to different classes?
# Write two functions that receive the output of the ROC performance function and return the
# best probability thresholds using the distance to (0,1) and Youden’s approach respectively.

library(pROC)
indx <- sample(2, nrow(Balanced_data) , replace = T, prob = c(0.8,0.2))
train = Balanced_data[indx==1, ]
test = Balanced_data[indx==1, ]
fit_glm = glm(Manipulater ~ DSRI+GMI+AQI+SGI+ACCR, data=train, family = binomial(link = "logit"))
glm_pred = predict(fit_glm,test,type="response")
plot(glm_pred)
test_predict <- prediction(glm_pred, test$Manipulater)

perf = performance(test_predict,"tpr","fpr")
plot(perf, colorize = T, lwd = 2)

print(perf@x.values)
print(perf@y.values)
print(perf@alpha.values)

mydistance <- function(x,y,p){
  d=(x-0)^2+(y-1)^2 # given the points (x, y), compute the distance to the corner point (0,1)
  ind <- which(d==min(d)) # Find the minimum distance and its index
  c(recall = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]]) # return the corresponding tpr, fpr and the cutoff point
}

opt.cut <- function(perf){
  cut.ind <- mapply(FUN = mydistance, 
                    perf@x.values, perf@y.values,perf@alpha.values)
}
Output <- opt.cut(perf)
print(Output[,1])

Threshold <- Output[,1]["cutoff"]
predictedClass <- as.factor(ifelse(glm_pred >= Threshold, "Low", "High"))
CM1 <- table(predictedClass, test$Manipulater, dnn = c("Predicted","Actual"))
print(CM1)
tpr <- sum(CM1[1])/sum(CM1[1],CM1[1,-1])
fpr <- sum(CM1[2])/sum(CM1[2],CM1[2,-1])
youden_index = tpr + fpr - 1
youden_index




# (6) Based on the models developed in questions 4 and 5, suggest a M-score (Manipulator score)
# that can be used by regulators to identify potential manipulators.





data$C.MANIPULATOR <- as.factor(ifelse(data$C.MANIPULATOR == "1", "Manipulator","Non-Manipulators"))
summary(data$C.MANIPULATOR)
install.packages("ROSE")