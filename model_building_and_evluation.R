rm(list = ls(all = TRUE))
###################################
### Model Building & Evaluation ###
###################################
library('randomForest')
library('FSelector')
library('ROCR')
load('feat.Rdata')

###################
### Build Model ###
###################


# Sample to balance target feature
ind1     = which(train[,"Bad_label"] == 1)
ind0     = which(train[,"Bad_label"] == 0)
sampind1 = sample(ind1, length(ind1), replace = TRUE)
sampind0 = sample(ind0, length(ind1), replace = TRUE) # undersampling
sampind  = c(sampind1, sampind0)
train    = train[sampind,]

print(information.gain(Bad_label ~ . , train))
write.csv(information.gain(Bad_label ~.,train), file = 'ig.csv')

# Build random forest model
model = randomForest(Bad_label ~ . ,
                     data = train, 
                     importance = TRUE,
                     do.trace = 500,
                     ntree = 1000)

fit        = predict(model, test[,2:ncol(test)], type = "prob")
prediction = prediction(fit[,2], test$Bad_label)


################
### Evaluate ###
################

auroc      = round(performance(prediction, measure = "auc")@y.values[[1]]*100, 2)
Gini       = (2*auroc - 100)
print(Gini)
score       = data.frame(score = apply(fit[,1:2],1, max))
score$class = colnames(fit)[apply((fit[,1:2] == score[,1]), 1, which.max)]
score       = score[order(score[,1], decreasing = TRUE),]
rank = data.frame(decile = c(10:1))
win = nrow(score)/10
for (x in c(1:10)){
  rank[x,2] = sum(score[(win*x-(win-1)):(win*x),'class'] == 1)/win
}

roc.perf = performance(prediction, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)
specificity = sum(score$class == test$Bad_label & score$class == '0') / (sum(score$class == test$Bad_label & score$class == '0') + sum(score$class != test$Bad_label & score$class == '1'))
sensitivity = sum(score$class == test$Bad_label & score$class == '1') / (sum(score$class == test$Bad_label & score$class == '1') + sum(score$class != test$Bad_label & score$class == '0'))

# Output result
print(rank)
print(specificity)
print(sensitivity)
write.csv(rank, file = 'rank.csv')
write.csv(information.gain(Bad_label ~.,train), file = 'ig.csv')

