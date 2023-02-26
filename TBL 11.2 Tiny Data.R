# setwd()

install.packages("neuralnet")
install.packages("nnet")
install.packages("caret")
install.packages("e1071")



library(neuralnet); library(nnet); library(caret); library(e1071)

# ---- Table 11.2: Neural Network with a Single Hidden Layer (3 Nodes)  -- for Tiny Data----------

df<- read.csv("TinyData.csv")
df
class(df)
str(df)

df$like <- df$Acceptance == "like"
df$dislike <- df$Acceptance == "dislike"
df

set.seed(1234) # Compare resulting nn$weights with and without seed. 
nn <- neuralnet(like + dislike ~ Salt + Fat, data = df, linear.output = F, hidden = 3)

# display weights
nn$weights

# display predictions
prediction(nn)

# plot network
plot(nn, rep="best")

# -----End of Table 11.2--------------------------------------------------------

# ----- Table 11.3: Confusion Matrix for the Tiny Example -----------

library(caret)
predict <- compute(nn, data.frame(df$Salt, df$Fat))
predicted.class = apply(predict$net.result, 1, which.max) - 1
# 
confusionMatrix(as.factor(ifelse(predicted.class =="1", "dislike", "like")), as.factor(df$Acceptance))

# ----------- End of Table 11.3 -------------------

##


