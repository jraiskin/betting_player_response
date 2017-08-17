library(psych)  # describe, additional statistics
library(ROCR)  # ROC curve
library(corrplot)  # plot correlation matrix

# set working directory
work_dir <- switch(Sys.info()[["user"]],
             'Yarden-' = 'D:/Python_env/betting_player_response')
stopifnot(dir.exists(work_dir))
setwd(work_dir)

# set file names and paths
data_dir <- 'data'
output_dir <- 'outputs'
utils_dir <- 'utils'
raw_data_fname <- 'class_prob_data.RData'
raw_data_path <- file.path(data_dir, raw_data_fname)

load(raw_data_path)  # loaded as DATA_1

# some basic descriptives
str(DATA_1)


#### descriptive statisttics ####
# change types to factors
factor_column_names <- c('PlatformID', 'GameID', 'IND_USER_ID')
for (column in factor_column_names){
  DATA_1[, column] <- as.factor(DATA_1[, column])
}

# look for missing values => none found
DATA_1[!complete.cases(DATA_1),]

# have a look again (visualization is not straight forward)
summary(DATA_1)
str(DATA_1)
describe(DATA_1[, names(DATA_1) != 'TimeStamp'])  # exclude time var
# variables are skewed, especially the "monetary" variables (unsurprising)


#### visualize correlations plot, save to file ####
cor_matrix <- cor(DATA_1[, !names(DATA_1) %in% c(factor_column_names, 'TimeStamp', 'LABELS')])
pdf(file.path(output_dir, 'corr_plot.pdf'))
corrplot(cor_matrix,
         type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# dev.copy(png, )  # , width=500, height=500)
dev.off()
# obviously, Startbalance and EndBalance are strongly correlated,
# as well as MBetsAmount with Startbalance and EndBalance


#### test-train split ####

test_split_ratio <- 0.2
n_rows <- nrow(DATA_1)
train_sample_size <- floor((1-test_split_ratio)*n_rows)
## set seed for partition reproductibility
set.seed(42)
train_ind <- sample(seq_len(n_rows), 
                    size=train_sample_size)

train_set <- DATA_1[train_ind, ]
test_set <- DATA_1[-train_ind, ]


#### logistic regression on the basic model ####
# this will be my "baseline"
# standard glm methods cannot handle more variables than provided here (memory issues)

fit <- glm(LABELS ~ Time_from_first_spin_seconds + Startbalance + MBetsAmount + V_1, 
           data=train_set, 
           family=binomial(link='logit'))

summary(fit)

fit_results <- predict(fit, type='response', 
                       newdata=test_set)
fit_results <- ifelse(fit_results > 0.5,1,0)

accuracy <- mean(fit_results == DATA_1$LABELS)
print(paste('Accuracy:',round(accuracy, 3)))


#### feature extraction ####
# get the weekday
DATA_1$weekdays <- as.factor(weekdays(DATA_1$TimeStamp))
# get the (decimal) hour
DATA_1$hour <- as.factor(format(DATA_1$TimeStamp,'%H'))
# we should *usually* get: EndBalance=Startbalance - MBetsAmount+MWinsAmount
# therefore, we can define the differences as "gifts"
DATA_1$gift <- with(DATA_1, EndBalance - Startbalance + MBetsAmount - MWInsAmount)
DATA_1$gift <- with(DATA_1, gift / Startbalance)
# possibly useful variables, normalizing "monetary" variables
DATA_1$MBetsAmount <- with(DATA_1, MBetsAmount / Startbalance)
DATA_1$log_Startbalance <- log(DATA_1$Startbalance)

# allocate test-train again
train_set <- DATA_1[train_ind, ]
test_set <- DATA_1[-train_ind, ]
all(names(DATA_1) == names(test_set))

describe(DATA_1$gift)  # very skewed, even after normalizing for start balance


#### forward selection ####
# Ideally, I would try lots more interactions and cross validation.
# However, computing power starts to be an issue here so search is significantly restricted.
# I have not used some of the balance variable, due to the 
# multi-colinearity in the data.
fit_forward <- glm(LABELS ~ 1, 
                   data=train_set, 
                   family=binomial(link='logit'))

step(fit_forward, 
     direction='forward',
     steps=4,  # up to *steps* variables
     scope=~ GameID * (Time_from_first_spin_seconds + log_Startbalance
             + MBetsAmount + MWInsAmount + gift + V_1 + weekdays + hour))



# fit_cv <- cv.glm(DATA_1, fit, K=5)

## chosen model formula
form <- LABELS ~ Time_from_first_spin_seconds + hour + GameID
fit_select <- glm(form, 
                  data=DATA_1, family=binomial(link='logit'))

fit_results <- predict(fit_select, type='response')
fit_results <- ifelse(fit_results > 0.5,1,0)

accuracy <- mean(fit_results == DATA_1$LABELS)
print(paste('Accuracy:', round(accuracy, 3)))


#### ROC analysis ####
p <- predict(fit_select, type="response")
pr <- prediction(p, DATA_1$LABELS)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
# plot ROC, save to file
pdf(file.path(output_dir, 'roc.pdf'))
plot(prf)
abline(a=0, b= 1)
# dev.copy(png, )  # , width=500, height=500)
dev.off()

# calculate AUC metric
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(paste('AUC (Area Under the Curve):', round(auc, 3)))

