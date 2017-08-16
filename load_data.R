# set working directory
library(psych)  # describe, additional statistics
library(ROCR)  # ROC curve
library(corrplot)  # plot correlation matrix

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

# visualize correlations plot, save to file
cor_matrix <- cor(DATA_1[, !names(DATA_1) %in% c(factor_column_names, 'TimeStamp', 'LABELS')])
pdf(file.path(output_dir, 'corr_plot.pdf'))
corrplot(cor_matrix,
         type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# dev.copy(png, )  # , width=500, height=500)
dev.off()
# obviously, Startbalance and EndBalance are strongly correlated,
# as well as MBetsAmount with Startbalance and EndBalance



### plan onwards:
# handle the multi-colinearity in the data,
# by either removing a few variables or creating a new one
# (possibly log variables wrt balance, relative win)

# extract factors from posixlt varaible
# as from:
# https://stackoverflow.com/questions/11583166/extract-weekday-from-posixlt-date-time-variable
# strptime("07/20/2012 18:00", "%m/%d/%Y %H:%M")

# regularize

# create new features, with interactions

# forward greedy search

# logistic reg
# as from:
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/



