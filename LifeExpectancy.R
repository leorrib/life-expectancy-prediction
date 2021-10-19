dir <- ''
setwd(dir)
getwd()
source('src/tools.R')
tools.load_sources()

file <- list.files(pattern = ".csv")
raw_data <- read.csv(file)
dim(raw_data)
colnames(raw_data)

library(dplyr)
raw_data <- raw_data %>% 
  rename( Thin.5.to.9.yo = thinness.5.9.years,
          Thin.10.to.19.yo = thinness..1.19.years,
          Measles.cases = Measles,
          Alcohol.consume = Alcohol,
          GDP.on.health = percentage.expenditure,
          HepB.immune = Hepatitis.B,
          Polio.immune = Polio,
          DPT.vacc = Diphtheria,
          Inc.comp.resource = Income.composition.of.resources,
          Years.in.School = Schooling,
          Deaths.under.5.yo = under.five.deaths,
          Infant.deaths = infant.deaths,
          Development.Status = Status)

# Exploratory Data Analysis

de.find_values(raw_data, value = NA)

raw_data <- dm.drop_cols(raw_data, c('HepB.immune', 'Population'))
raw_data <- na.omit(raw_data)
dim(raw_data)

de.find_values(raw_data, value = 0)
raw_data <- raw_data[raw_data$Inc.comp.resource != 0,]
raw_data <- raw_data[raw_data$Years.in.School != 0,]
dim(raw_data)

library(ggplot2)
raw_data <- dm.factorize_cols(raw_data, 'Development.Status')
raw_data <- dm.range_divide(raw_data, 'Life.expectancy', 3, 1)
dv.plot_multiple_bars_II(raw_data, c('Development.Status'),
                         'Life.expectancy_range',
                         title = 'Life Expectancy Range per', 
                         xlabel = '',
                         ylabel = 'Number of cases')

cors <- de.build_corr_coef_list(raw_data[,-c(1, 3, 21)], 'spearman')
de.get_corr_coef_w_target_var(data.frame(cors), 'Life.expectancy', 0.7)

require(lattice)
Map(dv.plot_corr_coeffs, cors, 
    'spearman', 'Correlation between variables using the method')

labels <- c("Life exp per Years in School",
            "Life exp per Income composition of resources",
            "Life exp per GDP",
            "Life exp per Polio Immunization")

xAxis <- c("Years.in.School", "Inc.comp.resource", "GDP",
           "Polio.immune")

Map(dv.plot_scatter, xAxis, labels)

labels_n <- c("Life exp per Thin.5.to.9.yo",
              "Life exp per Thin.10.to.19.yo",
              "Life exp per HIV.AIDS",
              "Life exp per Measles.cases")

xAxis_n <- c("Thin.5.to.9.yo",
             "Thin.10.to.19.yo",
             "HIV.AIDS",
             "Measles.cases")

Map(dv.plot_scatter, xAxis_n, labels_n)


# Data Manipulation

data <- raw_data
data$Development.Status = sapply(raw_data$Development.Status, function(x) {
  ifelse(x == 'Developed', 1, 0)
})
data <- dm.drop_cols(data, 'Life.expectancy_range')

cols_to_normalize = c(colnames(data[,c(-1, -4)]))
data <- dm.normalize_cols(data, cols_to_normalize)
summary(data$GDP)

de.get_corr_coef_predictor_vars(data.frame(cors), 'Life.expectancy', 0.8)

data <- dm.drop_cols(data, 'Infant.deaths')


#  The Machine Learning Model

random_indexes <- de.get_random_row_indexes(data, 70)
trainSet <- data[random_indexes, ]
testSet <- data[-random_indexes, ]

# Model 1

relevance <- lm(Life.expectancy ~. - Country - Year - GDP - Polio.immune
                - Thin.10.to.19.yo, 
                data = trainSet)
summary(relevance)

model_1 <- lm(Life.expectancy ~ Development.Status + Adult.Mortality 
              + Alcohol.consume + Total.expenditure + DPT.vacc 
              + HIV.AIDS + Inc.comp.resource + Years.in.School, 
              data = trainSet)
summary(model_1)

prediction = data.frame(predict(model_1, testSet, interval = 'confidence'))
score_1 <- data.frame(actual = testSet$Life.expectancy,
                      prediction = prediction$fit)
score_1 <- mutate(score_1, error = prediction - actual)
ggplot(score_1, aes(x = error)) +
  geom_histogram(binwidth = 1, fill = 'white', color = 'black')

res <- -score_1$error
pred <- score_1$prediction
obs <- score_1$actual
var_range <- range(pred, obs)
plot(obs, pred, 
     xlim = var_range, ylim = var_range,  
     xlab = "Observed Life Expectancy", 
     ylab = "Predicted Life Expectancy",
     main = "Residues of Model 1")
abline(0,1, col = "red")
segments(obs, pred, obs, pred + res)

RMSE <- sqrt(sum(score_1$error^2)/nrow(score_1))
RMSE

# Model 2

library(randomForest)
relevance <- randomForest(Life.expectancy ~. - Country - Year - GDP 
                          - Thin.10.to.19.yo - Polio.immune, 
                          data = trainSet, 
                          ntree = 500,
                          nodesize = 4, 
                          importance = T)
varImpPlot(relevance)

model_2 <- randomForest(Life.expectancy ~. - Country - Year 
                        - GDP - Thin.10.to.19.yo - Polio.immune 
                        - Development.Status,
                        data = trainSet,
                        ntree = 500,
                        nodesize = 4)
model_2

prediction = predict(model_2, testSet)
score_2 <- data.frame(actual = testSet$Life.expectancy,
                      predicted = prediction)
score_2 <- mutate(score_2, error = predicted - actual)
ggplot(score_2, aes(x = error)) +
  geom_histogram(binwidth = 1, fill = 'white', color = 'black')

res <- (-1)*score_2$error
pred <- score_2$predicted
obs <- score_2$actual
var_range <- range(pred, obs)
plot(obs, pred,
     xlim = var_range, ylim = var_range,
     xlab = "Observed Life Expectancy",
     ylab = "Predicted Life Expectancy",
     main = "Residuals of Model 2")
abline(0,1, col = "red")
segments(obs, pred, obs, pred + res)

RMSE <- sqrt(sum(score_2$error^2)/nrow(score_2))
RMSE
