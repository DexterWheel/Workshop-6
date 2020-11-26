#We used the same data to train the LDA model as we used to examine its performance. Few were incorrectly classified. But this is not very robust - we could have overfitting.

#We can use the caret function createDataPartition() to split the dataset in to training and testing sets.

#It returns a proportion of row numbers randomly sampled from the dataframe.

library(tidyverse)
library(GGally)
library(caret)
library(e1071)

library(palmerpenguins)

#clean the variable names for ease of use:

penguin <- penguins_raw %>%
  janitor::clean_names()


#filter out rows with missing values

penguin <- penguin %>% 
  filter(!is.na(body_mass_g))

#Split species into common_name and scientific_name:

penguin <- penguin %>% 
  extract(species, 
          c("common_name", "scientific_name"),
          "([a-zA-Z]+\\s[a-zA-Z]+)\\s\\(([a-zA-Z]+\\s[a-zA-Z]+)\\)")

#Create a vector of row numbers that will be used to split the dataset in to training and testing sets:

ids <- createDataPartition(y = penguin$common_name,
                           p = 0.75,
                           list = FALSE)

#p is the proportion of rows to sample.

#list = FALSE gives me a vector of numbers rather than a one item list.

#You might want to examine the ids variable.

ids

#Now we use those row numbers to select the rows from penguin to create the training and test datasets. We use the dplyr function slice which works like the filter function but filters rows on their index rather than a match to a condition.


train <- penguin %>% slice(ids)

test <- penguin %>% slice(-ids)

#The process of the using lda() and predict() is the same as previously but the input data differ.

lda <- train %>%
  select(body_mass_g,
         ends_with("_mm")) %>%
  MASS::lda(grouping = train$common_name)


plda <- test %>%
  select(body_mass_g,
         ends_with("_mm")) %>%
  predict(object = lda)


confusionMatrix(plda$class,factor(test$common_name))
