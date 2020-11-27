library(tidyverse)
library(GGally)
library(caret)
library(e1071)

library(palmerpenguins)

#In these slides show you will apply:
  
#LDA to the Penguin data without training and testing

#LDA to the Penguin data with training and testing

#LDA to the scRNASeq data with training and testing but we will consider how good the model is at predicting classes from the training set compared to the test set.

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

#Select the four variables and pipe into the MASS::lda() function which does the PCA:

lda <- penguin %>% 
  select(body_mass_g,
         ends_with("_mm")) %>%
  MASS::lda(grouping = penguin$common_name)

#we saved the result to a list

#This is a good example of when we definitely want to use package::function().

#MASS has a function called "select()" like dplyr, but it works differently (the function is defined differently).

#You can spend hours of your life wondering what is wrong with your code when it looks fine and worked before if you load both packages. I have done this with these particular functions too many times!


#Just as we could see the importance of each variable in each Principal Component using pca$loadings

#we can see the importance of each variable in each discriminant using lda$scaling.

lda$scaling


#LD1 = 0.0013 body_mass_g +  0.0883 culmen_length_mm +  -1.0373 culmen_depth_mm + 0.0862 flipper_length_mm You might want to compare to the loadings for PCA below
#PC1 =  0.5483502 body_mass_g +  0.4552503 culmen_length_mm +  -0.4003347 culmen_depth_mm +  0.5760133 flipper_length_mm


#The generic function predict() is used to give the species predicted by the model for each observation.

#Select the variables from Penguin that are in the model and predict the species from the lda model object:

plda <- penguin %>% 
  select(body_mass_g,
         ends_with("_mm")) %>%
  predict(object = lda)

plda

#caret provides us with a useful function to examine the confusion matrix.

#A confusion matrix is a table that tells us about the performance of a classification model.

#correct predictions: the species predicted matches the observed species

#incorrect predictions: the species predicted does not match the observed species. for each species.



#confusionMatrix() function also outputs:
  
#Accuracy - No. correct predictions / No. of observations

#95% CI - 95 percent confidence interval on the accuracy (using binom.test())

#No Information Rate - No. observations in the largest class / Number of observations

#P-Value [Acc > NIR] - Is the model significantly better than than you could do by always predicting the most common class (again using binom.test()).


confusionMatrix(plda$class, factor(penguin$common_name))

#To plot, we might want to use the scores on each of the new axes and colour them by species. The scores are in a variable called $x in plda

#Extract the scores into a dataframe with the species names:

lda_labelled <- data.frame(plda$x,
                           common_name = penguin$common_name)
lda_labelled %>% 
  ggplot(aes(x = LD1, y = LD2, color = common_name)) +
  geom_point()


#The separation between species is stronger in the LDA than in the PCA

#We used the same data to train the LDA model as we used to examine its performance. Few were incorrectly classified. But this is not very robust - we could have overfitting.

#We can use the caret function createDataPartition() to split the dataset in to training and testing sets.