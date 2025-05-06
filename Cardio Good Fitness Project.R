#======================================================================= 
# 
# Exploratory Data Analysis - CardioGoodFitness 
# 
#=======================================================================

# Environment Set up and Data Import

# Invoking Libraries 
library(readr) # To import csv files
library(ggplot2) # To create plots
library(corrplot) # To plot correlation plot between numerical variables
library(dplyr) # To manipulate dataset
library(gridExtra) # To plot multiple ggplot graphs in a grid
library(knitr) # Necessary to generate sourcecodes from a .Rmd File
library(markdown) # To convert to HTML
library(rmarkdown) # To convret analyses into high quality documents

# Set working directory
setwd("C:/Users/egwuc/Desktop/PGP-DSBA-UT Austin/Introduction to R/Week 4 - Project/")

# Read input file
Cardio_fitness <- read_csv("CardioGoodFitness.csv")

# Global options settings 
options(scipen = 999) # turn off scientific notation like 1e+06

# Variable identification
# check dimension of dataset 
dim(Cardio_fitness)

# check first 6 rows(observations) of dataset
head(Cardio_fitness)

# check last 6 rows(observations) of dataset
tail(Cardio_fitness)

# check structure of dataset
str(Cardio_fitness)

# change product, gender and maritalstatus to factor variable
Cardio_fitness$Product <- as.factor(Cardio_fitness$Product)
Cardio_fitness$Gender <- as.factor(Cardio_fitness$Gender)
Cardio_fitness$MaritalStatus <- as.factor(Cardio_fitness$MaritalStatus)

# get summary of dataset
summary(Cardio_fitness)

# View the dataset 
View(Cardio_fitness)

ggplot(Cardio_fitness, aes(x = Product)) + 
  geom_bar(fill = c("red"), color="black") + 
  labs(x = "Product",
       y = "Frequency",
       title = "")

ggplot(Cardio_fitness, aes(x = Gender)) + 
  geom_bar(fill = c("blue"), color="black") + 
  labs(x = "Gender",
       y = "Frequency",
       title = "")

ggplot(Cardio_fitness, aes(x = MaritalStatus)) + 
  geom_bar(fill = c("yellow"), color="black") + 
  labs(x = "MaritalStatus",
       y = "Frequency",
       title = "")

plot_histogram_n_boxplot = function(variable, variableNameString, binw){
  
  a = ggplot(data = Cardio_fitness, aes(x= variable)) +
    labs(x = variableNameString,y ='frequency')+
    geom_histogram(fill = 'green',col = 'white', binwidth = binw) +
    geom_vline(aes(xintercept = mean(variable)),
               color = "black", linetype = "dashed", size = 0.5)
  
  b = ggplot(data = Cardio_fitness, aes('',variable))+ 
    geom_boxplot(outlier.colour = 'red',col = 'red', outlier.shape = 19)+
    labs(x = '', y = variableNameString) + coord_flip()
  grid.arrange(a,b,ncol = 2)
}

plot_histogram_n_boxplot(Cardio_fitness$Age, 'Age', 1)

plot_histogram_n_boxplot(Cardio_fitness$Education, 'Education', 1)

plot_histogram_n_boxplot(Cardio_fitness$Usage, 'Usage', 1)

plot_histogram_n_boxplot(Cardio_fitness$Fitness, 'Fitness', 1)

plot_histogram_n_boxplot(Cardio_fitness$Income, 'Income', 10000)

plot_histogram_n_boxplot(Cardio_fitness$Miles, 'Miles', 50)

ggplot(Cardio_fitness, aes(x = Gender, fill = Product)) + 
  geom_bar(position = "dodge") + 
  labs(y = "Count", 
       fill = "Product",
       x = "Gender",
       title = "Gender by Product") +
  theme_minimal()

ggplot(Cardio_fitness, aes(x = MaritalStatus, fill = Product)) + 
  geom_bar(position = "dodge") + 
  labs(y = "Count", 
       fill = "Product",
       x = "MaritalStatus",
       title = "MaritalStatus by Product") +
  theme_minimal()

# Numeric variables in the data
num_vars = sapply(Cardio_fitness, is.numeric)

# Correlation Plot
corrplot(cor(Cardio_fitness[,num_vars]), method = 'number')

plot_scatterplot = function(variableNameString, variable, binw){
  ggplot(data = Cardio_fitness, aes(x= Age, y = variable)) +
    labs(x = "Age", y = variableNameString) +
    geom_point(color="cornflowerblue", size =2, alpha=.8)+
    geom_smooth(method ="lm") # adds a linear trend line which is useful to summarize the relationship between the two variables
}

grid.arrange(plot_scatterplot('Education', Cardio_fitness$Education),
             plot_scatterplot('Usage', Cardio_fitness$Usage),
             plot_scatterplot('Fitness', Cardio_fitness$Fitness),
             plot_scatterplot('Income', Cardio_fitness$Income),
             plot_scatterplot('Miles', Cardio_fitness$Miles),
             ncol = 3)

ggplot(Cardio_fitness, 
       aes(x = Age, 
           fill = Product)) +
  geom_density(alpha = 0.4) +
  labs(title = "Age by Product")


ggplot(Cardio_fitness, 
       aes(x = Product, 
           y = Age)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Age by Product")

ggplot(Cardio_fitness, 
       aes(x = Usage, 
           fill = Product)) +
  geom_density(alpha = 0.4) +
  labs(title = "Usage by Product")


ggplot(Cardio_fitness, 
       aes(x = Product, 
           y = Usage)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Usage by Product")

ggplot(Cardio_fitness, 
       aes(x = Fitness, 
           fill = Product)) +
  geom_density(alpha = 0.4) +
  labs(title = "Fitness by Product")


ggplot(Cardio_fitness, 
       aes(x = Product, 
           y = Fitness)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Fitness by Product")

ggplot(Cardio_fitness, 
       aes(x = Income, 
           fill = Product)) +
  geom_density(alpha = 0.4) +
  labs(title = "Income by Product")


ggplot(Cardio_fitness, 
       aes(x = Product, 
           y = Income)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Income by Product")

ggplot(Cardio_fitness, 
       aes(x = Miles, 
           fill = Product)) +
  geom_density(alpha = 0.4) +
  labs(title = "Miles by Product")


ggplot(Cardio_fitness, 
       aes(x = Product, 
           y = Miles)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Miles by Product")

ggplot(Cardio_fitness, 
       aes(x = Usage, 
           fill = Gender)) +
  geom_density(alpha = 0.4) +
  labs(title = "Usage by Gender")


ggplot(Cardio_fitness, 
       aes(x = Gender, 
           y = Usage)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Usage by Gender")

ggplot(Cardio_fitness, 
       aes(x = Fitness, 
           fill = Gender)) +
  geom_density(alpha = 0.4) +
  labs(title = "Fitness by Gender")


ggplot(Cardio_fitness, 
       aes(x = Gender, 
           y = Fitness)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Fitness by Gender")

ggplot(Cardio_fitness, 
       aes(x = Income, 
           fill = Gender)) +
  geom_density(alpha = 0.4) +
  labs(title = "Income by Gender")


ggplot(Cardio_fitness, 
       aes(x = Gender, 
           y = Income)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Income by Gender")

ggplot(Cardio_fitness, 
       aes(x = Miles, 
           fill = Gender)) +
  geom_density(alpha = 0.4) +
  labs(title = "Miles by Gender")


ggplot(Cardio_fitness, 
       aes(x = Gender, 
           y = Miles)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Miles by Gender")

ggplot(Cardio_fitness, 
       aes(x = Usage, 
           fill = MaritalStatus)) +
  geom_density(alpha = 0.4) +
  labs(title = "Usage by MaritalStatus")


ggplot(Cardio_fitness, 
       aes(x = MaritalStatus, 
           y = Usage)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Usage by MaritalStatus")

ggplot(Cardio_fitness, 
       aes(x = Fitness, 
           fill = MaritalStatus)) +
  geom_density(alpha = 0.4) +
  labs(title = "Fitness by MaritalStatus")


ggplot(Cardio_fitness, 
       aes(x = MaritalStatus, 
           y = Fitness)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Fitness by MaritalStatus")

ggplot(Cardio_fitness, 
       aes(x = Income, 
           fill = MaritalStatus)) +
  geom_density(alpha = 0.4) +
  labs(title = "Income by MaritalStatus")


ggplot(Cardio_fitness, 
       aes(x = MaritalStatus, 
           y = Income)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Income by MaritalStatus")

ggplot(Cardio_fitness, 
       aes(x = Miles, 
           fill = MaritalStatus)) +
  geom_density(alpha = 0.4) +
  labs(title = "Miles by MaritalStatus")


ggplot(Cardio_fitness, 
       aes(x = MaritalStatus, 
           y = Miles)) +
  geom_boxplot(color = "cornflowerblue") +
  labs(title = "Miles by MaritalStatus")

#======================================================================= 
# 
# T H E - E N D 
# 
#=======================================================================

# Generate the .R file from this .Rmd to hold the source code 

purl("Cardio Good Fitness Project.Rmd", documentation = 0)
