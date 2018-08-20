setwd("/Users/praneesh/Downloads/All")




# General
library(tidyverse)
library(skimr)

# Preprocessing
library(recipes)

# Machine Learning
library(h2o)

app_train <- read.csv(file = "application_train.csv", header=TRUE, stringsAsFactors = TRUE)
app_test <- read.csv(file = "application_test.csv", header=TRUE, stringsAsFactors = TRUE)

# Training data: Separate into x and y tibbles
x_train_tbl <- app_train %>% select(-TARGET)
y_train_tbl <- app_train %>% select(TARGET)

# Testing data: What we submit in the competition
x_test_tbl  <- app_test

# Remove the original data to save memory
rm(app_train)
rm(app_test)

# Data Inspection

skim_to_list(x_train_tbl)
skimvar <- skim(x_train_tbl)
factorvar <- skim(x_train_tbl) %>% filter(type == "factor")
integervar <- skim(x_train_tbl) %>% filter(type == "integer")
numericvar <- skim(x_train_tbl) %>% filter(type == "numeric")

rm(factorvar,integervar,numericvar)

# There are 3 data type formats: integer, numeric, and character.
# For H2O we need numeric and factor. Some of the integer values are “FLAG”, which typically indicates a factor.

x_factors<- x_train_tbl %>% keep(is.factor) %>% names()
x_numeric <- x_train_tbl %>% keep(is.numeric) %>% names()
x_train_tbl %>% keep(is.integer) %>% names() %>% length()

for (i in seq(x_factors))
{

  f <- ggplot(cbind(y_train_tbl,x_train_tbl), aes_string(x_factors[i],fill=quote(factor(TARGET)))) +
    geom_bar(position="fill") +
    theme_bw()
      ggsave(f,filename=paste("myplot",x_factors[i],".png",sep="")) #save images
    print(x_factors[i])
    print(f)
}

for (i in seq(x_numeric))
{

  f <- ggplot(cbind(y_train_tbl,x_train_tbl), aes(as.factor(TARGET))) +
    geom_boxplot(aes_string(x_numeric[i])) # stacked percent
  ggsave(f,filename=paste("myplot",x_numeric[i],".png",sep="")) #save images
  print(x_numeric[i])
  print(f)
}
#XXXX

cbind(y_train_tbl,x_train_tbl) %>%
  keep(is.numeric) %>%
  gather(-TARGET,key="Var",value="Value") %>%
  ggplot(aes(x=TARGET, y=Value)) + geom_jitter(height=2,width=2) + facet_grid(~Var)


  head()

#summary
cbind(y_train_tbl,x_train_tbl) %>% select(ORGANIZATION_TYPE) %>%
  group_by(ORGANIZATION_TYPE) %>%
  count() %>%
  arrange(desc(n)) %>%
  View()


#Character data
string_2_factor_names <- x_train_tbl %>% select_if(is.character) %>% names()
string_2_factor_names

#Numeric factor data
unique_numeric_values_tbl <- x_train_tbl %>%
  select_if(is.numeric) %>%
  map_df(~ unique(.) %>% length()) %>%
  gather() %>%
  arrange(value) %>%
  mutate(key = as_factor(key))

unique_numeric_values_tbl

factor_limit <- 7

num_2_factor_names <- unique_numeric_values_tbl %>%
  filter(value < factor_limit) %>%
  arrange(desc(value)) %>%
  pull(key) %>%
  as.character()

num_2_factor_names

#Missing data
missing_tbl <- x_train_tbl %>%
  summarize_all(.funs = ~ sum(is.na(.)) / length(.)) %>% #.funs anonymous function
  gather() %>%
  arrange(desc(value)) %>%
  filter(value > 0)

missing_tbl


