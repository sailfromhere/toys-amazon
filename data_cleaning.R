library(tidyverse)
library(lubridate)

# import data

Toys_Amazon <- read_csv("Toys_Amazon.csv", 
                        # specify data type as numeric to avoid parsing error
                        col_types = cols(number_of_reviews = col_number()))

# split data

product <- Toys_Amazon %>%
  select(uniq_id, product_name, manufacturer, price,
         number_available_in_stock, number_of_reviews,
         number_of_answered_questions, average_review_rating,
         description, product_information, product_description)

cat <- Toys_Amazon %>%
  select(uniq_id, amazon_category_and_sub_category)

also_bought <- Toys_Amazon %>%
  select(uniq_id, customers_who_bought_this_item_also_bought)

bought_after <- Toys_Amazon %>%
  select(uniq_id, items_customers_buy_after_viewing_this_item)

qa <- Toys_Amazon %>%
  select(uniq_id, customer_questions_and_answers)

reviews <- Toys_Amazon %>%
  select(uniq_id, customer_reviews)

sellers <- Toys_Amazon %>%
  select(uniq_id, sellers)

# clean product dataframe

## price

df1 <- product %>%
  # parse price as numeric
  mutate(price = parse_number(price))
df1[,4]

## number available

df2 <- df1 %>%
  # separate into availability and condition
  separate(number_available_in_stock, 
           into = c("number_available", "condition")) %>%
  # parse availability as numeric
  mutate(number_available = parse_number(number_available))
df2[,5:6]

## ratings

df3 <- df2 %>%
  mutate(
    average_review_rating = 
      # parse as number
      parse_number(
        # extract the ratings
        str_extract(
          average_review_rating, 
          # use regular expression to 
          # look for the ratings
          "[:digit:]\\.[:digit:](?=[:space:])"
        )
      )
  )
df3[,"average_review_rating"]

## product description

tmp <- df3

# create variable that returns 1 
# when description is different from 
# product_description
tmp$isdiff <- ifelse(tmp$description == 
                       tmp$product_description,
                     0, 1)

# output the observations that are different
tmp <- tmp %>%
  select(description, product_description) %>%
  filter(tmp$isdiff == 1)

# view the output
# only one observation is different
tmp

# content of that observation is still
# largely the same
# drop product_description
df4 <- df3 %>% select(-product_description)

## output to product dataframe

product <- df4

# clean cat dataframe

df1 <- cat %>%
  # drop na rows
  drop_na() %>%
  # separate each product's categories
  separate(amazon_category_and_sub_category,
           # into a main category
           into = c("category",
                    # and 4 sub categories
                    paste0("sub_cat", 1:4)),
           sep = " > ")
df1

# output to cat dataframe
cat <- df1

# clean also_bought and bought_after dataframes

# define a function to clean amazon urls
# takes in 4 arguments:
# 1. dataframe, 2. column to operate on,
# 3. new columns/keys to turn that column into,
# 4. number of new columns/keys
clean_amzn_url <- function(data, col, into, n) {
  
  tmp <- data
  
  # clean urls into product names
  tmp[[col]] <-
    tmp[[col]] %>%
    # remove head of urls
    str_remove_all("http://www.amazon.co.uk/") %>%
    # replace dashes with spaces
    str_replace_all("\\-", " ") %>%
    # remove tail of urls
    str_remove_all("(?<=\\/dp\\/)[:alnum:]*(?=[:space:]\\|[:space:])") %>%
    # remove tail of the last url in a cell
    str_remove("(?<=\\/dp\\/)[:alnum:]*(?![:blank:])") %>%
    # remove the remnant of urls
    str_remove_all("\\/dp\\/")
  
  tmp <- tmp %>%
    # separate each product into a new column
    separate(col,
             into = c(paste0(into, 1:n)),
             sep = " \\| ") %>%
    # gather new columns into clean format
    pivot_longer(c(paste0(into, 1:n)), 
                 names_to = paste0(into, "_key"), 
                 values_to = paste0(into, "_product")) %>%
    # order by uniq_id
    arrange(uniq_id) %>%
    # drop na rows
    drop_na()

  return(tmp)
  
}

# use the function to clean also_bought dataframe
also_bought <- also_bought %>%
  clean_amzn_url(col = "customers_who_bought_this_item_also_bought",
                 into = "also_bought",
                 # max of 12 also_bought products
                 n = 12)

# use the function to clean bought_after_viewing dataframe
bought_after <- bought_after %>%
  clean_amzn_url(col = "items_customers_buy_after_viewing_this_item",
                 into = "bought_after_viewing",
                 # max of 4 bought_after_viewing products
                 n = 4)

head(also_bought)
head(bought_after)

# clean qa dataframe

# separate the variable into 10 columns
df1 <- qa %>%
  # drop na columns
  drop_na() %>%
  # separate into 10 qa sets
  separate(customer_questions_and_answers,
           into = c(paste0("set", 1:10)),
           sep = " \\| ")

df2 <- df1 %>%
  # gather qa sets into clean format
  pivot_longer(c(paste0("set", 1:10)),
               names_to = "question_set",
               values_to = "question_answer") %>%
  drop_na()

df3 <- df2 %>%
  separate("question_answer",
           into = c("question", "answer"),
           sep = " // ")

head(df3)
qa <- df3














