library(tidyverse)

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
        # extract the numbers
        str_extract(
          average_review_rating, 
          # use regular expression to 
          # look for the numbers
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

# clean also_bought dataframes












