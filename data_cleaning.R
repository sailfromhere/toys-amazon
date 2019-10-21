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









