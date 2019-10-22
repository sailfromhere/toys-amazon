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
  # separate into 10 qa sets
  separate(customer_questions_and_answers,
           into = c(paste0("question_set", 1:10)),
           sep = " \\| ")

df2 <- df1 %>%
  # gather qa sets into clean format
  pivot_longer(c(paste0("question_set", 1:10)),
               names_to = "question_sets",
               values_to = "question_answer") %>%
  # drop na columns
  drop_na()

df3 <- df2 %>%
  # separate each qa set into question & answer
  separate("question_answer",
           into = c("question", "answer"),
           sep = " // ")

head(df3)
qa <- df3

# clean reviews dataframe

df1 <- reviews %>%
  # separate into review sets
  separate(customer_reviews,
           into = c(paste0("review_set", 1:8)),
           sep = " \\| ") %>%
  # gather new columns into clean format
  pivot_longer(c(paste0("review_set", 1:8)),
               names_to = "review_sets",
               values_to = "tmp") %>%
  # drop na rows
  drop_na() %>%
  # separate each review set into components
  separate(tmp,
           into = c("review_title", 
                    "review_rating",
                    "review_date",
                    "reviewer",
                    "review"),
           sep = " // ") %>%
  # separate reviewer from irrelevant info
  separate(reviewer,
           into = c("t1", "t2",
                    "reviewer",
                    "t3", "cred"),
           sep = "\n") %>%
  # separate actual reviewer credentials
  separate(cred,
           into = c("reviewer_credential",
                    "t4"),
           sep = (" on ")) %>%
  # drop temporary columns
  select(-c(t1, t2, t3, t4)) %>%
  # parse rating as numeric
  mutate(review_rating = parse_number(review_rating)) %>%
  # parse date as date using lubridate
  mutate(review_date = dmy(review_date))

# remove white space before reviewer names
df1$reviewer <- df1$reviewer %>%
  str_replace("    ", "")

# replace credential with NA if cell is empty
for (i in 1:nrow(df1)) {
  df1[i, "reviewer_credential"] <-
    ifelse(df1[i, "reviewer_credential"] != "",
           df1[i, "reviewer_credential"],
           NA)
}

head(df1)
reviews <- df1

# clean sellers dataframe

# separate into 20 seller sets
df1 <- sellers %>%
  separate(sellers,
           into = c("temp",
                    paste0("seller_set", 1:20)),
           sep = "Seller_name") %>%
  select(-temp)

# gather sellers
df2 <- df1 %>%
  pivot_longer(c(paste0("seller_set", 1:20)), 
               names_to = "seller_sets", 
               values_to = "col") %>%
  drop_na()

# separate seller name and price
df3 <- df2 %>%
  separate(col, into = c("seller", "price"),
           sep = '\\"\\, \\"')

# clean up seller name
df3$seller <- str_extract(df3$seller, 
                          '(?<=\\"\\=\\>\\").*')

# clean up seller price
df3$price <- as.numeric(
  str_extract(df3$price,
              '(?<=£).*(?=\\"\\})')
)

head(df3)
sellers <- df3

# export

write_csv(also_bought, "also_bought.csv")
write_csv(bought_after, "bought_after.csv")
write_csv(cat, "category.csv")
write_csv(product, "product.csv")
write_csv(qa, "question_answer.csv")
write_csv(reviews, "reviews.csv")
write_csv(sellers, "sellers.csv")

# clean up environment
rm(list = c(ls()))





