amazon |> 
  ggplot(aes(x = rating_count)) + 
  geom_histogram() + 
  scale_x_log10(breaks = c(0, 10, 100, 1000, 10000, 100000), 
                labels = comma) + 
  labs(title = "Histogram of number of Amazon ratings per product", 
       x = "Number of ratings", 
       y = "Number of products") 

amazon |> 
  group_by(category, sub_category, rating) |> 
  summarise(
    rating_count = sum(rating_count), 
    discounted_price = mean(discounted_price, na.rm = TRUE), 
    .groups = "drop") |> 
  mutate(rating_total = rating * rating_count, 
         price_total = discounted_price * rating_count) |> 
  group_by(category, sub_category) |> 
  summarise(
    rating_total = sum(rating_total), 
    price_total = sum(price_total), 
    rating_count = sum(rating_count), 
    .groups = "drop"
  ) |> 
  mutate(mean_rating = rating_total / rating_count, 
         mean_discounted_price = price_total / rating_count) |> 
  ggplot(aes( x = mean_discounted_price, 
              y = mean_rating)) + 
  geom_point(aes(colour = category)) + 
  geom_text_repel(aes(label = sub_category), 
                  size = 2) +
  scale_x_log10() + 
  scale_color_viridis_d(option = "turbo") + 
  labs(x = "Mean discounted price (USD)", 
       y = "Mean rating", 
       title = "Mean ratings and prices of Amazon products by category", 
       subtitle = "Tablets, power accessories and computer components are the most highly-reviewed", 
       colour = "Category")

# Maybe try something with crosstalk 
library(ggplot2)
library(plotly)
library(crosstalk)

# 1. Wrap your data in a SharedData object
shared_mtcars <- SharedData$new(mtcars)

# 2. Create the filter widget
filter_dropdown <- filter_select(
  id = "cyl_filter", 
  label = "Select Cylinders:", 
  sharedData = shared_mtcars, 
  group = ~cyl
)

# 3. Build your ggplot using the shared data
p <- ggplot(shared_mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_minimal()

# 4. Convert to ggplotly
gg_plotly <- ggplotly(p)

# 5. Combine the widget and the plot together
bscols(filter_dropdown, gg_plotly)

# Bigrams are probably better
amazon_words <- amazon |> 
  select(review_id, review_content, user_id, category, sub_category, product_type) |>
  unnest_tokens(word, review_content) |> 
  anti_join(stop_words, by = "word") |> 
  filter(str_detect(word, "[a-z]")) 

amazon_words |>
  filter(word %out% c("product")) |>
  add_count(word, sub_category) |> 
  distinct(word, sub_category, n) |> 
  group_by(sub_category) |> 
  slice_max(order_by = n, n = 10) |> 
  ggplot(aes(x = n, y = reorder_within(word, by = n, within = sub_category))) + 
  geom_col(aes(fill = sub_category)) + 
  facet_wrap(~ sub_category, scales = "free") +  
  scale_y_reordered() + 
  scale_fill_viridis_d() +
  theme(legend.position = "none")