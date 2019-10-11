homework3\_cq2207
================
Carolina Q Cardoso
10/9/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(ggplot2)

library(p8105.datasets)
data("instacart")
```

The dataset `Instacart` contains 1384617 observations and 15 variables.
Variables describe details of the order and of the products
ordered.

``` r
#How many aisles are there, and which aisles are the most items ordered from?

aisle_count = instacart %>%
  group_by(aisle_id, aisle) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
```

There are 134 different aisles. The aisle with the highest number of
ordered items in “fresh vegetables” with 150609 items, followed by
“fresh fruits” with 150473 ordered
items.

``` r
#Make a plot that shows the number of items ordered in each aisle, limiting this to aisles with more than 10000 items ordered. Arrange aisles sensibly, and organize your plot so others can read it.

library(ggplot2)

aisle_count %>%
  filter(count > 10000) %>%
  ggplot(aes(x = aisle_id, y = count)) + 
    geom_point() + geom_line() + 
    labs(
    title = "Number of Items Ordered by Aisle (n > 10000)",
    x = "Aisle",
    y = "Total Number of Items Ordered")
```

![](homework3_cq2207_files/figure-gfm/-%20problem1_cont2-1.png)<!-- -->

``` r
#Make a table showing the three most popular items in each of the aisles “baking ingredients”, “dog food care”, and “packaged vegetables fruits”. Include the number of times each item is ordered in your table.

library(knitr)

product_count = instacart %>%
  group_by(aisle, product_name) %>%
  summarize (count = n()) %>%
  filter(aisle == "baking ingredients" | aisle == "dog food care" | aisle == "packaged vegetables fruits") %>%
  mutate (rank = min_rank(desc(count))) %>%
  filter (rank <= 3) %>%
  select(-rank) %>% 
  knitr::kable() %>% 
    print()
```

    ## 
    ## 
    ## aisle                        product_name                                     count
    ## ---------------------------  ----------------------------------------------  ------
    ## baking ingredients           Cane Sugar                                         336
    ## baking ingredients           Light Brown Sugar                                  499
    ## baking ingredients           Pure Baking Soda                                   387
    ## dog food care                Organix Chicken & Brown Rice Recipe                 28
    ## dog food care                Small Dog Biscuits                                  26
    ## dog food care                Snack Sticks Chicken & Rice Recipe Dog Treats       30
    ## packaged vegetables fruits   Organic Baby Spinach                              9784
    ## packaged vegetables fruits   Organic Blueberries                               4966
    ## packaged vegetables fruits   Organic Raspberries                               5546

``` r
#Make a table showing the mean hour of the day at which Pink Lady Apples and Coffee Ice Cream are ordered on each day of the week; format this table for human readers (i.e. produce a 2 x 7 table)
  

library(tidyverse)
library(tidyr)

apple_coffee = instacart %>% 
  group_by (product_name, order_dow) %>%
  summarize(mean_hour = mean(order_hour_of_day)) %>%
  filter(product_name == "Pink Lady Apples" | product_name == "Coffee Ice Cream")%>%
  pivot_wider(names_from = order_dow, values_from = mean_hour) %>%
  knitr::kable(col.names = c("Product", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) %>% 
    print()
```

    ## 
    ## 
    ## Product                  Sun        Mon        Tue        Wed        Thu        Fri        Sat
    ## -----------------  ---------  ---------  ---------  ---------  ---------  ---------  ---------
    ## Coffee Ice Cream    13.77419   14.31579   15.38095   15.31818   15.21739   12.26316   13.83333
    ## Pink Lady Apples    13.44118   11.36000   11.70213   14.25000   11.55172   12.78431   11.93750

``` r
data("brfss_smart2010")

brfss_new = brfss_smart2010 %>% 
    janitor::clean_names() %>%
    mutate_all(tolower) %>%
    filter (question == "how is your general health?") %>%
    mutate(response = factor(response, labels = c("poor","fair","good","very good","excellent")))
    
 #In 2002, which states were observed at 7 or more locations? What about in 2010?

brfss_2002 = brfss_new %>%
    group_by(year, locationabbr) %>%
    summarize(n = n_distinct(geo_location)) %>%
    filter (n > 6, year == '2002')

#In 2002, the states observed in 7 or more locations were CT, FL, MA, NC, NJ, PA

brfss_2010 = brfss_new %>%
    group_by(year, locationabbr) %>%
    summarize(n = n_distinct(geo_location)) %>%
    filter (n > 6, year == '2010')
    
 #In 2010, the states observed in 7 or more locations were CA, CO, FL, MA, MD, NC, NE, NJ, NY, OH, PA, SC, TX, WA 
```

In 2002, there were 6 states that were observed in 7 or more locations:
ct, fl, ma, nc, nj, and pa. In 2010, there were 14 states that were
observed in 7 or more locations: ca, co, fl, ma, md, nc, ne, nj, ny, oh,
pa, sc, tx, and
wa.

``` r
#Construct a dataset that is limited to Excellent responses, and contains, year, state, and a variable that averages the data_value across locations within a state. Make a “spaghetti” plot of this average value over time within a state (that is, make a plot showing a line for each state across years – the geom_line geometry and group aesthetic will help).

brfss_excellent = brfss_new %>%
  transform(data_value = as.numeric(data_value))%>%
    filter (response == 'excellent') %>%
    group_by(locationabbr, year, response) %>%
    summarize(mean_data = mean(data_value))

brfss_excellent %>% 
    ggplot(aes(x = year, y = mean_data, group = locationabbr, color = locationabbr)) + 
    geom_point() + geom_line() +
    labs(
    title = "Average value over time by state",
    x = "Year",
    y = "Average Data Value"
  )
```

    ## Warning: Removed 6 rows containing missing values (geom_point).

    ## Warning: Removed 2 rows containing missing values (geom_path).

![](homework3_cq2207_files/figure-gfm/problem2_cont-1.png)<!-- -->

``` r
#Make a two-panel plot showing, for the years 2006, and 2010, distribution of data_value for responses (“Poor” to “Excellent”) among locations in NY State.

brfss_two = brfss_new %>%
    transform(data_value = as.numeric(data_value)) %>%
    filter(locationabbr == 'ny',year == '2006' | year == '2010') %>%
    select(locationabbr, year, response, data_value)

brfss_two %>%
    ggplot(aes(x = response, y = data_value, color = response)) + 
    geom_boxplot() + 
    facet_grid(. ~ year) + 
    labs(
    title = "Distribution of Data Values per Response in NY State",
    x = "Response",
    y = "Data Value")
```

![](homework3_cq2207_files/figure-gfm/problem2_cont-2.png)<!-- -->
