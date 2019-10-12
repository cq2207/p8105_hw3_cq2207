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
    filter (topic == "overall health") %>%
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

``` r
#Load, tidy, and otherwise wrangle the data. Your final dataset should include all originally observed variables and values; have useful variable names; include a weekday vs weekend variable; and encode data with reasonable variable classes. Describe the resulting dataset (e.g. what variables exist, how many observations, etc).

accel_data = 
    read_csv("./data/accel_data.csv") %>%
    janitor::clean_names() %>%
    mutate_all(tolower)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   day = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
accel_data
```

    ## # A tibble: 35 x 1,443
    ##    week  day_id day   activity_1 activity_2 activity_3 activity_4
    ##    <chr> <chr>  <chr> <chr>      <chr>      <chr>      <chr>     
    ##  1 1     1      frid… 88.377777… 82.244444… 64.444444… 70.044444…
    ##  2 1     2      mond… 1          1          1          1         
    ##  3 1     3      satu… 1          1          1          1         
    ##  4 1     4      sund… 1          1          1          1         
    ##  5 1     5      thur… 47.355555… 48.777777… 46.888888… 35.8      
    ##  6 1     6      tues… 64.822222… 59.511111… 73.688888… 45.711111…
    ##  7 1     7      wedn… 71.066666… 103.11111… 68.511111… 45.4      
    ##  8 2     8      frid… 675        542        1010       779       
    ##  9 2     9      mond… 291        335        393        335       
    ## 10 2     10     satu… 64         11         1          1         
    ## # … with 25 more rows, and 1,436 more variables: activity_5 <chr>,
    ## #   activity_6 <chr>, activity_7 <chr>, activity_8 <chr>,
    ## #   activity_9 <chr>, activity_10 <chr>, activity_11 <chr>,
    ## #   activity_12 <chr>, activity_13 <chr>, activity_14 <chr>,
    ## #   activity_15 <chr>, activity_16 <chr>, activity_17 <chr>,
    ## #   activity_18 <chr>, activity_19 <chr>, activity_20 <chr>,
    ## #   activity_21 <chr>, activity_22 <chr>, activity_23 <chr>,
    ## #   activity_24 <chr>, activity_25 <chr>, activity_26 <chr>,
    ## #   activity_27 <chr>, activity_28 <chr>, activity_29 <chr>,
    ## #   activity_30 <chr>, activity_31 <chr>, activity_32 <chr>,
    ## #   activity_33 <chr>, activity_34 <chr>, activity_35 <chr>,
    ## #   activity_36 <chr>, activity_37 <chr>, activity_38 <chr>,
    ## #   activity_39 <chr>, activity_40 <chr>, activity_41 <chr>,
    ## #   activity_42 <chr>, activity_43 <chr>, activity_44 <chr>,
    ## #   activity_45 <chr>, activity_46 <chr>, activity_47 <chr>,
    ## #   activity_48 <chr>, activity_49 <chr>, activity_50 <chr>,
    ## #   activity_51 <chr>, activity_52 <chr>, activity_53 <chr>,
    ## #   activity_54 <chr>, activity_55 <chr>, activity_56 <chr>,
    ## #   activity_57 <chr>, activity_58 <chr>, activity_59 <chr>,
    ## #   activity_60 <chr>, activity_61 <chr>, activity_62 <chr>,
    ## #   activity_63 <chr>, activity_64 <chr>, activity_65 <chr>,
    ## #   activity_66 <chr>, activity_67 <chr>, activity_68 <chr>,
    ## #   activity_69 <chr>, activity_70 <chr>, activity_71 <chr>,
    ## #   activity_72 <chr>, activity_73 <chr>, activity_74 <chr>,
    ## #   activity_75 <chr>, activity_76 <chr>, activity_77 <chr>,
    ## #   activity_78 <chr>, activity_79 <chr>, activity_80 <chr>,
    ## #   activity_81 <chr>, activity_82 <chr>, activity_83 <chr>,
    ## #   activity_84 <chr>, activity_85 <chr>, activity_86 <chr>,
    ## #   activity_87 <chr>, activity_88 <chr>, activity_89 <chr>,
    ## #   activity_90 <chr>, activity_91 <chr>, activity_92 <chr>,
    ## #   activity_93 <chr>, activity_94 <chr>, activity_95 <chr>,
    ## #   activity_96 <chr>, activity_97 <chr>, activity_98 <chr>,
    ## #   activity_99 <chr>, activity_100 <chr>, activity_101 <chr>,
    ## #   activity_102 <chr>, activity_103 <chr>, activity_104 <chr>, …
