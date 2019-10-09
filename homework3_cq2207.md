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
    ## ✔ tidyr   0.8.3     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
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
  mutate (count = min_rank(desc(count))) %>%
  filter (count <= 3) 

product_table <- spread(product_count,count,product_name) %>% 
knitr::kable(col.names = c("Aisle", "1º Item", "2nd Item", "3rd Item")) %>% 
    print()
```

    ## 
    ## 
    ## Aisle                        1º Item                                         2nd Item                              3rd Item            
    ## ---------------------------  ----------------------------------------------  ------------------------------------  --------------------
    ## baking ingredients           Light Brown Sugar                               Pure Baking Soda                      Cane Sugar          
    ## dog food care                Snack Sticks Chicken & Rice Recipe Dog Treats   Organix Chicken & Brown Rice Recipe   Small Dog Biscuits  
    ## packaged vegetables fruits   Organic Baby Spinach                            Organic Raspberries                   Organic Blueberries
