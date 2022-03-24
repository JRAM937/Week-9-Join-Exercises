Join Data with dplyr
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.2.0     ✓ stringr 1.4.0
    ## ✓ readr   2.1.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(nycflights13)

band <- tribble(
   ~name,     ~band,
  "Mick",  "Stones",
  "John", "Beatles",
  "Paul", "Beatles"
)

instrument <- tribble(
    ~name,   ~plays,
   "John", "guitar",
   "Paul",   "bass",
  "Keith", "guitar"
)

instrument2 <- tribble(
    ~artist,   ~plays,
   "John", "guitar",
   "Paul",   "bass",
  "Keith", "guitar"
)
```

# nycflights13

``` r
View(flights)
```

``` r
View(airlines)
```

# mutating joins

``` r
band %>% left_join(instrument, by = "name")
```

    ## # A tibble: 3 × 3
    ##   name  band    plays 
    ##   <chr> <chr>   <chr> 
    ## 1 Mick  Stones  <NA>  
    ## 2 John  Beatles guitar
    ## 3 Paul  Beatles bass

``` r
band %>% right_join(instrument, by = "name")
```

    ## # A tibble: 3 × 3
    ##   name  band    plays 
    ##   <chr> <chr>   <chr> 
    ## 1 John  Beatles guitar
    ## 2 Paul  Beatles bass  
    ## 3 Keith <NA>    guitar

``` r
band %>% full_join(instrument, by = "name")
```

    ## # A tibble: 4 × 3
    ##   name  band    plays 
    ##   <chr> <chr>   <chr> 
    ## 1 Mick  Stones  <NA>  
    ## 2 John  Beatles guitar
    ## 3 Paul  Beatles bass  
    ## 4 Keith <NA>    guitar

``` r
band %>% inner_join(instrument, by = "name")
```

    ## # A tibble: 2 × 3
    ##   name  band    plays 
    ##   <chr> <chr>   <chr> 
    ## 1 John  Beatles guitar
    ## 2 Paul  Beatles bass

## Your Turn 1a

Which airlines had the largest arrival delays? Complete the code below.

1.  Join `airlines` to `flights`

*(Hint: Be sure to remove each `_` before running the code)*

``` r
flights %>%
  drop_na(arr_delay) %>%
  left_join(airlines, by = "carrier")  %>%
  head() 
```

    ## # A tibble: 6 × 20
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ## 1  2013     1     1      517            515         2      830            819
    ## 2  2013     1     1      533            529         4      850            830
    ## 3  2013     1     1      542            540         2      923            850
    ## 4  2013     1     1      544            545        -1     1004           1022
    ## 5  2013     1     1      554            600        -6      812            837
    ## 6  2013     1     1      554            558        -4      740            728
    ## # … with 12 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>, name <chr>

## Your Turn 1b

Which airlines had the largest arrival delays? Complete the code below.

1.  Join `airlines` to `flights`
2.  Compute and order the average arrival delays by airline. Display
    full names, no codes.

*(Hint: Be sure to remove each `_` before running the code)*

``` r
flights %>%
  drop_na(arr_delay) %>%
  left_join(airlines, by = "carrier")  %>%
  group_by(name) %>%
  summarise(mean_arr_delay=mean(arr_delay))  %>%
  arrange(desc(mean_arr_delay)) 
```

    ## # A tibble: 16 × 2
    ##    name                        mean_arr_delay
    ##    <chr>                                <dbl>
    ##  1 Frontier Airlines Inc.              21.9  
    ##  2 AirTran Airways Corporation         20.1  
    ##  3 ExpressJet Airlines Inc.            15.8  
    ##  4 Mesa Airlines Inc.                  15.6  
    ##  5 SkyWest Airlines Inc.               11.9  
    ##  6 Envoy Air                           10.8  
    ##  7 Southwest Airlines Co.               9.65 
    ##  8 JetBlue Airways                      9.46 
    ##  9 Endeavor Air Inc.                    7.38 
    ## 10 United Air Lines Inc.                3.56 
    ## 11 US Airways Inc.                      2.13 
    ## 12 Virgin America                       1.76 
    ## 13 Delta Air Lines Inc.                 1.64 
    ## 14 American Airlines Inc.               0.364
    ## 15 Hawaiian Airlines Inc.              -6.92 
    ## 16 Alaska Airlines Inc.                -9.93

## Your Turn 2a

Join `flights` and `airports` by `dest` and `faa`.

``` r
flights %>% 
  drop_na(arr_delay) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  head()
```

    ## # A tibble: 6 × 26
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ## 1  2013     1     1      517            515         2      830            819
    ## 2  2013     1     1      533            529         4      850            830
    ## 3  2013     1     1      542            540         2      923            850
    ## 4  2013     1     1      544            545        -1     1004           1022
    ## 5  2013     1     1      554            600        -6      812            837
    ## 6  2013     1     1      554            558        -4      740            728
    ## # … with 18 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>, name <chr>, lat <dbl>,
    ## #   lon <dbl>, alt <dbl>, tz <dbl>, dst <chr>, tzone <chr>

## Your Turn 2b

Join `flights` and `airports` by `dest` and `faa`.

Then for each `name`, compute the `distance` from NYC and the average
`arr_delay`. *Hint: use `first()` to get the first value of distance.*

Order by average delay, worst to best.

*(Hint: Be sure to remove each `_` before running the code)*

``` r
flights %>% 
  drop_na(arr_delay) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  group_by(name) %>%
  summarise(distance = first(distance), delay = mean(arr_delay)) %>%
  arrange(desc (delay))
```

    ## # A tibble: 101 × 3
    ##    name                          distance delay
    ##    <chr>                            <dbl> <dbl>
    ##  1 Columbia Metropolitan              602  41.8
    ##  2 Tulsa Intl                        1215  33.7
    ##  3 Will Rogers World                 1325  30.6
    ##  4 Jackson Hole Airport              1874  28.1
    ##  5 Mc Ghee Tyson                      631  24.1
    ##  6 Dane Co Rgnl Truax Fld             799  20.2
    ##  7 Richmond Intl                      277  20.1
    ##  8 Akron Canton Regional Airport      397  19.7
    ##  9 Des Moines Intl                   1017  19.0
    ## 10 Gerald R Ford Intl                 605  18.2
    ## # … with 91 more rows

# filtering joins

``` r
band %>% semi_join(instrument, by = "name")
```

    ## # A tibble: 2 × 2
    ##   name  band   
    ##   <chr> <chr>  
    ## 1 John  Beatles
    ## 2 Paul  Beatles

``` r
band %>% anti_join(instrument, by = "name")
```

    ## # A tibble: 1 × 2
    ##   name  band  
    ##   <chr> <chr> 
    ## 1 Mick  Stones

## Your Turn 3

How many airports in `airports` are serviced by flights in `flights`?
(i.e. how many places can you fly to direct from New York?)

Notice that the column to join on is named `faa` in the **airports**
data set and `dest` in the **flights** data set.

``` r
dim(airports)
```

    ## [1] 1458    8

``` r
airports %>%
  semi_join(flights, by = c("faa" = "dest"))%>%
  select(faa)
```

    ## # A tibble: 101 × 1
    ##    faa  
    ##    <chr>
    ##  1 ABQ  
    ##  2 ACK  
    ##  3 ALB  
    ##  4 ANC  
    ##  5 ATL  
    ##  6 AUS  
    ##  7 AVL  
    ##  8 BDL  
    ##  9 BGR  
    ## 10 BHM  
    ## # … with 91 more rows

# other functions

``` r
distinct(instrument, plays)
```

    ## # A tibble: 2 × 1
    ##   plays 
    ##   <chr> 
    ## 1 guitar
    ## 2 bass

``` r
instrument %>% distinct(plays)
```

    ## # A tibble: 2 × 1
    ##   plays 
    ##   <chr> 
    ## 1 guitar
    ## 2 bass

``` r
flights %>% 
  distinct(carrier, origin) %>% 
  arrange(carrier)
```

    ## # A tibble: 35 × 2
    ##    carrier origin
    ##    <chr>   <chr> 
    ##  1 9E      JFK   
    ##  2 9E      EWR   
    ##  3 9E      LGA   
    ##  4 AA      JFK   
    ##  5 AA      LGA   
    ##  6 AA      EWR   
    ##  7 AS      EWR   
    ##  8 B6      JFK   
    ##  9 B6      EWR   
    ## 10 B6      LGA   
    ## # … with 25 more rows

# Take aways

-   `left_join()` retains all cases in *left* data set

-   `right_join()` retains all cases in *right* data set

-   `full_join()` retains all cases in *either* data set

-   `inner_join()` retains *only* cases in *both* data sets

-   `semi_join()` extracts cases that *have* a match

-   `anti_join()` extracts cases that *do not have* a match
