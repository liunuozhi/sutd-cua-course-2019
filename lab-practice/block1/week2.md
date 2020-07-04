week2
================
Ronnie
2/6/2020

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## Warning: package 'tidyr' was built under R version 3.6.2

    ## -- Conflicts ------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(forcats)
```

``` r
sales <- read_csv(here::here("data/resale-flat-prices-based-on-registration-date-from-jan-2015-onwards.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   month = col_character(),
    ##   town = col_character(),
    ##   flat_type = col_character(),
    ##   block = col_character(),
    ##   street_name = col_character(),
    ##   storey_range = col_character(),
    ##   floor_area_sqm = col_double(),
    ##   flat_model = col_character(),
    ##   lease_commence_date = col_double(),
    ##   remaining_lease = col_double(),
    ##   resale_price = col_double()
    ## )

``` r
sales %>% glimpse()
```

    ## Observations: 79,100
    ## Variables: 11
    ## $ month               <chr> "2015-01", "2015-01", "2015-01", "2015-01", "20...
    ## $ town                <chr> "ANG MO KIO", "ANG MO KIO", "ANG MO KIO", "ANG ...
    ## $ flat_type           <chr> "3 ROOM", "3 ROOM", "3 ROOM", "3 ROOM", "3 ROOM...
    ## $ block               <chr> "174", "541", "163", "446", "557", "603", "709"...
    ## $ street_name         <chr> "ANG MO KIO AVE 4", "ANG MO KIO AVE 10", "ANG M...
    ## $ storey_range        <chr> "07 TO 09", "01 TO 03", "01 TO 03", "01 TO 03",...
    ## $ floor_area_sqm      <dbl> 60, 68, 69, 68, 68, 67, 68, 68, 67, 68, 67, 68,...
    ## $ flat_model          <chr> "Improved", "New Generation", "New Generation",...
    ## $ lease_commence_date <dbl> 1986, 1981, 1980, 1979, 1980, 1980, 1980, 1981,...
    ## $ remaining_lease     <dbl> 70, 65, 64, 63, 64, 64, 64, 65, 62, 69, 60, 64,...
    ## $ resale_price        <dbl> 255000, 275000, 285000, 290000, 290000, 290000,...

``` r
sales <- sales %>% 
  mutate(month = ymd(month, truncated = 1),
         flat_type = as_factor(flat_type),
         storey_range = as_factor(storey_range),
         flat_model = as_factor(flat_model))
```

What is the earlier lease commencement date and where are these flats
located?

``` r
sales %>% arrange(desc(lease_commence_date)) %>% 
  select(town, lease_commence_date)
```

    ## # A tibble: 79,100 x 2
    ##    town        lease_commence_date
    ##    <chr>                     <dbl>
    ##  1 BUKIT MERAH                2016
    ##  2 BUKIT MERAH                2016
    ##  3 BUKIT MERAH                2016
    ##  4 BUKIT MERAH                2016
    ##  5 BUKIT MERAH                2016
    ##  6 BUKIT MERAH                2016
    ##  7 BUKIT MERAH                2016
    ##  8 BUKIT MERAH                2016
    ##  9 BUKIT MERAH                2016
    ## 10 QUEENSTOWN                 2015
    ## # ... with 79,090 more rows

What are the largest HDB flats in Singapore? How much did they sell for?

``` r
sales %>% 
  arrange(floor_area_sqm) %>% 
  select(town, floor_area_sqm)
```

    ## # A tibble: 79,100 x 2
    ##    town        floor_area_sqm
    ##    <chr>                <dbl>
    ##  1 BUKIT MERAH             31
    ##  2 BUKIT MERAH             31
    ##  3 BUKIT MERAH             31
    ##  4 BUKIT MERAH             31
    ##  5 BUKIT MERAH             31
    ##  6 BUKIT MERAH             31
    ##  7 BUKIT MERAH             31
    ##  8 BUKIT MERAH             31
    ##  9 BUKIT MERAH             31
    ## 10 BUKIT MERAH             31
    ## # ... with 79,090 more rows

What is the most expensive flat in Punggol?

``` r
sales %>% 
  filter(town == "PUNGGOL") %>% 
  arrange(desc(resale_price)) %>% 
  select(town, block, street_name, resale_price)
```

    ## # A tibble: 4,386 x 4
    ##    town    block street_name   resale_price
    ##    <chr>   <chr> <chr>                <dbl>
    ##  1 PUNGGOL 305B  PUNGGOL RD          870000
    ##  2 PUNGGOL 267A  PUNGGOL FIELD       860000
    ##  3 PUNGGOL 270A  PUNGGOL FIELD       835000
    ##  4 PUNGGOL 306D  PUNGGOL DR          800000
    ##  5 PUNGGOL 305A  PUNGGOL RD          788000
    ##  6 PUNGGOL 305D  PUNGGOL DR          780000
    ##  7 PUNGGOL 306A  PUNGGOL PL          760000
    ##  8 PUNGGOL 272A  PUNGGOL WALK        728000
    ##  9 PUNGGOL 271C  PUNGGOL WALK        725000
    ## 10 PUNGGOL 270B  PUNGGOL FIELD       722800
    ## # ... with 4,376 more rows

Which town has, on average, the largest flats (by floor area)?

``` r
sales %>% 
  group_by(town) %>% 
  summarise(average_area = mean(floor_area_sqm)) %>% 
  arrange(desc(average_area))
```

    ## # A tibble: 26 x 2
    ##    town          average_area
    ##    <chr>                <dbl>
    ##  1 PASIR RIS             123.
    ##  2 CHOA CHU KANG         111.
    ##  3 BUKIT TIMAH           110.
    ##  4 WOODLANDS             107.
    ##  5 SEMBAWANG             107.
    ##  6 BISHAN                107.
    ##  7 TAMPINES              105.
    ##  8 BUKIT PANJANG         105.
    ##  9 JURONG WEST           102.
    ## 10 HOUGANG               102.
    ## # ... with 16 more rows

Which town has, on average, the cheapest flats per square meter?

``` r
sales %>%
  mutate(
    price_per_sqr_m = resale_price / floor_area_sqm
  ) %>% 
  group_by(town) %>% 
  summarise(average_price_per_sqr = mean(price_per_sqr_m)) %>% 
  arrange(average_price_per_sqr)
```

    ## # A tibble: 26 x 2
    ##    town          average_price_per_sqr
    ##    <chr>                         <dbl>
    ##  1 CHOA CHU KANG                 3529.
    ##  2 WOODLANDS                     3625.
    ##  3 SEMBAWANG                     3729.
    ##  4 YISHUN                        3907.
    ##  5 PASIR RIS                     3920.
    ##  6 JURONG WEST                   3928.
    ##  7 BUKIT PANJANG                 4016.
    ##  8 BUKIT BATOK                   4126.
    ##  9 HOUGANG                       4182.
    ## 10 SENGKANG                      4341.
    ## # ... with 16 more rows
