library(tidyverse)
who
#> # A tibble: 7,240 × 60
#>       country  iso2  iso3  year new_sp_m014 new_sp_m1524 new_sp_m2534
#>         <chr> <chr> <chr> <int>       <int>        <int>        <int>
#> 1 Afghanistan    AF   AFG  1980          NA           NA           NA
#> 2 Afghanistan    AF   AFG  1981          NA           NA           NA
#> 3 Afghanistan    AF   AFG  1982          NA           NA           NA
#> 4 Afghanistan    AF   AFG  1983          NA           NA           NA
#> 5 Afghanistan    AF   AFG  1984          NA           NA           NA
#> 6 Afghanistan    AF   AFG  1985          NA           NA           NA
#> # ... with 7,234 more rows, and 53 more variables: new_sp_m3544 <int>,
#> #   new_sp_m4554 <int>, new_sp_m5564 <int>, new_sp_m65 <int>,
#> #   new_sp_f014 <int>, new_sp_f1524 <int>, new_sp_f2534 <int>,
#> #   new_sp_f3544 <int>, new_sp_f4554 <int>, new_sp_f5564 <int>,
#> #   new_sp_f65 <int>, new_sn_m014 <int>, new_sn_m1524 <int>,
#> #   new_sn_m2534 <int>, new_sn_m3544 <int>, new_sn_m4554 <int>,
#> #   new_sn_m5564 <int>, new_sn_m65 <int>, new_sn_f014 <int>,
#> #   new_sn_f1524 <int>, new_sn_f2534 <int>, new_sn_f3544 <int>,
#> #   new_sn_f4554 <int>, new_sn_f5564 <int>, new_sn_f65 <int>,
#> #   new_ep_m014 <int>, new_ep_m1524 <int>, new_ep_m2534 <int>,
#> #   new_ep_m3544 <int>, new_ep_m4554 <int>, new_ep_m5564 <int>,
#> #   new_ep_m65 <int>, new_ep_f014 <int>, new_ep_f1524 <int>,
#> #   new_ep_f2534 <int>, new_ep_f3544 <int>, new_ep_f4554 <int>,
#> #   new_ep_f5564 <int>, new_ep_f65 <int>, newrel_m014 <int>,
#> #   newrel_m1524 <int>, newrel_m2534 <int>, newrel_m3544 <int>,
#> #   newrel_m4554 <int>, newrel_m5564 <int>, newrel_m65 <int>,
#> #   newrel_f014 <int>, newrel_f1524 <int>, newrel_f2534 <int>,
#> #   newrel_f3544 <int>, newrel_f4554 <int>, newrel_f5564 <int>,
#> #   newrel_f65 <int>

who1 <- who %>% 
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)
who1
#> # A tibble: 76,046 × 6
#>       country  iso2  iso3  year         key cases
#> *       <chr> <chr> <chr> <int>       <chr> <int>
#> 1 Afghanistan    AF   AFG  1997 new_sp_m014     0
#> 2 Afghanistan    AF   AFG  1998 new_sp_m014    30
#> 3 Afghanistan    AF   AFG  1999 new_sp_m014     8
#> 4 Afghanistan    AF   AFG  2000 new_sp_m014    52
#> 5 Afghanistan    AF   AFG  2001 new_sp_m014   129
#> 6 Afghanistan    AF   AFG  2002 new_sp_m014    90
#> # ... with 7.604e+04 more rows

who1 %>% 
  count(key)
#> # A tibble: 56 × 2
#>            key     n
#>          <chr> <int>
#> 1  new_ep_f014  1032
#> 2 new_ep_f1524  1021
#> 3 new_ep_f2534  1021
#> 4 new_ep_f3544  1021
#> 5 new_ep_f4554  1017
#> 6 new_ep_f5564  1017
#> # ... with 50 more rows

who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2
#> # A tibble: 76,046 × 6
#>       country  iso2  iso3  year         key cases
#>         <chr> <chr> <chr> <int>       <chr> <int>
#> 1 Afghanistan    AF   AFG  1997 new_sp_m014     0
#> 2 Afghanistan    AF   AFG  1998 new_sp_m014    30
#> 3 Afghanistan    AF   AFG  1999 new_sp_m014     8
#> 4 Afghanistan    AF   AFG  2000 new_sp_m014    52
#> 5 Afghanistan    AF   AFG  2001 new_sp_m014   129
#> 6 Afghanistan    AF   AFG  2002 new_sp_m014    90
#> # ... with 7.604e+04 more rows

who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")
who3
#> # A tibble: 76,046 × 8
#>       country  iso2  iso3  year   new  type sexage cases
#> *       <chr> <chr> <chr> <int> <chr> <chr>  <chr> <int>
#> 1 Afghanistan    AF   AFG  1997   new    sp   m014     0
#> 2 Afghanistan    AF   AFG  1998   new    sp   m014    30
#> 3 Afghanistan    AF   AFG  1999   new    sp   m014     8
#> 4 Afghanistan    AF   AFG  2000   new    sp   m014    52
#> 5 Afghanistan    AF   AFG  2001   new    sp   m014   129
#> 6 Afghanistan    AF   AFG  2002   new    sp   m014    90
#> # ... with 7.604e+04 more rows

who3 %>% 
  count(new)
#> # A tibble: 1 × 2
#>     new     n
#>   <chr> <int>
#> 1   new 76046
who4 <- who3 %>% 
  select(-new, -iso2, -iso3)

who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)
who5
#> # A tibble: 76,046 × 6
#>       country  year  type   sex   age cases
#> *       <chr> <int> <chr> <chr> <chr> <int>
#> 1 Afghanistan  1997    sp     m   014     0
#> 2 Afghanistan  1998    sp     m   014    30
#> 3 Afghanistan  1999    sp     m   014     8
#> 4 Afghanistan  2000    sp     m   014    52
#> 5 Afghanistan  2001    sp     m   014   129
#> 6 Afghanistan  2002    sp     m   014    90
#> # ... with 7.604e+04 more rows

who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
  mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
  separate(code, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)
##exercise1
##In this case study I set na.rm = TRUE just to make it easier 
##to check that we had the correct values. Is this reasonable? 
##Think about how missing values are represented in this dataset. 
##Are there implicit missing values? What’s the difference 
##between an NA and zero?
who1 %>%
  filter(cases == 0) %>%
  nrow()
gather(who, new_sp_m014:newrel_f65, key = "key", value = "cases") %>%
  group_by(country, year)  %>%
  mutate(missing = is.na(cases)) %>%
  select(country, year, missing) %>%
  distinct() %>%
  group_by(country, year) %>%
  filter(n() > 1)
##exercise2
##What happens if you neglect the mutate() step? 
##(mutate(key = stringr::str_replace(key, "newrel", "new_rel")))
who3a <- who1 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")
filter(who3a, new == "newrel") %>% head()
##exercise3
##I claimed that iso2 and iso3 were redundant with country. 
##Confirm this claim.
select(who3, country, iso2, iso3) %>%
  distinct() %>%
  group_by(country) %>%
  filter(n() > 1)
##exercise4
##For each country, year, and sex compute the total number 
##of cases of TB. Make an informative visualization of the data.
who5 %>%
  group_by(country, year, sex) %>%
  filter(year > 1995) %>%
  summarise(cases = sum(cases)) %>%
  unite(country_sex, country, sex, remove = FALSE) %>%
  ggplot(aes(x = year, y = cases, group = country_sex, colour = sex)) +
  geom_line()
