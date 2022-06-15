library(h2o)

# To launch H2O locally with default initialization arguments, use the following: 
h2o.init()

library(tidyverse)
hp_by_cyl <- mtcars %>% 
  group_by(cyl) %>%
  summarize(min_hp=min(hp),
            max_hp=max(hp))
hp_by_cyl


groupby_var <- "vs"

hp_by_vs <- mtcars %>% 
  group_by(groupby_var) %>%
  summarize(min_hp=min(hp),
            max_hp=max(hp))
## Error: Must group by variables found in `.data`.
## * Column `groupby_var` is not found.
## Run `rlang::last_error()` to see where the error occurred.

groupby_var <- quo(vs)

hp_by_vs <- mtcars %>% 
  group_by(!!groupby_var) %>%
  summarize(min_hp=min(hp),
            max_hp=max(hp))
hp_by_vs

car_stats <- function(groupby_var, measure_var) {
  
  groupby_var <- enquo(groupby_var)
  measure_var <- enquo(measure_var)
  
  ret <- mtcars %>% 
    
    group_by(!!groupby_var) %>%
    summarize(min = min(!!measure_var), max = max(!!measure_var)) %>%
    
    # Optional: as_label() and "walrus operator" :=
    mutate(
      measure_var = as_label(measure_var), !!measure_var := "test"
    )
  
  return(ret)
  
}
car_stats(am,hp)
## # A tibble: 2 x 5
##      am   min   max measure_var hp   
##   <dbl> <dbl> <dbl> <chr>       <chr>
## 1     0    62   245 hp          test 
## 2     1    52   335 hp          test 

car_stats(gear,cyl)

scatter_plot <- function(data, x_var, y_var) {
  
  x_var <- enquo(x_var)
  y_var <- enquo(y_var)
  
  ret <- data %>% 
    ggplot(aes(x = !!x_var, y = !!y_var)) + 
    geom_point() + 
    geom_smooth() +
    ggtitle(str_c(as_label(y_var), " vs. ",as_label(x_var)))
  
  return(ret)
}
scatter_plot(mtcars, disp, hp)
