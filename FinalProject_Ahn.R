## STATS506 F20 Final Project
##
## DESCRIPTION: Summarize # of TVs and most-used TV types by division and 
##   urban/rural settings for recs dataset from eia.gov for years 2009 and 
##   2015. Addiitional analysis of changes in these values from 2009 to 2015
##   was performed.
##
##   Any related plotting is done in FinalProject_Ahn.Rmd
## 
## Data Source: 
##     https://www.eia.gov/consumption/commercial/data/
##                                  2012/index.php?view=microdata
##
## Author(s): EunSeon Ahn, eunahn@umich.edu
## Updated: December 14, 2020 

# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)

# Script: ---------------------------------------------------------------------

# Load data: ------------------------------------------------------------------
## CBEC Data
cbec = read_delim('./Data/2012_public_use_data_aug2016.csv',delim = ",") %>%
  select(id = PUBID, w = FINALWT, year = YRCONC, wall = WLCNS, 
         roof = RFCNS, size = SQFTC)

## Replicate weights
weights =  read_delim('./Data/2012_public_use_data_aug2016.csv',  
                      delim = ",") %>% 
  select(id = PUBID, starts_with("FINALWT")) %>% 
  select(-FINALWT)

### Convert replicate weights to long form 

## Code taken from Dr. Henderson's lecture from 'Intro to Tidyverse' 
long_weights = weights %>%
  pivot_longer(
    cols = starts_with('FINALWT'),
    names_to = 'rep',
    names_prefix = 'FINALWT',
    values_to = 'rw'
  ) %>%
  mutate(rep = as.integer(rep),
         id = as.integer(id))

## Codebook
codebook = readxl::read_excel('./Data/2012microdata_codebook.xlsx') %>% 
  select(c(2,6,7)) 

variables = c(year = 'YRCONC', wall = 'WLCNS', roof = 'RFCNS', size = "SQFTC")

codes = codebook %>% filter( `Variable\r\nname` %in% variables) %>%
  transmute(
    variable = `Variable\r\nname`,
    temp =
      str_remove_all(`Values/Format codes`, pattern = "'")) %>%
  mutate(
    temp = str_split(temp, pattern = '\r\n')) %>%
  mutate(
    temp = lapply(temp, str_split_fixed, pattern = " = ", n = 2)) %>%
  mutate(
    levels = lapply(temp, function(m){m[,1]}),
    labels = lapply(temp, function(m){m[,2]})) %>%
  select(-temp)

codes$labels[[2]] = replace(codes$labels[[2]], c(3, 4, 6),
                            c("Concrete block or poured concrete",
                              "Aluminum, asbestos, plastic, or wood",
                              "Window or vision glass")) 

codes$labels[[3]] = replace(codes$labels[[3]], c(1, 3, 6),
                            c("Built-up (tar, felts, or fiberglass/ballast)",
                              "Wood shingles, shakes, or other wood",
                              "Plastic, rubber, or synthetic sheeting"))


## apply labels
decode_recs = function(x, varname, codes = codes){
  # Source: Dr. Henderson's Youtube lecture from 'Intro to Tidyverse'
  # apply factor labels to var using codebook "codes"
  # INPUTS: 
  #   x - input vector to be changed to a factor
  #   varname - chr vector with name of the 'variable' in 'codes'
  #   codes - codebook of factor, levels, and labels
  # OUPTUT: x converted to a factor with levels givin in the codes 
  
  with(filter(codes, variable == varname ), 
       factor(x, levels = as.numeric(levels[[1]]), labels = labels[[1]]))
}

cbec = cbec %>%
  mutate(size = as.numeric(size),
         year = as.numeric(year)) %>%
  mutate(size = decode_recs(size,'SQFTC',codes),
         wall = decode_recs(wall,'WLCNS',codes),
         roof = decode_recs(roof,'RFCNS',codes),
         year = decode_recs(year,'YRCONC', codes),
         id = as.double(id))

## Collapse across size categories for wider size categories
cbec = cbec %>% 
  mutate(size_group = 
           fct_collapse(size,size1 = c("1,001 to 5,000 square feet" ,
                                       "5,001 to 10,000 square feet",
                                       "10,001 to 25,000 square feet"),
                        size2 = c("25,001 to 50,000 square feet",
                                  "50,001 to 100,000 square feet",
                                  "100,001 to 200,000 square feet"),
                        size3 = c("200,001 to 500,000 square feet",
                                  "500,001 to 1 million square feet",
                                  "Over 1 million square feet")))

cbec = cbec %>% mutate(size_group = recode(size_group, 
                size1 = "1001 to 25,000 sq. ft.",
                size2 = "25,001 to 200,000 sq. ft.",
                size3 = "200,001 to over 1 mil. sq. ft"))

head(cbec)

gen_CI = function(df, varA, varB){
  # Construct point estimates and confidence intervals
  
  # generate pt. estimates confidence intervals for var A grouped by var B
  #     by variable B
  # INPUTS: 
  #   df - the tibble or data frame with all available values
  #   varA - variable of interest
  #   varB - variable you want to group varA by
  
  # OUPTUT: pt_CI - data frame containing variance, standard error, and lower and 
  #         upper bounds of the 95% confidence interval
  
  pt_est = df %>% group_by_at(c(varA, varB)) %>%
    summarize(nTotal = sum(w)) %>%
    mutate(p = nTotal/sum(nTotal))
  
  pt_repl = df %>% select(-w) %>%
    left_join(long_weights, by = "id") %>%
    group_by_at(c(varA, "rep", varB)) %>%
    summarize(nTotal_repl = sum(rw)) %>% 
    mutate(p_repl = nTotal_repl/sum(nTotal_repl)) %>% ungroup()
    
  # Calc. confidence intervals
  pt_var = pt_repl %>%
    select(-nTotal_repl) %>%
    left_join(pt_est, by = c(varA, varB)) %>%
    select(-nTotal) %>%
    group_by_at(c(varA, varB)) %>%
    summarize(p = mean(p), v =  sum({(p_repl - p)^2}))
  
  m = qnorm(.975)
  pt_CI = pt_var %>% 
    mutate(
      se = sqrt(v), 
      lwr = pmax(p - m * se, 0),
      upr = pmin(p + m * se, 1)) 

  return(pt_CI)
}

wall_year = gen_CI(cbec, "year", "wall") %>%
  mutate(across(all_of(c('p', 'lwr', 'upr')), 
                 .fns = function(x) 100 * x))
wall_year_size = gen_CI(cbec, c("year", "size_group"), "wall") %>%
  mutate(across(all_of(c('p', 'lwr', 'upr')), 
                .fns = function(x) 100 * x))

roof_year = gen_CI(cbec, "year", "roof") %>%
  mutate(across(all_of(c('p', 'lwr', 'upr')), 
                .fns = function(x) 100 * x))
roof_year_size = gen_CI(cbec, c("year", "size_group"), "roof") %>%
  mutate(across(all_of(c('p', 'lwr', 'upr')), 
                .fns = function(x) 100 * x))

# Construct point estimates ---------------------------------------------------
## Wall construction material
### Calculate mean proportion
wall_by_yr = cbec %>% group_by(year, wall) %>%
  summarize(nbuildings = sum(w), n()) %>% 
  mutate(p_wall = nbuildings/sum(nbuildings)) %>% filter(wall == c("Brick, stone, or stucco"))

### Calculate replicate  proportion
wall_by_yr_repl = cbec %>% 
  select(-w) %>%
  left_join(long_weights, by = "id") %>%
  group_by(year, rep, wall) %>%
  summarize(nwalls = sum(rw)) %>% 
  mutate(p_repl = nwalls/sum(nwalls))

### Calculate variance of replicate proportions around the point estimate
wall_by_yr_var = wall_by_yr_repl %>%
  select(-nwalls) %>%
  left_join(wall_by_yr, by = c("year", "wall")) %>%
  select(-nbuildings) %>% 
  group_by(year, wall) %>%
  summarize(p_wall = mean(p_wall), v =  sum({(p_repl - p_wall)^2}))

m = qnorm(.975)
wall_by_yr_var = wall_by_yr_var %>% 
  mutate(
  se = sqrt(v), 
  lwr = p_wall - m * se,
  upr = p_wall + m * se) 

# Plots and Tables -----------------------------------------------------------

## Plot for wall construction material by year/square footage

## construct a plot
wall_year_size %>%
  mutate(`Size Group` = size_group) %>%
  ggplot( aes(x = year, y = p, color = `Size Group`) ) +
  geom_point( position = position_dodge(.5) ) +
  geom_point( position = position_dodge(.5) ) +
  geom_errorbar( 
    aes(ymin = lwr, ymax = upr),
    position = position_dodge(.5), width = 0.2
  ) + 
  facet_wrap(~ wall) + 
  coord_flip() +
  theme_bw() +
  ylab('% Using Wall Material') + 
  xlab('Construction Year') +
  ylim(c(0, 90))  


## Plot for roof construction material by year/square footage
roof_year_size %>%
  mutate(`Size Group` = size_group) %>%
  ggplot( aes(x = year, y = p, color = `Size Group`)) +
  geom_point( position = position_dodge(.5) ) +
  geom_point( position = position_dodge(.5) ) +
  geom_errorbar( 
    aes(ymin = lwr, ymax = upr),
    position = position_dodge(.5), width = 0.2
  ) + 
  facet_wrap(~ roof) + 
  coord_flip() +
  theme_bw() +
  ylab('% Using Roof Material') + 
  xlab('Construction Year') +
  ylim(c(0, 90))  
