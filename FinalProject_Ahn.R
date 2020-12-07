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
## Updated: December 09, 2020 

# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)

# Script: ---------------------------------------------------------------------

# Load data: ------------------------------------------------------------------
### CBEC Data
cbec = read_delim('./Data/2012_public_use_data_aug2016.csv',delim = ",") %>%
  select(id = PUBID, w = FINALWT, wall = WLCNS, 
         roof = RFCNS, size = SQFTC)

### Replicate weights
weights =  read_delim('./Data/2012_public_use_data_aug2016.csv',  
                      delim = ",") %>% 
  select(id = PUBID, starts_with("FINALWT")) %>% 
  select(-FINALWT)

## Codebook
codebook = readxl::read_excel('./Data/2012microdata_codebook.xlsx') %>% 
  select(c(2,6,7)) 

variables = c(wall = 'WLCNS', roof = 'RFCNS', size = "SQFTC")

codes = codebook %>% filter( `Variable\r\nname` %in% variables) %>%
  transmute(
    variable = `Variable\r\nname`,
    levels =
      str_split(`Values/Format codes`, pattern = '\r\n'))

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
  mutate(wall = decode_recs(wall,'WLCNS',codes),
         roof = decode_recs(roof,'RFCNS',codes),
         size = decode_recs(size,'SQFTC',codes),
         id = as.double(id))

head(cbec)
