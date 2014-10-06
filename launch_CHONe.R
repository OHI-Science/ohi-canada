rm(list=ls())
wd <- getwd()
devtools::load_all('~/GitHub/ohicore')
require(methods)
require(ohicore)

# populate CHONe folder and modify layers and functions
# delete
unlink('eezCHONE/conf/*');unlink('eezCHONE/layers/*');unlink('eezCHONE/spatial/*')
unlink('eezCHONE/layers.csv');unlink('eezCHONE/scores.csv');


# create new layers.csv, and layers folder for Canada-CHONe2014 ####################
file.copy('eez2013/layers.csv', 'eezCHONE/layers.csv', overwrite = T)
file.copy('eez2013/scores.csv', 'eezCHONE/scores.csv', overwrite = T)
fl=list.files('eez2013/layers')
file.copy(paste('eez2013/layers/',fl,sep = ""),paste('eezCHONE/layers/',fl,sep = ""), overwrite = T)
fl=list.files('eez2013/conf')
file.copy(paste('eez2013/conf/',fl,sep = ""),paste('eezCHONE/conf/',fl,sep = ""), overwrite = T)
fl=list.files('eez2013/spatial')
file.copy(paste('eez2013/spatial/',fl,sep = ""),paste('eezCHONE/spatial/',fl,sep = ""), overwrite = T)

# calculate normal scores
 source("eez2013/calculate_scores.R")
# launch_app('~/GitHub/ohi-canada/eezCHONE')
setwd(wd)
# modify
source("layers_Canada-CHONe2014.R") # this line "Canadianizes" the index

# calculate Canadian scores
source("eezCHONE/calculate_scores.R")
#launch_app('~/GitHub/ohi-canada/eezCHONE')

scores2013 = read.csv('eez2013/scores.csv') %>%
   filter(region_id == 218, dimension == 'score')
 
scoreschone = read.csv('eezCHONE/scores.csv') %>% 
   filter(region_id == 218, dimension == 'score')
   
scoreschone$diff <- scores2013$score-scoreschone$score

