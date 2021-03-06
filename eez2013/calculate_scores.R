# presume that working directory in current scenario directory, eg:
# setwd('~/github/ohi-global/eez2013')
# devtools::load_all('~/github/ohicore')
# library(ohicore)

# load conf
conf = Conf('eez2013/conf')

# run checks on layers
CheckLayers('eez2013/layers.csv', 'eez2013/layers', flds_id=conf$config$layers_id_fields)

# load layers
layers = Layers('eez2013/layers.csv', 'eez2013/layers')

# calculate scores
scores = CalculateAll(conf, layers, debug=F)
write.csv(scores, 'eez2013/scores.csv', na='', row.names=F)


# 
# # requrired packages
# library(httr)
# library(knitr)
# library(markdown)
# library(plyr)
# library(rCharts)
# library(RColorBrewer)
# library(reshape2)
# library(RJSONIO)
# library(psych)
# library(shiny)
# library(stringr)
# library(dplyr)
# 
# 
# #library(devtools)
# #if ('ohicore' %in% loaded_packages()) detach('package:ohicore')
# #library(ohicore)
# devtools::load_all('~/github/ohicore')
# setwd('~/github/ohi-global/eez2013')
# scenario='eez2013'
# conf   = Conf('conf')
# layers = Layers('layers.csv', 'layers')  
# d = conf$functions$MAR(layers, status_years=2005:2011)
# # A WRONG -- MAR status for Thailand[25]:  0.02
# # B RIGHT -- MAR status for Thailand[25]: 88.86
# 
# #paste(sub('^package:','',search()), collapse=', ')
# #cat(paste(sub('^package:','', rev(search())), collapse=')\nlibrary('))
# # load_all()
# #   DEBUG: MAR status for Thailand[25]: 0.02 WRONG!
# #   .GlobalEnv, dplyr, ohicore, RColorBrewer, stringr, psych, RJSONIO, reshape2, markdown, knitr, shiny, plyr, tools:rstudio, stats, graphics, grDevices, utils, datasets, methods, Autoloads, base
# # default after Restarting R Session...
# #   .GlobalEnv, tools:rstudio, stats, graphics, grDevices, utils, datasets, methods, Autoloads, base
# # preload with dplyr last
# #   DEBUG: MAR status for Thailand[25]: 88.86 CORRECT!
# #   .GlobalEnv, ohicore, dplyr, stringr, shiny, psych, RJSONIO, reshape2, RColorBrewer, rCharts, plyr, markdown, knitr, httr, tools:rstudio, stats, graphics, grDevices, utils, datasets, methods, Autoloads, base
# 
# # # set scenario variables
# # conf   = Conf('conf')
# # layers = Layers('layers.csv', 'layers')
# # scores = read.csv('scores.csv')
# # 
# # NP
# # # debug=T
# # # harvest_peak_buffer = 0.35
# # # year_max = c(eez2014=2011, eez2013=2010, eez2012=2009)[[scenario]]
# # 
# # # TR
# # year_max = c(eez2014=2012, eez2013=2011, eez2012=2010)[[scenario]]
# # 
# # 
# # scores = CalculateAll(conf, layers, debug=T)
# #   
# # 
# # # load conf
# # conf = Conf('conf')
# # 
# # # run checks on layers
# # CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)
# # 
# # # load layers
# # layers = Layers('layers.csv', 'layers')
# # 
# # # calculate scores
# # scenario='eez2013'
# # try({ 
# #   scores = CalculateAll(conf, layers, debug=T) })
# # write.csv(scores, 'scores.csv', na='', row.names=F)
