# presume that working directory in current scenario directory, eg:
#setwd('~/github/ohi-canada/eezCHONE')
devtools::load_all('~/GitHub/ohicore')
library(ohicore) # devtools::install('~/github/ohicore')

# load conf
conf = Conf('eezCHONE/conf')

# run checks on layers
CheckLayers('eezCHONE/layers.csv', 'eezCHONE/layers', flds_id=conf$config$layers_id_fields)

# load layers
layers = Layers('eezCHONE/layers.csv', 'eezCHONE/layers')

# calculate scores
scores = CalculateAll(conf, layers, debug=F)
write.csv(scores, 'eezCHONE/scores.csv', na='', row.names=F)
