# Forcing upgrades of specific packages from CRAN 
install.packages('remotes',      repos='http://cloud.r-project.org')
install.packages('shiny',        repos='http://cloud.r-project.org')
install.packages("DescTools",    repos='http://cloud.r-project.org')
install.packages("symengine",    repos='http://cloud.r-project.org')
install.packages("shinyWidgets", repos='http://cloud.r-project.org')

#install.packages('ggplot2',    repos='http://cloud.r-project.org')

# Removed with 3.0
# rxode2et
# rxode2parse
# rxode2random
#
# Added with 3.0
# monolix2rx

# nlmixr2verse files:
# install.packages(c("dparser",     "nlmixr2data",  "lotri",        "rxode2ll",
#                    "rxode2",      "nlmixr2est",   "nlmixr2extra", "nlmixr2plot",
#                    "nlmixr2",     "nlmixr2lib",   "monolix2rx",   "nonmem2rx"),
#                  repos = c('https://nlmixr2.r-universe.dev',
#                            'https://cloud.r-project.org'))
install.packages(c('dparser',     'nlmixr2data',  'lotri',        'rxode2ll',
                   'rxode2',      'nlmixr2est',   'nlmixr2extra', 'nlmixr2plot',
                   'nlmixr2',     'nlmixr2lib',   'babelmixr2',   'nonmem2rx',
                   'monolix2rx',  "ggPMX",         "nlmixr2rpt",  "posologyr", 
                   "shinyMixR",   'xpose.nlmixr2'),
                 repos = c('https://nlmixr2.r-universe.dev',
                           'https://cloud.r-project.org'))

# Dev versions of different packages: 
remotes::install_github('humanpred/pknca',       dependencies=TRUE)
remotes::install_github('john-harrold/onbrand',  dependencies=TRUE)
remotes::install_github('john-harrold/formods',  dependencies=TRUE)
remotes::install_github('john-harrold/ruminate', dependencies=TRUE)
