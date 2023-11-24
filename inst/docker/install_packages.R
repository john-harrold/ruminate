# General
install.packages('remotes',   repos='http://cloud.r-project.org')
install.packages('PKNCA',     repos='http://cloud.r-project.org')
install.packages('shiny',     repos='http://cloud.r-project.org')
install.packages("DescTools", repos='http://cloud.r-project.org')
install.packages("symengine", repos='http://cloud.r-project.org')

# nlmixr2verse files:
install.packages(c('dparser',     'nlmixr2data',  'lotri',       'rxode2ll',
                   'rxode2parse', 'rxode2random', 'rxode2et',    'rxode2',
                   'nlmixr2est',  'nlmixr2extra', 'nlmixr2plot', 'nlmixr2',
                   'nonmem2rx',   'nlmixr2lib'),
                 repos = c('https://nlmixr2.r-universe.dev',
                           'https://cloud.r-project.org'))

# Dev versions of ubiquity tools:
remotes::install_github('john-harrold/onbrand',  dependencies=TRUE)
remotes::install_github('john-harrold/formods',  dependencies=TRUE)
remotes::install_github('john-harrold/ruminate', dependencies=TRUE)
