repo_root = here::here()
setwd(repo_root)

devtools::load_all()

# creating app example
ruminate_app= system.file(package="ruminate", "templates", "ruminate.R")

nca_app_contents = readLines(ruminate_app)
nca_app_contents = c("if(interactive()){",
          nca_app_contents,
          "}")

nca_app = file.path(repo_root, "inst", "test_apps", "nca_app.R")

fileConn=file(nca_app)
writeLines(nca_app_contents, fileConn)
close(fileConn)

# building documentation
devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))

# Rebuilding the pkgdown site
pkgdown::build_site()

# Fixing any broken image references
art_dir = file.path("docs", "articles")

# Getting all of the html files in the article dir
htds = dir(art_dir, "*.html")
for(htd in htds){
  fn = file.path(art_dir, htd)

  cfn = file(fn, open="r")
  htd_lines = readLines(cfn)
  close(cfn)

  # For some reason it's doing this weird relative path thing, so I'm stripping that out here:
  trim_txt = "../../../../../../My%20Drive/projects/ruminate/github/ruminate/articles/"
  htd_lines = gsub(trim_txt, "", htd_lines)

  write(htd_lines, file=fn, append=FALSE)

}
