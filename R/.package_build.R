# Set the name of your (!) new package
package <- "kwb.misa"

# Set the path to your (!) local folder to which GitHub repositories are cloned
repo_dir <- "~/R/git"

# Set the path to the package directory
pkg_dir <- file.path(repo_dir, package)

# Create directory for R package
kwb.pkgbuild::create_pkg_dir(pkg_dir)
#> /var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T//RtmpE9Q727/kwb.newpackage is a valid 'root_dir' for pkg 'kwb.newpackage'
#> [1] "/var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T//RtmpE9Q727/kwb.newpackage"
# Create a default package structure
withr::with_dir(pkg_dir, {kwb.pkgbuild::use_pkg_skeleton(package)})
#> ✔ Setting active project to '/private/var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T/RtmpE9Q727/kwb.newpackage'
#> ✔ Writing 'kwb.newpackage.Rproj'
#> ✔ Adding '.Rproj.user' to '.gitignore'
#> NULL

author <- list(name = "Malte Zamzow")

# author <- list(
#   name = "Michael Rustler",
#   orcid = "0000-0003-0647-7726",
#   url = "http://mrustl.de"
# )

description <- list(
  name = package,
  title = "Functions to be used in project MiSa",
  desc  = "Assessment of oxygen course in rivers. Assessment is aimed at reducing critical situations"
)

setwd(pkg_dir)

kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.0.0.9000",
  stage = "experimental"
)

usethis::use_vignette("documention")

### R functions
if(FALSE) {
  ## add your dependencies (-> updates: DESCRIPTION)
  pkg_dependencies <- c("dplyr")

  sapply(pkg_dependencies, usethis::use_package)

  desc::desc_add_remotes("kwb-r/kwb.utils",normalize = TRUE)

  usethis::use_pipe()
}

kwb.pkgbuild::use_ghactions()

# in case package dependencies need to be added
pkg_dependencies <- c() # insert package names
sapply(pkg_dependencies, usethis::use_package)

