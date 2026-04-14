## code to prepare `NavyAdenoma` dataset goes here
library(MultiMed)
data("NavyAdenoma")
#TODO Ask them to export the data so we don't have to load it into the package manually

usethis::use_data(NavyAdenoma, internal = TRUE, overwrite = TRUE)
