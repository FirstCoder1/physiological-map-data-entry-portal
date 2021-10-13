library(reticulate)
<<<<<<< HEAD
<<<<<<< HEAD
library(evaluate)
=======
>>>>>>> 3425471 (Fixed .xml conversion and cleaned repo)
library(here)

# Open python in R and load sbtab converter
<<<<<<< HEAD
conda_create("ontox-app", python_version = "3.8")
conda_install("ontox-app", "sbtab", pip = FALSE)
=======
library(here)

# Open python in R and load sbtab converter
conda_create("ontox-app", python_version = "3.8")
conda_install("ontox-app", "sbtab", pip = TRUE)
>>>>>>> 2304e4f (progress for xml conversion)
use_condaenv("ontox-app", required = TRUE)
=======
virtualenv_create("ontox-app")
virtualenv_install(envname = "ontox-app", "sbtab")
use_virtualenv("ontox-app", required = TRUE)
>>>>>>> 3425471 (Fixed .xml conversion and cleaned repo)

# Load in file to convert
file_test <- "physmap.tsv"
