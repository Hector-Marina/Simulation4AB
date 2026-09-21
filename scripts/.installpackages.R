################################################
# Simulation for Animal Breeding - PhD course  #
################################################

# Automated script to install the R packages needed for the programme of this course.

# Checking the version of R required for the pipeline
if (!grepl("R version 4.5.1", R.version$version.string)) {
  stop("This course requires R version 4.5.1 to be installed.\n")
}

# List of packages required for the pipeline
ListPackages<-c("AlphaSimR", "MASS", #, "dplyr", "ggplot2", 
                )
for (i in c(1:length(ListPackages))){
  package_name<-ListPackages[i]
  if (!requireNamespace(package_name, quietly = TRUE)) {
    # Install the package if is not
    install.packages(package_name, dependencies = TRUE, quiet=TRUE)
    
    if (!requireNamespace(package_name, quietly = TRUE)) {
      cat("Error: Unable to install package: ", package_name, "\n")
    } else {
      cat("Package", package_name, "installed and loaded successfully.\n")
    }
  } else {
    cat("Package: ", package_name, "was already installed.\n")
  }
}

# List of packages developed for the course
remotes::install_github("Hector-Marina/SimNet", dependencies = TRUE, build_vignettes=TRUE)

# Install R tools (not required so far)
#print("Warning: some packages might require Rtools installed in your computer")
#install.packages("https://cran.r-project.org/bin/windows/Rtools/rtools42/files/rtools42-5355-5357.exe", repos = NULL, type = "win.binary")