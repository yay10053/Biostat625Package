# Biostat625Package
This repository contains the LM package for biostat625 hw3

# LM Package description 
To start off your statistical journey, you will encounter some commonly used regression models, this package is here to guide you through this. There is 3 main functions and 1 helper function in this package:

* SLM() :Simple Linear Regression with RCPP
* MultiLinearRegression() :Multiple Linear Regression
* OnePassCov() :Covariance
* RidgeRegression() :Ridge Regression

# Package Installation Instruction

In your Rstudio, make sure you have installed devtools package.
In the Console, type in the following:

devtools::install_github("yay10053/Biostat625Package", build_vignettes = TRUE)

# Instruction Content
You will be able to find a Help Pages containing the function document and a User guide containing examples, and function accurancy and efficiency comparsion. 
In the Console, type in the following:

help(package = "LM")

