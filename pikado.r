####################################################################################################################
# IGRA PIKADA 301
####################################################################################################################

# nalozimo knjiznice
# source("lib/libraries.r")
library(darts)
library(MASS)
library(ggplot2)
library(pheatmap)
library(wesanderson)
library(plyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(knitr)
library(Matrix)
library(slam)
library(abind)


# poklicemo osnovne funckije
# source("program/osnovne_funkcije.r")

####################################################################################################################

# OPTIMALNA STRATEGIJA

# optimalna strategija je razdeljena na dva dela 
# 1) maksimiziranje števila zadetih tock
# 2) minimiziranje števila rund do konca
# 
# POJASNILO:
# Na zacetku želimo maksimizirati stevilo zadetih tock in čim bolj nižati število 301,
# ko se bližamo koncu igre, pa maksimiziranje točk ni vec optimalno, saj moramo koncati tocno na 0.
# (ce bi se naprej maksimizirali tocke, bi zasli na negativno stran, 
# to pa v igri pomeni povratek tock kot na zacetku "runde")
# Lahko imamo se dodatno omejitev, da je potrebno koncati z zadetkom v inner bull ali v  pas z dvojnimi tockami.
# Zaradi navedenih razlogov je v drugem delu igre optimalna strategija minimiziranjem števila rund do konca igre.
# Strategijo 2 bomo implementirali za uporabo, ko enkrat dosežemo 60 točk. Pred tem pa lahko
# uporabljamo strategijo za maksimiziranje točk. 

####################################################################################################################
# Optimalna strategija 1: maksimiziranje stevila tock pri metu puscice

source("program/strategija1.r")

####################################################################################################################
# Optimalna strategija 2: minimiziranje stevila rund do konca igre, ko enkrat dosežemo 60 ali manj točk

source("program/strategija2_60.r")

####################################################################################################################
# Shiny aplikacija

runApp(appDir = "shiny/", launch.browser=TRUE)

####################################################################################################################

