####################################################################################################################
# IGRA PIKADA 301
####################################################################################################################

# naložimo knjižnice
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


# pokličemo osnovne funkcije
source("program/osnovne_funkcije.r")

####################################################################################################################

# OPTIMALNA STRATEGIJA

# optimalna strategija je razdeljena na dva dela 
# 1) maksimiziranje števila zadetih točk
# 2) minimiziranje števila rund do konca
# 
# POJASNILO:
# Na začetku želimo maksimizirati število zadetih točk in čim bolj znižati število 301,
# ko se bližamo koncu igre, pa maksimiziranje točk ni več optimalno, saj moramo končati točno na 0.
# (če bi še naprej maksimizirali točke, bi zašli na negativno stran točk, 
# to pa v igri pomeni povratek točk kot na začetku "runde")
# Lahko imamo še dodatno omejitev, da je potrebno končati z zadetkom v inner bull ali v  pas z dvojnimi točkami.
# Zaradi navedenih razlogov je v drugem delu igre optimalna strategija minimiziranjem števila rund do konca igre.
# Strategijo 2 bomo implementirali za uporabo, ko enkrat dosežemo 60 točk. Pred tem pa lahko
# uporabljamo strategijo za maksimiziranje točk. 

####################################################################################################################
# Optimalna strategija 1: maksimiziranje števila točk pri metu puscice

source("program/strategija1.r")

####################################################################################################################
# Optimalna strategija 2: minimiziranje števila rund do konca igre, ko enkrat dosežemo 60 ali manj točk

source("program/strategija2_60.r")

####################################################################################################################
# Shiny aplikacija

runApp(appDir = "shiny/", launch.browser=TRUE)

####################################################################################################################

