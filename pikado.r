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
library(knitr)
library(Matrix)


# poklicemo osnovne funckije
source("program/osnovne_funkcije.r")

####################################################################################################################

# OPTIMALNA STRATEGIJA

# optimalna strategija je razdeljena na dva dela 
# 1) maksimiziranje števila zadetih tock
# 2) minimiziranje števila rund do konca
# 
# POJASNILO:
# na zacetku želimo maksimizirati stevilo zadetih tock in čim bolj nizati število 501
# ko se blizamo koncu igre, pa maksimiziranje tock ni vec optimalno, saj moramo koncati tocno na 0
# ce bi se naprej maksimizirali tocke, bi zasli na negativno stran, 
# to pa v igri pomeni povratek tock kot na zacetku "runde"
# imamo se dodatno omejitev, da je potrebno koncati z zadetkom v inner bull ali v  pas z dvojnimi tockami
# zato bomo v drugem delu igre optimalno strategijo implementirali z minimiziranjem števila rund do konca igre

####################################################################################################################
# Optimalna strategija 1: maksimiziranje stevila tock pri metu puscice

source("program/strategija1.r")

####################################################################################################################
# Optimalna strategija 2: maksimiziranje stevila tock pri metu puscice

#source("program/strategija2.r")

####################################################################################################################
# Shiny aplikacija

runApp(appDir = "shiny/", launch.browser=TRUE)

####################################################################################################################

# TO DO: implementacija 2 strategije 
# TO DO: strategija 1 - izračun vrednosti namesto MC metode?
# TO OD: aplikacija za obe strategiji:
#       - strategija 1: izračun heatmap za 3 nivoje ali za poljubno vrednost, izpis ciljne točke, lepši heatmapi
#       - strategija 2: poljubni vpis stanja in izpis ciljne točke, simualacija igre za vsak nivo

