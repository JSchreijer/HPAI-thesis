#Script from Jitske Schreijer, used for the Master project "The detection of Avian Influenza from movement and behavioural data"
#starting date: 9th February 2023


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            PACKAGES AND SETUP                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("D:/universiteit/Master Biological Science/Master project/Data_for_Jitske/Jitske/Data")
install.packages("moveVis")
library(moveVis)
library(move)
library(reshape2)
library(ggmap)
require(mapproj)
library(ggplot2)
require(scales)
library(moments)
library(Rfast)
library(readxl)
library(mixdist)
library(adehabitatHR)
library(dplyr)
library(plyr)
library(ctmm)
library(sf)
library(tidyverse)
library(ggpubr)