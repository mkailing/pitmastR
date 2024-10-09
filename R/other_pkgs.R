## this script plays with PITcleanr a pre-existing R package for working with PIT tag data
## & writes/test new functions

#clear workspace
rm(list=ls())

## Load packages for PITcleanr ####
#devtools::install_github("hadley/devtools")

library(devtools)

#remotes::install_github("KevinSee/PITcleanr",
#                        build_vignettes = TRUE)

#remotes::install_github("KevinSee/PITcleanr@develop")

library(PITcleanr)

## Work through PITcleanr with dummy files ####
# read in dummy ptagis file
ptagis_file = read.csv("test_PITcleanr/ptagis_file.csv")

ptagis_cth = readCTH(ptagis_file, file_type = "PTAGIS")
ptagis_comp = compress(ptagis_cth, units = 'mins')

#this isn't running
qc_detections = qcTagHistory(ptagis_file)

# exploring with non-ptagis files

#non_ptagis = read.

## Work through PITcleanr with MW files ####
#try MW_PIT raw log file
pit = readCTH("/Users/mkailing/Dropbox/Kailing_Projects/test_PITcleanr/non_ptagis/NE2_2023_10_02.log", file_type='raw')
#this function is used for single .log file and default assigns an event type specific for fish (ie Instream)
#also not sure if 'nodes' are required here

pit_com = compress(pit, units = 'hours')
#compress calculates the duration between detections based on 24HR time - would need to be able to adjust based on bat hours (create flexibility in foraging hours)

#getwd()

## Explore 'survival' package ####

library(survival)
View(veteran)

test <- as.data.frame(Surv(veteran$time, veteran$status))
test2 <- as.data.frame(Surv(veteran$time, veteran$diagtime, veteran$status, type = 'interval'))

## play around with package/function building

(x <- "alfa,bravo,charlie,delta")
#> [1] "alfa,bravo,charlie,delta"
strsplit(x, split = ",")

strsplit(x, split = ",")[[1]]

strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}

