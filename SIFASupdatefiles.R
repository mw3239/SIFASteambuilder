invisible(Sys.setlocale("LC_CTYPE", locale="Japanese"))

invisible(library(readr))
invisible(library(rvest))
invisible(library(magrittr))
invisible(library(dplyr))
invisible(library(stringr))
invisible(library(purrr))
invisible(library(tidyr))
invisible(library(RSQLite))


source("SIFASgetdb.R", encoding = "UTF-8")
source("SIFASfixdberrors.R", encoding = "UTF-8")
source("SIFASchardata.R", encoding = "UTF-8")
source("SIFASbondbonus.R", encoding = "UTF-8")
source("SIFASbondboard.R", encoding = "UTF-8")
source("SIFAScardscape.R", encoding = "UTF-8")
source("SIFAScardskills.R", encoding = "UTF-8")
source("SIFASaccessoryscape.R", encoding = "UTF-8")
source("SIFASaccessoryskillscrape.R", encoding = "UTF-8")
source("SIFASinspiscrape.R", encoding = "UTF-8")

rm(list = ls())
