# Import all data for report 1
# Run from main project directory
library(tidyverse, quietly = TRUE)
library(igraph, quietly = TRUE)
library(wikkitidy, quietly = TRUE)
source(file.path("scripts", "who-counts-lib.R"))

DATA_DIR <- file.path("data", "who-counts")

# Use cached versions of files?
parser <- optparse::OptionParser()
parser <- optparse::add_option(parser, c("-r", "--redownload"), action="store_true",
                     default=FALSE, help="Freshly download the data; by default, cached data is used.")
args <- optparse::parse_args(parser)
USE_CACHE <- !args$redownload




