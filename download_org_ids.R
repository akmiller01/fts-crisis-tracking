list.of.packages <- c("data.table","httr","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.us.r-project.org")
lapply(list.of.packages, require, character.only=T)

setwd("~/git/fts-crisis-tracking")

url = "https://api.hpc.tools/v1/public/organization"

source_json = fromJSON(url,)

org_data = source_json$data
org_data$type = sapply(org_data$categories,`[[`,2)
org_data$categories = NULL
fwrite(org_data,"org_ids.csv")