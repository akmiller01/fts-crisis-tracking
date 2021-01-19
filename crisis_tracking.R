list.of.packages <- c("data.table","anytime","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/fts-crisis-tracking")

fts = fread("fts.csv")
org_ids = fread("org_ids.csv")
org_ids$abbreviation = NULL
setnames(org_ids,"id","source_Organization_id")
setnames(org_ids,"name","source_Organization_name")

overlap_org_df = data.frame(source_Organization_name=c(
  "African Development Bank",
  "Asian Development Bank",
  "World Bank",
  "NaCSA/World Bank",
  "Caribbean Development Bank",
  "Inter-American Development Bank",
  "International Bank for Reconstruction & Development (World Bank)"
))
overlap_org_df = merge(overlap_org_df, org_ids, by="source_Organization_name", all.x=T)
overlap_orgs = as.character(overlap_org_df$source_Organization_id)

wb_groupings = fread("wb_groupings.csv")
setnames(wb_groupings,"iso3c","Country Code")
setnames(wb_groupings,"region","Region")
setnames(wb_groupings,"income","Income Type")

loop_param_df = fread("loop_parameters.csv")
loop_param_df$appeal_name %in% unique(fts$destination_Plan_name)
loop_param_df$emergency_name %in% unique(fts$destination_Emergency_name)
loop_param_df$start_date = anydate(loop_param_df$start_date)
loop_param_df$end_date = anydate(loop_param_df$end_date)

for(i in 1:nrow(loop_param_df)){
  country = loop_param_df$country[[i]]
  emergency_name = loop_param_df$emergency_name[[i]]
  appeal_name = loop_param_df$appeal_name[[i]]
  search_patterns = loop_param_df$search_patterns[[i]]
  start_date = loop_param_df$start_date[[i]]
  end_date = loop_param_df$end_date[[i]]
  
  ctry_dat = subset(fts, Destination.Country==country & status %in% c("paid", "commitment"))
  ctry_dat$analysis_date = NA
  ctry_dat$analysis_date[which(ctry_dat$status=="commitment")] = ctry_dat$date[which(ctry_dat$status=="commitment")]
  ctry_dat$analysis_date[which(ctry_dat$status=="paid")] = ctry_dat$decisionDate[which(ctry_dat$status=="paid")]
  ctry_dat$analysis_date[which(ctry_dat$status=="paid" & is.na(ctry_dat$decisionDate))] = ctry_dat$date[which(ctry_dat$status=="paid" & is.na(ctry_dat$decisionDate))]
  ctry_dat$analysis_date = anydate(ctry_dat$analysis_date)
  
  ctry_dat = subset(ctry_dat, analysis_date>=start_date & analysis_date<=end_date)
  
  ctry_dat$emergency_boundary = NA
  ctry_dat$emergency_boundary[which(
    ctry_dat$source_Emergency_name=="" &
      grepl(emergency_name, ctry_dat$destination_Emergency_name, fixed=T)
  )] = "Incoming"
  ctry_dat$emergency_boundary[which(
    grepl(emergency_name, ctry_dat$source_Emergency_name, fixed=T) &
      grepl(emergency_name, ctry_dat$destination_Emergency_name, fixed=T)
  )] = "Internal"
  ctry_dat$emergency_boundary[which(
    grepl(emergency_name, ctry_dat$source_Emergency_name, fixed=T) &
      !grepl(emergency_name, ctry_dat$destination_Emergency_name, fixed=T)
  )] = "Outgoing"
  
  ctry_dat$row.name = c(1:nrow(ctry_dat))
  
  ctry_dat_analysis = subset(
    ctry_dat,
    destination_Emergency_name==emergency_name |
      (destination_Emergency_name=="" & destination_Plan_name==appeal_name)
  )
  ctry_dat_analysis$description_search_flag = FALSE
  remainder_rows = setdiff(c(1:nrow(ctry_dat)),ctry_dat_analysis$row.name)
  ctry_dat_remainder = ctry_dat[remainder_rows,]
  
  ctry_dat_remainder = subset(ctry_dat_remainder, grepl(search_patterns,ctry_dat_remainder$description,ignore.case=T))
  ctry_dat_remainder$description_search_flag = TRUE
  ctry_dat_analysis = rbind(ctry_dat_analysis, ctry_dat_remainder)
  ctry_dat_analysis$row.name = NULL
  
  ctry_dat_analysis$potential_overlap_flag = ctry_dat_analysis$source_Organization_id %in% overlap_orgs
  fwrite(ctry_dat_analysis, paste0("output/",country,"_unformatted.csv"))
  ctry_dat_analysis$originalAmountmn = ctry_dat_analysis$originalAmount/1000000
  ctry_dat_analysis$amountUSDmn = ctry_dat_analysis$amountUSD/1000000
  keep = c(
    "Country Code" = "destination_iso3",
    "Country" = "Destination.Country",
    "Approval Date" = "analysis_date",
    "Last Updated" = "updatedAt",
    "Project Title" = "description",
    "Project ID" = "refCode",
    "Total Committed Amount (original currency mn)" = "originalAmountmn",
    "x : USD" = "exchangeRate",
    "Total Amount Committed (USD mn)" = "amountUSDmn",
    "Grant Amount Committed (USD mn)" = "amountUSDmn",
    "Total Disbursed" = "amountUSDmn"
  )
  ctry_dat_analysis_formatted = ctry_dat_analysis[,keep,with=F]
  names(ctry_dat_analysis_formatted) = names(keep)
  ctry_dat_analysis_formatted = merge(ctry_dat_analysis_formatted,wb_groupings,by="Country Code",all.x=T)
  ctry_dat_analysis_formatted[,c("Source","Classification","Loan Amount Committed (USD mn)","Comment","Press release/project website","Additional source")] = NA
  ctry_dat_analysis_formatted$`Grant Portion` = 1
  ctry_dat_analysis_formatted = ctry_dat_analysis_formatted[order(ctry_dat_analysis_formatted$`Approval Date`),]
  ctry_dat_analysis_formatted$row.name = c(1:nrow(ctry_dat_analysis_formatted))
  ctry_dat_analysis_formatted_l = ctry_dat_analysis_formatted[,c("row.name","Approval Date", "Total Disbursed")]
  ctry_dat_analysis_formatted_l$variable = strftime(ctry_dat_analysis_formatted_l$`Approval Date`,format="%b-%y")
  ctry_dat_analysis_formatted_l$variable = factor(ctry_dat_analysis_formatted_l$variable, levels=unique(ctry_dat_analysis_formatted_l$variable))
  ctry_dat_analysis_formatted_l$`Approval Date` = NULL
  setnames(ctry_dat_analysis_formatted_l,"Total Disbursed","value")
  ctry_dat_analysis_formatted_w = dcast(ctry_dat_analysis_formatted_l,row.name~variable,sum)
  ctry_dat_analysis_formatted_w$row.name = NULL
  date_names = names(ctry_dat_analysis_formatted_w)
  ctry_dat_analysis_formatted$row.name = NULL
  ctry_dat_analysis_formatted = cbind(ctry_dat_analysis_formatted, ctry_dat_analysis_formatted_w)
  name_order = c(
    "Country Code",
    "Country",
    "Income Type",
    "Region",
    "Source",
    "Classification",
    "Approval Date",
    "Last Updated",
    "Project Title",
    "Project ID",
    "Total Committed Amount (original currency mn)",
    "x : USD",
    "Total Amount Committed (USD mn)",
    "Loan Amount Committed (USD mn)",
    "Grant Amount Committed (USD mn)",
    "Grant Portion",
    date_names,
    "Total Disbursed",
    "Comment",
    "Press release/project website",
    "Additional source"
  )
  ctry_dat_analysis_formatted = ctry_dat_analysis_formatted[,name_order,with=F]
  fwrite(ctry_dat_analysis_formatted, paste0("output/",country,"_formatted.csv"))
  rm(ctry_dat, ctry_dat_analysis, ctry_dat_remainder)
}