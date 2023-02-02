# AllData_NoVoid <- fread('appData/AllData_NoVoid.csv', stringsAsFactors = FALSE)
AllData_NoVoid <- readRDS('appData/AllData_NoVoid.rds')
AllData_NoVoid$Sampling_Date <- as.Date(AllData_NoVoid$Sampling_Date, "%Y-%m-%d")

POCs <- readRDS('appData/Oregon_POCs.rds')
# Create summary table with statistics by analyte and year

# AllData_Sum <- AllData_NoVoid %>% 
#   dplyr:::group_by(Analyte, Year, add=TRUE) %>% 
#   dplyr:::summarise(DetectFreq = sum(!is.na(Result.ug.l))/length(Result.ug.l),
#                     N_Samples = length(Analyte),
#                     N_Detects = sum(!is.na(Result.ug.l)),
#                     AQL_Value = ifelse(!is.na(min(min.AQL.value)), min(min.AQL.value), NA),
#                     AQL.Ratio = max(AQL_Ratio, na.rm = TRUE),
#                     Over_AQL = sum(na.omit(AQL_Ratio) > 1.0),
#                     "AQL_50_100" = sum(na.omit(AQL_Ratio) > 0.5 & na.omit(AQL_Ratio) <= 1.0),
#                     "AQL_10_50" = sum(na.omit(AQL_Ratio) > 0.1 & na.omit(AQL_Ratio) <= 0.5),
#                     "AQL_0_10" = sum(na.omit(AQL_Ratio) <= 0.1)
#                     # ,
#                     # HH_Value = min(min.HH.value, na.rm = TRUE),
#                     # HH.Ratio = max(HH_Ratio, na.rm = TRUE),
#                     # Over_HH = sum(na.omit(HH_Ratio) > 1.0),
#                     # "50_100_HH" = sum(na.omit(HH_Ratio) > 0.5 & na.omit(HH_Ratio) <= 1.0),
#                     # "10_50_HH" = sum(na.omit(HH_Ratio) > 0.1 & na.omit(HH_Ratio) <= 0.5),
#                     # "<10_HH" = sum(na.omit(HH_Ratio) <= 0.1)
#   )

# AllData_SumBy_Basin <- AllData_NoVoid %>%
#   group_by(Analyte, Project, Year, add=TRUE) %>%
#   dplyr:::summarise(DetectFreq = sum(!is.na(Result.ug.l))/length(Result.ug.l),
#                     N_Samples = length(Analyte),
#                     N_Detects = sum(!is.na(Result.ug.l)),
#                     AQL_Value = ifelse(!is.na(min(min.AQL.value)), min(min.AQL.value), NA),
#                     AQL.Ratio = ifelse(is.infinite(max(AQL_Ratio, na.rm = TRUE)),NA, max(AQL_Ratio, na.rm = TRUE)),
#                     Over_AQL = sum(na.omit(AQL_Ratio) > 1.0),
#                     "AQL_50_100" = sum(na.omit(AQL_Ratio) > 0.5 & na.omit(AQL_Ratio) <= 1.0),
#                     "AQL_10_50" = sum(na.omit(AQL_Ratio) > 0.1 & na.omit(AQL_Ratio) <= 0.5),
#                     "AQL_0_10" = sum(na.omit(AQL_Ratio) <= 0.1)
#                     # ,
#                     # HH_Value = min(min.HH.value, na.rm = TRUE),
#                     # HH.Ratio = max(HH_Ratio, na.rm = TRUE),
#                     # Over_HH = sum(na.omit(HH_Ratio) > 1.0),
#                     # "50_100_HH" = sum(na.omit(HH_Ratio) > 0.5 & na.omit(HH_Ratio) <= 1.0),
#                     # "10_50_HH" = sum(na.omit(HH_Ratio) > 0.1 & na.omit(HH_Ratio) <= 0.5),
#                     # "<10_HH" = sum(na.omit(HH_Ratio) <= 0.1)
#   )
# 
# AllData_SumBy_Station <- AllData_NoVoid %>%
#   group_by(Analyte, Station_ID, Year, add=TRUE) %>%
#   dplyr:::summarise(StationDescription = first(Station_Description),
#                     DetectFreq = sum(!is.na(Result.ug.l))/length(Result.ug.l),
#                     N_Samples = length(Analyte),
#                     N_Detects = sum(!is.na(Result.ug.l)),
#                     AQL_Value = ifelse(!is.na(min(min.AQL.value)), min(min.AQL.value), NA),
#                     AQL.Ratio = max(AQL_Ratio, na.rm = TRUE),
#                     Over_AQL = sum(na.omit(AQL_Ratio) > 1.0),
#                     "AQL_50_100" = sum(na.omit(AQL_Ratio) > 0.5 & na.omit(AQL_Ratio) <= 1.0),
#                     "AQL_10_50" = sum(na.omit(AQL_Ratio) > 0.1 & na.omit(AQL_Ratio) <= 0.5),
#                     "AQL_0_10" = sum(na.omit(AQL_Ratio) <= 0.1),
#                     # HH_Value = min(min.HH.value, na.rm = TRUE),
#                     # HH.Ratio = max(HH_Ratio, na.rm = TRUE),
#                     # Over_HH = sum(na.omit(HH_Ratio) > 1.0),
#                     # "50_100_HH" = sum(na.omit(HH_Ratio) > 0.5 & na.omit(HH_Ratio) <= 1.0),
#                     # "10_50_HH" = sum(na.omit(HH_Ratio) > 0.1 & na.omit(HH_Ratio) <= 0.5),
#                     # "<10_HH" = sum(na.omit(HH_Ratio) <= 0.1),
#                     Avg_Result = mean(Result.ug.l, na.rm = TRUE),
#                     Max_Result = max(Result.ug.l, na.rm = TRUE)
#   )

StationReference <- subset(AllData_NoVoid, select = c('Project', 'Station_ID', 'Station_Description')) %>%
  group_by(Project, Station_ID, add=TRUE) %>%
  dplyr:::summarise(StationDescription = first(Station_Description))

