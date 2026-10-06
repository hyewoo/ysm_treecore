# load libraries
library(data.table)
library(dplyr)
library(tidyverse)
library(openxlsx)

# load inventory compiled data
sampledat_pub <- fread("../faib_sample_byvisit.csv")
treedat <- fread("../compilation_nonPSP_db/treelist.csv")

# set path to folder
folderloc <- "../Ground_Sampling_Program/Contract_Deliverables/Tree_Core_Data"

# 2017 report
core_2017 <- read.xlsx(paste0(folderloc, "/2017/2017_Final_Report.xlsx"))

# clean up headers
headers <- names(core_2017)
headers <- gsub("#", "ID", headers)
headers <- gsub(" ", "_", headers)
headers <- gsub("\\.$", "", headers)
headers <- gsub("\\.", "_", headers)
headers <- ifelse(
  grepl("^\\d{4}$", headers),
  paste0("y", headers),
  headers
)
headers <- gsub("\\(", "", headers)
headers <- gsub("\\)", "", headers)
names(core_2017) <- headers
names(core_2017)[4] <- "SampleName"  # duplicate

saveRDS(core_2017, "core_2017.rds")


# 2018 report
core_2018 <- read.xlsx(paste0(folderloc, "/2018/2018_Final_Report.xlsx"))

headers <- names(core_2018)
headers <- ifelse(
  grepl("^\\d{4}$", headers),
  paste0("y", headers),
  headers
)
names(core_2018) <- headers

# fixing incorreect sample id
missingsampleid_2018 <- core_2018 %>% 
  filter(!Sample %in% sampledat_pub$SITE_IDENTIFIER) %>% select(CLSTR_ID, Sample) %>% distinct() %>%
  extract(
    CLSTR_ID,
    into = c("h1", "h2", "h3"),
    regex = "(.*)-(.*)-(.*)",
    remove = FALSE
  ) %>% 
  mutate(h4 = paste0(h1, "_", h2)) %>%
  data.table() %>%
  left_join(
    sampledat_pub %>%
      select(SITE_IDENTIFIER, SAMPLE_SITE_NAME) %>% distinct(), 
    by = c('h4' = 'SAMPLE_SITE_NAME')) %>% 
  filter(!is.na(CLSTR_ID)) %>% 
  data.table() 

setDT(core_2018)[
  missingsampleid_2018,
  Sample := SITE_IDENTIFIER,
  on = "CLSTR_ID"
]

saveRDS(core_2018, "core_2018.rds")


# 2019 report
core_2019 <- read.xlsx(paste0(folderloc, "/2019/3. Final Reports/2019_Final_Report.xlsx"))

headers <- names(core_2019)
headers <- gsub("#", "ID", headers)
headers <- gsub(" ", "_", headers)
headers <- gsub("\\.$", "", headers)
headers <- gsub("\\.", "_", headers)
headers <- ifelse(
  grepl("^\\d{4}$", headers),
  paste0("y", headers),
  headers
)
headers <- gsub("\\(", "", headers)
headers <- gsub("\\)", "", headers)
names(core_2019) <- headers

# fixing incorreect sample id
missingsampleid_2019 <- core_2019 %>%
  filter(!Sample %in% sampledat_pub$SITE_IDENTIFIER) %>% select(Proj_ID, Sample, PlotType) %>% distinct() %>%
  mutate(h4 = paste0(toupper(Proj_ID), "_", sprintf("%04d", Sample))) %>%
  data.table() %>%
  left_join(
    sampledat_pub %>%
      select(SITE_IDENTIFIER, SAMPLE_SITE_NAME) %>% distinct(), 
    by = c('h4' = 'SAMPLE_SITE_NAME')) %>% data.table()

core_2019 <- core_2019 %>%
  mutate(h4 = paste0(toupper(Proj_ID), "_", sprintf("%04d", Sample))) 

setDT(core_2019)[
  missingsampleid_2019,
  Sample := SITE_IDENTIFIER,
  on = "h4"
]

core_2019 <- core_2019 %>% select(-h4)

saveRDS(core_2019, "core_2019.rds")


# 2021 report
sheets2 <- getSheetNames(paste0(folderloc, "/2021/3. Final Reports/2021_Final Report.xlsx")) 

rows_discarded <- data.frame()
data_all_wide <- data.frame()
data_all <- data.frame()

for (i in 1:length(sheets2)){
  
  df1 <- read.xlsx(xlsxFile = paste0(folderloc, "/2021/3. Final Reports/2021_Final Report.xlsx"), 
                   sheet = i, 
                   skipEmptyRows = FALSE, colNames = FALSE)
  
# header
headerrow<-which(df1[,2] %like% "Client")-1

# remove columns with all NA
header1 <- df1 %>% 
  filter(row_number() <= headerrow) %>%
  select(where(~ !all(is.na(.))))

# remove rows with all NA
header1 <- as.matrix(header1[rowSums(is.na(header1)) != ncol(header1), ])

  # req_num
idx <- which(header1 %like% "Requisition", arr.ind = TRUE)
req_num <- header1[idx + nrow(header1)]

  # other info
idx <- which(header1 %like% "Project", arr.ind = TRUE)
batch <- str_extract(header1[idx + nrow(header1)], "(?<=;\\s)\\d+")
site  <- str_extract(header1[idx + nrow(header1)], "(?<=;\\s).*")

datein <- as.Date(as.numeric(header1[which(header1 %like% "Date", arr.ind = TRUE)[1]+nrow(header1)]), 
                  origin = "1899-12-30")
dateout <- as.Date(as.numeric(header1[which(header1 %like% "Date", arr.ind = TRUE)[2]+nrow(header1)]), 
                   origin = "1899-12-30")


# data
datarow <- which(df1[,1] %like% req_num) + 1

data1 <- df1 %>% 
  filter(row_number() >= datarow) 

headers <- df1[datarow-2,]

headers <- gsub("#", "ID", headers)
headers <- gsub(" ", "_", headers)
headers <- gsub("\\.$", "", headers)
headers <- gsub("\\.", "_", headers)
headers <- ifelse(
  grepl("^\\d{4}$", headers),
  paste0("y", headers),
  headers
)
headers <- gsub("\\(", "", headers)
headers <- gsub("\\)", "", headers)

names(data1) <- headers

data1$req_num <- req_num
data1$batch <- batch
data1$site <- site
data1$datein <- datein
data1$dateout <- dateout

name_map <- c(
  AL_ID = "BCELID",
  BCEL_ID = "BCELID",
  Contract_ID = "ContractID",
  Visit = "Visit_ID",
  Tree = "Tree_ID",
  Measure_code = "Measure_Code",
  Missed_Years = "Missed_Yrs_To_Pith"
)

names(data1) <- dplyr::recode(names(data1), !!!name_map)

#unusable data
data1_discarded <- data1[rowSums(is.na(data1[,1:16])) > 5, ]

rows_discarded <- rbind(rows_discarded, data1_discarded[,1:16])

# usable data
data2 <- data1[rowSums(is.na(data1[,1:15])) <= 5, ]

data2 <- data2 %>%
  mutate(Estimated_Distance_to_Pith_mm = ifelse(Estimated_Distance_to_Pith_mm == "N/A", NA, 
                                                Estimated_Distance_to_Pith_mm),
         Lab_Calc_Missed_Yrs = ifelse(Lab_Calc_Missed_Yrs == "N/A", NA, Lab_Calc_Missed_Yrs)) %>%
  mutate(
    across(
      -c(Project_ID, ContractID, Subplot, Species,
         Sample_Tree_Type, Measure_Code,
         Field_Comments, Lab_Comments, req_num, batch, site, datein, dateout),
      as.numeric
    )
  ) %>%
  mutate(End_Date = as.Date(End_Date, origin = "1899-12-30")) 

data_all_wide <- rbind(data_all_wide, data2 %>% select(BCELID, ID, Project_ID, ContractID, 
                                                       Site_ID, Visit_ID, End_Date, Subplot, Tree_ID,
                                                       Species, DBH, Sample_Tree_Type, Measure_Code, Field_Age,
                                                       Missed_Yrs_To_Pith, Field_Comments,
                                                       Lab_Age, Lab_Calc_Missed_Yrs,
                                                       Estimated_Distance_to_Pith_mm, Lab_Comments,
                                                       req_num, batch, site, datein, dateout))

data3 <- data2 %>%
  pivot_longer(cols = starts_with("y"),
               names_to = "Year", 
               values_to = "mm",
               values_drop_na = T) %>% 
  mutate(Year = as.numeric(gsub("y", "", Year))) %>% 
  select(BCELID, ID, Project_ID, ContractID, 
         Site_ID, Visit_ID, End_Date, Subplot, Tree_ID,
         Species, DBH, Sample_Tree_Type, Measure_Code, Field_Age,
         Missed_Yrs_To_Pith, Lab_Age, Lab_Calc_Missed_Yrs,
         Estimated_Distance_to_Pith_mm, 
         req_num, batch, site, datein, dateout, Year, mm) %>%
  data.table()

data3 <- data3 %>%
  arrange(ID, Site_ID, Visit_ID, Tree_ID, Year) %>%
  group_by(ID, Site_ID, Visit_ID, Tree_ID) %>%
  mutate(mean_mm = mean(mm)) %>% 
  data.table()

data_all <- rbind(data_all, data3)

}

data_all_wide <- data_all_wide %>%
  mutate(objectID = paste0("2021_", row_number()))

data_all1 <- data_all %>%
  left_join(data_all_wide %>% select(Site_ID, Visit_ID, Subplot, Tree_ID, Species, objectID), 
            by = c('Site_ID', 'Visit_ID', 'Subplot', 'Tree_ID', 'Species'))


saveRDS(data_all_wide, "core_2021_wide.rds")
saveRDS(data_all1, "core_2021.rds")


# combine old core data
ore_2017 <- core_2017 %>%
  mutate(objectID = paste0("2017_", row_number())) %>%
  mutate(Visit_ID = NA,
         req_num = NA,
         Lab_Age_Extrp = NA) 

core_2017_wide <- core_2017 %>% select(-starts_with("y")) %>% 
  select(objectID, ID, Project_ID = SampleName, ContractID = Contract, 
         Site_ID = Sample, Visit_ID, End_Date = Date, Subplot = Plot, Tree_ID = TreeID,
         Species = Spp, Sample_Tree_Type = Type, Field_Age,
         Field_Age_Extrap = Field_Age_Extrapolated, Field_Comments, Lab_Age, Lab_Age_Extrp, Lab_Comments, req_num) %>%
  data.table()

core_2017_long <- core_2017 %>%
  pivot_longer(cols = starts_with("y"),
               names_to = "Year", 
               values_to = "mm",
               values_drop_na = T) %>% 
  mutate(Year = as.numeric(gsub("y", "", Year))) %>% 
  select(objectID, ID, Project_ID = SampleName, ContractID = Contract, 
         Site_ID = Sample, Visit_ID, End_Date = Date, Subplot = Plot, Tree_ID = TreeID,
         Species = Spp, Sample_Tree_Type = Type, Field_Age,
         Field_Age_Extrap = Field_Age_Extrapolated, Lab_Age, Lab_Age_Extrp, Year, mm, req_num) %>%
  data.table()

core_2018 <- core_2018 %>%
  mutate(objectID = paste0("2018_", row_number())) %>% 
  mutate(Visit_ID = NA,
         req_num = substr(FileName, 1, 5))

core_2018_wide <- core_2018 %>% select(-starts_with("y")) %>%
  select(objectID, ID = Lab_ID, Project_ID = CLSTR_ID, ContractID = Crew, 
         Site_ID = Sample, Visit_ID, End_Date = Date, Subplot, Tree_ID = Tree_No,
         Species, Sample_Tree_Type = Type, Field_Age,
         Field_Age_Extrap, Field_Comments = Field_Comment, Lab_Age, Lab_Age_Extrp, 
         Lab_Comments = Lab_Comment, req_num) %>%
  data.table()

core_2018_long <- core_2018 %>%
  pivot_longer(cols = starts_with("y"),
               names_to = "Year", 
               values_to = "mm",
               values_drop_na = T) %>% 
  mutate(Year = as.numeric(gsub("y", "", Year))) %>% 
  select(objectID, ID = Lab_ID, Project_ID = CLSTR_ID, ContractID = Crew, 
         Site_ID = Sample, Visit_ID, End_Date = Date, Subplot, Tree_ID = Tree_No,
         Species, Sample_Tree_Type = Type, Field_Age,
         Field_Age_Extrap, Lab_Age, Lab_Age_Extrp, Year, mm, req_num) %>%
  data.table()

core_2019 <- core_2019 %>%
  mutate(objectID = paste0("2019_", row_number())) %>% 
  mutate(Visit_ID = NA,
         ID = NA,
         req_num = substr(FileName, 1, 5))

core_2019_wide <- core_2019 %>% select(-starts_with("y")) %>%
  select(objectID, ID, Project_ID = Proj_ID, ContractID = Crew, 
         Site_ID = Sample, Visit_ID, End_Date = Date, Subplot, Tree_ID = Tree_No,
         Species, Sample_Tree_Type = Type, Field_Age,
         Field_Age_Extrap, Field_Comments = Field_Comment, Lab_Age, Lab_Age_Extrp, 
         Lab_Comments = Lab_Comment, req_num) %>%
  data.table()

core_2019_long <- core_2019 %>%
  pivot_longer(cols = starts_with("y"),
               names_to = "Year", 
               values_to = "mm",
               values_drop_na = T) %>% 
  mutate(Year = as.numeric(gsub("y", "", Year))) %>% 
  select(objectID, ID, Project_ID = Proj_ID, ContractID = Crew, 
         Site_ID = Sample, Visit_ID, End_Date = Date, Subplot, Tree_ID = Tree_No,
         Species, Sample_Tree_Type = Type, Field_Age,
         Field_Age_Extrap, Lab_Age, Lab_Age_Extrp, Year, mm, req_num) %>%
  data.table()


core_old_all <- rbind.data.frame(core_2017_wide, core_2018_wide, core_2019_wide)
core_old_long_all <- rbind.data.frame(core_2017_long, core_2018_long, core_2019_long)

core_old_all <- core_old_all %>%
  mutate(Subplot = ifelse(Subplot %in% c("", " ", "IPC", "IPC ", "n/a", NA), "I", Subplot))

core_old_long_all <- core_old_long_all %>%
  mutate(Subplot = ifelse(Subplot %in% c("", " ", "IPC", "IPC ", "n/a", NA), "I", Subplot))


# add DBH, MEasure_Code, Missed_Yrs_To_Pith, Lab_Calc_Missed_Yrs, Estimated_Distance_to_Pith_mm
# tree core condition code (ROT, PTH, CRC)
# tree status (Intermediate, dom. codom, residual)
core_old_all1 <- core_old_all %>%
  filter(!Tree_ID %in% c('364A',  '364B', '381A',  '381B', '6 (3)')) %>%
  mutate(Tree_ID = as.integer(Tree_ID)) %>%
  filter(!is.na(Tree_ID)) %>%
  data.table()

core_old_all2 <- core_old_all1 %>% 
  mutate(file_year = as.integer(substr(objectID, 1, 4))) %>%
  left_join(sampledat_pub %>% 
              select(SITE_IDENTIFIER, VISIT_NUMBER, MEAS_YR, SAMPLE_ESTABLISHMENT_TYPE),
            by = c('Site_ID' = 'SITE_IDENTIFIER')) %>%
  group_by(Site_ID, Subplot, Tree_ID, file_year, objectID) %>%
  slice_min(abs(MEAS_YR - file_year), n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  mutate(Visit_ID = VISIT_NUMBER) %>%
  select(-VISIT_NUMBER) %>%
  data.table()

core_old_all3 <- core_old_all2 %>%
  filter(Site_ID %in% sampledat_pub$SITE_IDENTIFIER) %>%
  left_join(treedat %>% 
              select(SITE_IDENTIFIER, VISIT_NUMBER, PLOT, TREE_NO, DBH, CROWN_CLASS_CODE, RESIDUAL_IND,
                     AGE_MEASURE_CODE, HT_TOTAL, TH_TREE, BORED_HT, BORED_AGE_SOURCE, BORED_AGE_FLAG,
                     SUIT_SI, SUIT_HT, SUIT_AGE, SITE_TREE, AGE_BH) %>% distinct(),
            by = c('Site_ID' = 'SITE_IDENTIFIER', 'Visit_ID' = 'VISIT_NUMBER',
                   'Subplot' = 'PLOT', 'Tree_ID' = 'TREE_NO')) %>% data.table() 


core_2021_wide <- data_all_wide

core_2021_wide1 <- core_2021_wide %>% 
  mutate(file_year = as.integer(substr(objectID, 1, 4))) %>%
  left_join(sampledat_pub %>% 
              select(SITE_IDENTIFIER, VISIT_NUMBER, MEAS_YR, SAMPLE_ESTABLISHMENT_TYPE, FEATURE_ID),
            by = c('Site_ID' = 'SITE_IDENTIFIER')) %>%
  group_by(Site_ID, Subplot, Tree_ID, file_year) %>%
  slice_min(abs(MEAS_YR - file_year), n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  data.table()

core_2021_wide2 <- core_2021_wide1 %>%
  left_join(treedat %>% 
              select(SITE_IDENTIFIER, VISIT_NUMBER, PLOT, TREE_NO, #DBH, 
                     CROWN_CLASS_CODE, RESIDUAL_IND,
                     AGE_MEASURE_CODE, HT_TOTAL, TH_TREE, BORED_HT, BORED_AGE_SOURCE, BORED_AGE_FLAG,
                     SUIT_SI, SUIT_HT, SUIT_AGE, SITE_TREE, AGE_BH) %>% distinct(),
            by = c('Site_ID' = 'SITE_IDENTIFIER', 'VISIT_NUMBER',
                   'Subplot' = 'PLOT', 'Tree_ID' = 'TREE_NO')) %>% data.table() 

core_2021_long <- data_all1 %>%
  filter(objectID %in% core_2021_wide2$objectID)


a1 <- core_old_all3 %>%
  mutate(BCELID = NA,
         Measure_Code = NA,
         Missed_Yrs_To_Pith = NA,
         Lab_Calc_Missed_Yrs = NA,
         Estimated_Distance_to_Pith_mm = NA) %>%
  select(objectID, file_year, BCELID, ID, Project_ID, ContractID, Site_ID, Visit_ID, End_Date, Subplot, Tree_ID,
         Species, DBH, Sample_Tree_Type, Measure_Code, Field_Age, Field_Age_Extrap, Missed_Yrs_To_Pith,
         Field_Comments, Lab_Age, Lab_Age_Extrp, Lab_Calc_Missed_Yrs, Estimated_Distance_to_Pith_mm,
         Lab_Comments, req_num, MEAS_YR, CROWN_CLASS_CODE, RESIDUAL_IND, AGE_MEASURE_CODE, HT_TOTAL,
         BORED_HT, BORED_AGE_SOURCE, BORED_AGE_FLAG, SUIT_SI, SUIT_HT, SUIT_AGE, SITE_TREE, AGE_BH,
         SAMPLE_ESTABLISHMENT_TYPE)

a2 <- core_2021_wide2 %>%
  mutate(Field_Age_Extrap = NA, 
         Lab_Age_Extrp = NA) %>%
  select(objectID, file_year, BCELID, ID, Project_ID, ContractID, Site_ID, Visit_ID, End_Date, Subplot, Tree_ID,
         Species, DBH, Sample_Tree_Type, Measure_Code, Field_Age, Field_Age_Extrap, Missed_Yrs_To_Pith,
         Field_Comments, Lab_Age, Lab_Age_Extrp, Lab_Calc_Missed_Yrs, Estimated_Distance_to_Pith_mm,
         Lab_Comments, req_num, MEAS_YR, CROWN_CLASS_CODE, RESIDUAL_IND, AGE_MEASURE_CODE, HT_TOTAL,
         BORED_HT, BORED_AGE_SOURCE, BORED_AGE_FLAG, SUIT_SI, SUIT_HT, SUIT_AGE, SITE_TREE, AGE_BH,
         SAMPLE_ESTABLISHMENT_TYPE)


core_2017_2021_wide <- rbind.data.frame(a1, a2)


b1 <- core_old_long_all %>%
  select(objectID, ID, Site_ID, Visit_ID, Subplot, Tree_ID, Species, 
         Sample_Tree_Type, Field_Age, Lab_Age, req_num, Year, mm)

b2 <- core_2021_long %>%
  select(objectID, ID, Site_ID, Visit_ID, Subplot, Tree_ID, Species, 
         Sample_Tree_Type, Field_Age, Lab_Age, req_num, Year, mm)

core_2017_2021_long <- rbind.data.frame(b1, b2)


saveRDS(core_2017_2021_wide, "core_2017_2021_wide.rds")
saveRDS(core_2017_2021_long, "core_2017_2021_long.rds")



# suppressed cores
core_2017_2021_wide_final <- core_2017_2021_wide %>%
  filter(SUIT_SI == "Y",
         SAMPLE_ESTABLISHMENT_TYPE %in% c("NFI", "CMI", "YSM")) %>%
  left_join(sampledat_pub %>% select(SITE_IDENTIFIER, FEATURE_ID) %>% distinct(),
            by = c('Site_ID' = 'SITE_IDENTIFIER')) %>% data.table()

core_2017_2021_long_final <- core_2017_2021_long %>%
  filter(objectID %in% core_2017_2021_wide_final$objectID)

# 5-year window
n <- 5

cores1 <- core_2017_2021_long_final %>%
  arrange(Site_ID, Subplot, Tree_ID, desc(Year)) %>%
  group_by(objectID) %>%
  mutate(rolling_mean1 = zoo::rollmean(mm, k = 5, fill = NA, align = "right"),
         rolling_mean2 = zoo::rollmean(mm, k = 10, fill = NA, align = "right")) %>%
  mutate(pgc = ((rolling_mean1 - lead(rolling_mean1, 5)) / lead(rolling_mean1, 5)),
         pgc_sign = c(sign(diff(pgc)), NA),
         pgc1 = ((rolling_mean2 - lead(rolling_mean2, 10)) / lead(rolling_mean2, 10)),
         pgc_sign1 = c(sign(diff(pgc1)), NA)) %>% 
  data.table()

core_suppressed <- cores1 %>%
  filter(pgc >= 1) %>%
  select(objectID, Site_ID, Subplot, Tree_ID, Species, req_num, Lab_Age) %>% distinct()

core_suppressed1 <- cores1 %>%
  filter(pgc >= .5) %>%
  select(objectID, Site_ID, Subplot, Tree_ID, Species, req_num, Lab_Age) %>% distinct()


# add RESULTS info
Result <- fread("/RSLT_ACTIVITY_TREATMENT_SVW_NullGeom/RSLT_ACTRT_NULL_geometry.csv")

Result <- Result %>%
  filter(SILV_BASE_CODE == "DN")

# use VRI spatial overlay to connect with opening id
vri <- read.xlsx("/spatial_overlay/ISMC_VRI_Overlay/2_All_VRI_Attributes_2025nov06.xlsx")

core_suppressed_results <- core_suppressed %>%
  left_join(sitedat_pub %>% select(SITE_IDENTIFIER, MGMT_UNIT) %>% distinct(),
            by = c('Site_ID' = 'SITE_IDENTIFIER')) %>% 
  left_join(vri %>% select(SITE_IDENTIFIER, OPENING_ID) %>% distinct(),
            by = c("Site_ID" = "SITE_IDENTIFIER")) %>%
  left_join(Result %>% select(OPENING_ID, SILV_BASE_CODE, ATU_START_DATE, ATU_COMPLETION_DATE),
            by = c("OPENING_ID")) %>% data.table()

saveRDS(core_suppressed_results, "core_suppressed_results.rds")

