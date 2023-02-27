#---
#title: "Script for analyzing tidy data from row-col design"
#author: "Xiaofei Zhang"
#date: "5/15/2022"
#output: html_document
#---

#Now I am developing the script for analyzing the tidy data from trials of row-column design in multiple locations. 
#The purpose is to standardize the analysis steps for generate standard report for the advancement meeting.

# install.packages("installr")

# library(installr)

# updateR()


rm(list = ls()) 
library(tidyverse)
library(readxl)
library(yarrr)
library(dplyr)
library(knitr)
library(rmarkdown)
library(statgenSTA)
library(statgenGxE)
library(openxlsx)


master_data = list()
# experiment = "LAEPR_EAR_2022"
# folder = "D:\\OneDrive - CGIAR\\01_2022_2022\\01_trial_trial\\2022_DVGST_DVGST_LAEAR\\LAEPR_EAR_2022May18\\"


experiment = "DMF1C_2022"
folder = "D:\\OneDrive - CGIAR\\03_Git_Git\\2022trials\\HLARC_F1C\\HungLoc_2022\\"
trial_interest = "DMF1C"
year_interest = 2022


#### __1. load the tidy data of trials__\


###__1.1 load the tidy data of trial set#1__\

#The tidy data have the names of "tidy_data4analysis"
#The tidy data passed the __quality control__ in this year or before.
#Need the informations: __trial_folder__, __year_interest__, __trial_interest__
#__Here are the data of trial set #1:__


trial_set_number = 1  # how many trial sets?   -----------------------????

#######################################
#### ---- load trial set #1
# the information required
# trial_folder = "D:\\OneDrive - CGIAR\\01_2022_2022\\01_trial_trial\\2022_DVGST_DVGST_LAEAR\\"
trial_folder = folder


# all files in the folder
list_file = list.files(trial_folder)
# tidy data of the trials interested
sel_file = list_file[str_detect(list_file, "_tidy_data4analysis_") &
                       str_detect(list_file,
                                  paste( year_interest, trial_interest, sep="") )]
sel_file
# the data we will use
sel_file_use = sel_file[1]

trial1_tidy = read.csv( paste(trial_folder,sel_file_use, sep=""), header=TRUE,
                        stringsAsFactors = FALSE,
                        as.is=T,
                        check.names = FALSE)

dim(trial1_tidy)  
names(trial1_tidy)

trial_tidy_all = trial1_tidy





#### __3 obtain all the meta col and trait col using a file in local database__

#### the master colname file
#1) trait names
database_foler = "D:\\OneDrive - CGIAR\\01_2021_2021\\01_CassavaBase_data\\"
trait_all = read_excel(paste(database_foler,
                             "01_standard_col_names_2021March.xlsx",
                             sep=""),
                       sheet="2021June05") %>%
  select(analysis_col_name) %>%
  filter(str_detect(analysis_col_name, "obs_" ) )
trait_all_adj =gsub("obs_", "", trait_all$analysis_col_name)
trait_all_adj = c(trait_all_adj, 
                  "harvest_number_plan", "germination_perc",
                  "yield_ha_v2", "DM_yield"  )

trait_all_adj = gsub("-", "_", trait_all_adj)

#2) meta info.
meta_all = read_excel(paste(database_foler,
                            "01_standard_col_names_2021March.xlsx",
                            sep=""),
                      sheet="2021June05") %>%
  select(analysis_col_name) %>%
  filter(str_detect(analysis_col_name, "use_" ) )
meta_all_adj = gsub("use_", "", meta_all$analysis_col_name)
meta_all_adj = c(meta_all_adj, 
                 "check_released", "latitude", "longitude",
                 "altitude", "department", "country",
                 "ag_zone","location_short" )







#### __4 select the meta info and trait for analysis__

# setdiff(names(trial_tidy_all), c(meta_all_adj, trait_all_adj) )

# all trait for analysis
names(trial_tidy_all) = gsub("-", "_", names(trial_tidy_all))
analysis_trait_v2 = names(trial_tidy_all)[25:53]

# all meta information available
meta_col = names(trial_tidy_all)[names(trial_tidy_all) %in%meta_all_adj]

print("All the meta information:")
print(meta_col)
# View(trial_tidy_all)


dim(trial_tidy_all)


#### __5 check the SD of each trait__

#__We removed the traits without variation__


# mean, SD  by trials
# ---- function ================================

my_dat = trial_tidy_all
analysis_trait = analysis_trait_v2
meta_info = meta_col

# remove columns with all NA
not_all_na = function(x) any(!is.na(x))
my_dat_noNA = my_dat %>% select_if(not_all_na)

mean_trial = my_dat[, c("trial_name",  all_of(analysis_trait))] %>%
  group_by(trial_name) %>%
  summarise_all(mean, na.rm=TRUE)

sd_trial = my_dat[, c("trial_name",  all_of(analysis_trait))] %>%
  group_by(trial_name) %>%
  summarise_all(sd, na.rm=TRUE)
print(sd_trial)

sd_mean = colMeans(sd_trial[, c( analysis_trait)] , na.rm = TRUE)
sd_mean = data.frame (sd_mean) %>%
  rownames_to_column(var = "trait") %>%
  rename(mean_of_sd = sd_mean)

print("The mean of SD of each trait:")
master_data[["mean_of_sd"]] = sd_mean[order(sd_mean$mean_of_sd),]   ## --------save mean of sd


sd_mean_0 = sd_mean %>%
  filter(mean_of_sd == 0 )

if (nrow(sd_mean_0) ==0) {
  #print("Good, no traits without variance.")
  trial_rm_sd = my_dat
}

if (nrow(sd_mean_0) >0) {
  #print("The traits without variation:")
  paged_table(sd_mean_0)
  #print("Remove the traits from the trial data")
  
  
  analysis_trait = analysis_trait[!analysis_trait %in% sd_mean_0$trait]
  
  trial_rm_sd = my_dat %>%
    select(all_of(meta_info), all_of(analysis_trait))
  
}

print("We removed the trait with SD=0")

# remove the filling clones
trial_rm_sd = trial_rm_sd %>%
  filter(!str_detect(accession_name, "DVGST") )
#unique(trial_rm_sd$accession_name)

master_data[["tidy_data_all"]] = trial_rm_sd   ## ----------------- save the tidy data

analysis_trait_v2 = names(trial_rm_sd)[names(trial_rm_sd) %in%analysis_trait_v2]

dim(trial_rm_sd)




#### __6 check the amount of missing data in each trial__

#__0, means all are missing data__

# missing data for each trait
miss_perc = trial_rm_sd %>%
  select(trial_name, all_of(analysis_trait_v2)) %>%
  group_by(trial_name) %>%
  summarise_all( funs(1-mean(is.na(.))) ) %>%
  ungroup()


master_data[["miss_perc"]] = miss_perc   ## ----------------- save the missing perc


View(miss_perc)



#### __7. count of clones in each trial and environment__


## ****************** FUNCTION START ***********

my_dat = trial_rm_sd
dim(trial_rm_sd)
clones = unique(my_dat$accession_name)
### 1.3.1 total plots per clone
clone_plotNUM = my_dat %>%
  count(accession_name) %>%
  arrange(accession_name) %>%
  rename(plot_ct = n )


### 1.3.2 total trials per clone
clone_envNUM = my_dat %>%
  distinct(accession_name, trial_name) %>%
  count(accession_name) %>%
  arrange(accession_name) %>%
  rename(trial_ct = n ) %>%
  select(accession_name, trial_ct)


### 1.3.3 number trials per year per clone
years = unique(my_dat$year)
clone_yrNUM = data.frame(matrix(nrow = length(clones),
                                ncol = 1+length(years)))
colnames(clone_yrNUM) = c("accession_name", paste(years, "_ct", sep=""))
env_yr_clone = subset(my_dat, select = c("accession_name", "trial_name", "year"))

for(i in 1:length(clones)){
  env_clone_i = subset(env_yr_clone, accession_name == clones[i])
  clone_yrNUM[i,1] = clones[i]
  for(j in 1:length(years)){
    year_j = subset(env_clone_i, year ==years[j])
    clone_yrNUM[i,c(j+1)] =  length(unique(year_j$trial_name))
  }
}

### 1.3.4 number trials per location per clone
locations = unique(my_dat$location_short)
clone_locNUM = data.frame(matrix(nrow = length(clones),
                                 ncol = 1+length(locations)))
colnames(clone_locNUM) = c("accession_name", paste(locations, "_ct", sep=""))
env_loc_clone = subset(my_dat, select = c("accession_name", "trial_name", "location_short"))

for(i in 1:length(clones)){
  env_clone_i = subset(env_loc_clone, accession_name == clones[i])
  clone_locNUM[i,1] = clones[i]
  for(j in 1:length(locations)){
    location_j = subset(env_clone_i, location_short ==locations[j])
    clone_locNUM[i,c(j+1)] =  length(unique(location_j$trial_name))
  }
}

### 1.3.5 merge 1.3.1, 1.3.2, 1.3.3, 1.3.4
ct_1 = merge(clone_plotNUM, clone_envNUM, by="accession_name")
ct_2 = merge(ct_1, clone_yrNUM, by="accession_name")
clone_ct_info = merge(ct_2, clone_locNUM, by = "accession_name") %>%
  arrange( desc(plot_ct) )


master_data[["clone_ct_info"]] = clone_ct_info   ## ------------ save the clone count

## ****************** FUNCTION END ************************************ ##









#### __8. visualize the variation within and among trials using boxplot__


# ---- function

my_dat = trial_rm_sd
# all the traits
#trait_wanted = analysis_trait_v2
trait_ideal = c("yield_ha_v2",
                "DM_yield",
                "DM_gravity",
                "plant_type",
                "germination_perc",
                "branch_number" ,
                "height",
                "height_1st_branch"  ,
                "height_wt_leaf",
                "root_number_commercial",
                "root_type1_5",
                "root_skin_color1_3",
                "vigor1_5",
                "lodging1_3",
                "thrips1_5",
                "root_length1_3",
                "root_rot_number" ,
                "root_peduncle1_3",
                "root_shape1_6",
                
                "branch_angle",
                "stake_plant",
                "root_constriction1_3" ,
                "carotenoid1_8"                
)

setdiff(trait_ideal, analysis_trait_v2)
setdiff(analysis_trait_v2, trait_ideal)

trait_wanted = analysis_trait_v2

# remove columns with all NA
not_all_na = function(x) any(!is.na(x))
my_dat_noNA = my_dat %>% select_if(not_all_na)

# save as PDF, can adjust the figure size
pdf(paste(folder, "01_", experiment, "_boxplot_",
          Sys.Date(),".pdf", sep=""), width = 4, height = 6)

for(i in 1: length(trait_wanted)){
  y_DATA = my_dat_noNA[[trait_wanted[i]]]   # data frame or vector?
  x_DATA = my_dat_noNA$trial_name
  my_DATA = my_dat_noNA
  y_LABEL = trait_wanted[i]
  x_LABEL = NULL
  TITLE = NULL
  y_MAX = max(y_DATA, na.rm = TRUE) * 1.2
  y_MIN = 0
  
  plot_box = ggplot(my_DATA, aes(x = x_DATA, y = y_DATA))+
    geom_violin(trim=FALSE, fill="gray")+
    geom_boxplot(width=0.3) +
    coord_cartesian(ylim = c(y_MIN,y_MAX))+
    theme(axis.text.x = element_text(face="bold", colour="black", size=12, angle = 45, hjust = 1),
          axis.text.y = element_text(face="bold", colour="black", size=12),
          axis.title.y=element_text(size=14,face="bold", angle = 90, vjust = 3) ,
          axis.title.x=element_text(size=14,face="bold", vjust = -0.5) ,
          plot.title = element_text(color="red", size=16, face="bold.italic", hjust = 0.5),
          plot.margin = unit(c(1,1,1,2), "cm"), # top, right, bottom, left
          legend.position = "none"
    )  +
    labs(y = y_LABEL , x = x_LABEL,
         title = TITLE)
  plot(plot_box)
}
dev.off()

## ****************** FUNCTION END ************************************ ##





#### ----------------------------------- change into NA for outliers
if(false) {
  trial_rm_sd = trial_rm_sd %>%
  mutate(root_rot_number =
           ifelse(root_rot_number >20, NA, root_rot_number) )

unique(trial_rm_sd$root_rot_number)


} 








#### __9 plot layout__

unique(trial_rm_sd$location_short)

data_MET_kp = trial_rm_sd # %>%
# filter(!location_short %in%"ciat" )


data_MET_kp = trial_rm_sd %>%
  filter(!location_short %in%"ciat" )

dim(data_MET_kp)


accession_rep_ct = trial_rm_sd %>%
  count(trial_name, accession_name, rep_number)  %>%
  arrange(trial_name) %>%
  filter(n>1)
print(accession_rep_ct)

## Create a TD object with the phenotypic data
td<- createTD(data_MET_kp, genotype = 'accession_name', trial = 'trial_name',
              loc = 'location_short', year = 'year', repId = 'rep_number',
              rowCoord = 'row_number', colCoord = 'col_number',
              rowId = 'row_number', colId = 'col_number',
              trLat = "latitude", trLong = "longitude",
              trDesign = 'res.rowcol'
)



# only print layout of selected trials
# save as PDF, can adjust the figure size

unique(data_MET_kp$trial_name)
pdf(paste(folder, "01_", experiment, "_layout_example_",
          Sys.Date(),".pdf", sep=""), width = 10, height = 10)

plot(td, showGeno = T,
     highlight = c("CR24-16",
                   "IITA-TMS-IBA980581",
                   "KM505",
                   "KU50",
                   "TMEB419"
     ) #,
)#trials = c("202114LAEPR_momi", "202061LAEPR_repe"))


dev.off()



#### __9.1 location map__

pdf(paste(folder, "01_", experiment, "_location_map_",
          Sys.Date(),".pdf", sep=""), width = 6, height = 6)

plot(td, plotType = "map",
     minLatRange = 6, minLongRange = 6)


dev.off()








####################################
#### 2023 Feb test agriutilities
####################################

#install.packages("agriutilities")
library(agriutilities)
library(agridat)

# from line 514
data_MET_kp = trial_rm_sd # %>%
# filter(!location_short %in%"ciat" )

# data_MET_kp = trial_rm_sd %>%
#  filter(!location_short %in%"ciat" )
names(data_MET_kp)
trait_MET = analysis_trait_v2[!analysis_trait_v2 %in%c("germinated_number_plot", "germination_perc")]


#data(besag.met)
#dat <- besag.met
dat = data_MET_kp
results <- check_design_met(
  data = dat,
  genotype = "accession_name",
  trial = "trial_name",
  traits = trait_MET,
  rep = "rep_number",
  col = "col_number",
  row = "row_number"
)
print(results)
obj <- single_trial_analysis(results, progress = TRUE, 
                             remove_outliers = FALSE,
                             engine = "asreml")


# only traits in more than two env
trait_MET_kp = as.character( (data.frame(table(obj$resum_fitted_model$trait)) %>%
  filter(Freq==2) )$Var1 )

trait_MET_kp = trait_MET_kp[!trait_MET_kp %in% c("harvest_number",
                                                 "root_number")]

results_kp <- check_design_met(
  data = dat,
  genotype = "accession_name",
  trial = "trial_name",
  traits = trait_MET_kp,
  rep = "rep_number",
  col = "col_number",
  row = "row_number"
)

obj_MET <- single_trial_analysis(results_kp, progress = TRUE, 
                             remove_outliers = FALSE,
                             engine = "asreml")
obj_MET$resum_fitted_model
met_results <- met_analysis(obj_MET, progress = TRUE,
                            h2_filter = 0 )



master_data_agri = list()
master_data_agri[["summ_traits"]] = results$summ_traits
master_data_agri[["exp_design_resum"]] = results$exp_design_resum
master_data_agri[["connectivity_matrix"]] = results$connectivity_matrix
master_data_agri[["resum_fitted_model"]] = obj$resum_fitted_model
master_data_agri[["blues_blups_single"]] = obj$blues_blups
master_data_agri[["trial_effects"]] = met_results$trial_effects
master_data_agri[["overall_BLUPs"]] = met_results$overall_BLUPs
#master_data_agri[["VCOV"]] = met_results$VCOV
master_data_agri[["stability"]] = met_results$stability
master_data_agri[["heritability"]] = met_results$heritability

blups_all = met_results$overall_BLUPs %>%
  select(!status) %>%
  rename(BLUPs = predicted.value,
         seBLUPs = std.error) %>%
  pivot_wider(names_from = "trait", values_from = c("BLUPs", "seBLUPs"))


trait_MET = trait_MET[!trait_MET %in% c("mites_3mon"
                                        )]

blues_blups = obj$blues_blups %>%
  select(trait, genotype, trial, BLUEs, seBLUEs) %>%
  pivot_wider(names_from = "trait", values_from = c("BLUEs", "seBLUEs")) %>%
  pivot_wider(names_from = trial, values_from = c(paste("BLUEs", trait_MET, sep = "_"), 
                                                  paste("seBLUEs", trait_MET, sep = "_") )  ) %>%
  left_join(blups_all, by="genotype")

header_sort = vector()
for (i in 1:length(trait_MET)) {
  
  header_sort = c(header_sort, 
                  grep(trait_MET[i], sort(names(blues_blups)), value=TRUE) 
  )
  
}

blues_blups= blues_blups%>%
  select(genotype, all_of(header_sort) )

master_data_agri[["blues_blups_MET"]] = blues_blups


blue_blup_agri = paste(folder, "01_BLUE_BLUP_marster_agri",
                       paste(year_interest, trial_interest, "_", sep=""),
                       Sys.Date(),
                       ".xlsx", sep="")


write.xlsx(master_data_agri, file = blue_blup_agri)



met_results$VCOV
