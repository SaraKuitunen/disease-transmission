# 10.7.2020
# Sara Kuitunen
# Transmission of non-transimittable diseases
# Data selection
# VERSION 2: ALL MARRIED COUPLES with UNIQUE individuals and their disease statuses


#### install and load packages ####

# path to the folder including the packages

.libPaths('/homes/skuitune/Rlibrary')
library(dplyr)

#### load the data sets ####

# function to load an RData file
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# load pedigree data
# check that the file exists
# file.exists('/homes/aliu/DSGE_LRS/input/r_files/ped_all.Rdata')
#pedigree <- loadRData('/homes/aliu/DSGE_LRS/input/r_files/ped_all.Rdata')

#print(class(pedigree))
#print(str(pedigree))
#print(head(pedigree))
#print(tail(pedigree))


# load endpoints data of FIRST DIAGNOSES
# load endpoint data of first diagnoses: ry_first_all.sas7bdat 
# endpoints_first_full <- loadRData('/homes/aliu/DSGE_LRS/input/r_files/ry_first_all.Rdata')

# load a NEW endpoints data of FIRST DIAGNOSES of INDEX persons 
endpoints_first_full <- loadRData('/homes/aliu/DSGE_LRS/input/r_files/ry_first_indexW_COMPLETE.Rdata')

print(class(endpoints_first_full))
print(str(endpoints_first_full))
print(head(endpoints_first_full))
print(tail(endpoints_first_full))
print(is.recursive(endpoints_first_full))

# change to data frame format
endpoints_first_full <- as.data.frame(endpoints_first_full)
print(class(endpoints_first_full))

# load marriage data
marriage <- loadRData('/homes/aliu/DSGE_LRS/input/r_files/thl2019_804_avio.Rdata')

print(class(marriage))
print(str(marriage))
print(head(marriage))
print(tail(marriage))

# change to data frame format
marriage <- as.data.frame(marriage)
print(class(marriage))

# load index data 
index <- loadRData('/homes/aliu/DSGE_LRS/input/r_files/index.Rdata')
print(class(index))
print(str(index))
print(head(index))
print(tail(index))

# change to data frame format
index <- as.data.frame(index)
print(class(index))


#### Combine pedigree and marriage data ####

# data frame of father_id and mother_id columns from pedigree data
#couples_pedigree <- pedigree[,c('mother_id', 'father_id')]

#print(head(couples_pedigree))
#print(dim(couples_pedigree))

# data frame of TUTKHENK_TNRO (index_id) and PUOLISON_TNRO (spouse_id) columns from marriage data
couples_marriage <- marriage[, c('TUTKHENK_TNRO', 'PUOLISON_TNRO')]

print(head(couples_marriage))
print(dim(couples_marriage))

# set columnnames to be same in both 
# colnames(couples_pedigree) <- c('id_person_1', 'id_person_2')
colnames(couples_marriage) <- c('id_person_1', 'id_person_2')

# rbind together
#couples_all <- rbind(couples_pedigree, couples_marriage)

# married couples are all couples to be included
couples_all <-couples_marriage

print(couples_all[1:40,])
print(dim(couples_all))


#### Make the unique couples ####

# DO NOT SELECT ONLY UNIQUE ID's IN A COLUMN as that would exclude cases of re-marrying/ having child(ren) someone else 
# or having chil(ren) or chil(ren) and marriage -> exclude only DUBLICATES OF THE SAME COUPLE

# see how many individuals occur more than once in the id column
# NOTE, that 'unique' function is needed 
non_unique_id1 <- unique(couples_all[duplicated(couples_all$id_person_1),'id_person_1'])
print(length(non_unique_id1))

non_unique_id2 <- unique(couples_all[duplicated(couples_all$id_person_2),'id_person_2'])
print(length(non_unique_id2))

# exclude duplicate couples
  # keep only unique couples
  # SORT switches the orientation of the dataframe (rows to columns) and sorts smaller id numbers to row one 
  # sorting is APPLIED to every row, i.e. every couple
  # DUBLICATES are removed, MARGIN = 2 (columns) because sort switches the orientation of the dataframe
couples_unique <- couples_all[!duplicated(apply(couples_all[1:2], 1, sort), MARGIN = 2L),]

print(couples_unique[1:40,])
print(dim(couples_unique))


#### Find all individuals that occur more than once in the data ####

# same individual occurs more than once in a id_person_1 column AFTER exluding duplicate couples
non_unique_id1 <- unique(couples_unique[duplicated(couples_unique$id_person_1),'id_person_1'])
print(length(non_unique_id1))

# same individual occurs more than once in a id_person_2 column AFTER exluding duplicate couples
non_unique_id2 <- unique(couples_unique[duplicated(couples_unique$id_person_2),'id_person_2'])
print(length(non_unique_id2))

# see how many cases there are where one individual exists in both id columns
print(length(unique(couples_unique[couples_unique$id_person_1 %in% couples_unique$id_person_2,'id_person_1'])))

# check to be same that above
id_in_both <- unique(couples_unique[couples_unique$id_person_2 %in% couples_unique$id_person_1,'id_person_2'])
print(length(id_in_both))

# all individuals occuring more than once in either of the id columns and/or occuring in both id columns
non_unique_ind <- unique(c(non_unique_id1, non_unique_id2, id_in_both))
print(length(non_unique_ind))

#### Exclude all couples including individuals that occur more than once in the data ####

print(dim(couples_unique))

# select only rows where non_unique_ind id is not in either id column
couples_unique <- couples_unique[!(couples_unique$id_person_1 %in% non_unique_ind | couples_unique$id_person_2 %in% non_unique_ind),]

print(dim(couples_unique))


#### Include only couples in which both are STRICTLY INDEX persons ####

### subset index to include only id's of persons that are born in Finland and not moved abroad (STRICTLY INDEX persons)
print(dim(index))
index <- index[index$SYNTYMAKOTIKUNTA!=200 & is.na(index$ULKOMAILLE_MUUTON_PV), ]
print(dim(index))

### include couples in which both are found in the index_data
print(dim(couples_unique))

# subset based on id_person_1
couples_unique <- couples_unique[couples_unique$id_person_1 %in% index$KANTAHENKILON_TNRO, ]

# subset based on id_person_2
couples_unique <- couples_unique[couples_unique$id_person_2 %in% index$KANTAHENKILON_TNRO, ]

print(dim(couples_unique))


#### Randomize the order of id's within the couples ####
# (by Sakari)
print(head(couples_unique))

for (i in 1:nrow(couples_unique)) {
  couples_unique[i,c("id_person_1", "id_person_2")] <- couples_unique[i,c("id_person_1", "id_person_2")][sample(1:2)]
}

print(head(couples_unique))


#### Add disease statuses for a given disease ####

# endpoint data including only rows with selected disease and 'ID' & 'ENDPOINT' & EVENT_DATE columns

disease = 'J10_COLD' # common cold
selected_endpoint_first <- endpoints_first_full[endpoints_first_full$ENDPOINT == disease, c('ID', 'ENDPOINT', 'EVENT_F_DATE')]

print(selected_endpoint_first[1:40,])
print(dim(selected_endpoint_first))


# change types to character
couples_unique$id_person_1 <- as.character(couples_unique$id_person_1)
couples_unique$id_person_2 <- as.character(couples_unique$id_person_2)

selected_endpoint_first$ID <- as.character(selected_endpoint_first$ID)

print(str(couples_unique))
print(head(couples_unique))

# Add ENDPOINT and EVENT_F_DATE columns of endpoint data that correspond to the id's of person_1 column

couples_unique_endpoints <- dplyr::left_join(couples_unique, selected_endpoint_first, by = c('id_person_1' = 'ID'))

print(couples_unique_endpoints[1:40,])
print(dim(couples_unique_endpoints))

# change the columnname of ENDPOINT and EVENT_F_DATE so that joining can be used to get endpoints person_2 column too
colnames(couples_unique_endpoints) <- c('id_person_1', 'id_person_2', 'person_1_endpoint', 'person_1_date')

# get endpoint for individuals in person_2 column
couples_unique_endpoints <- dplyr::left_join(couples_unique_endpoints, selected_endpoint_first, by = c('id_person_2' = 'ID'))

# change the columnname of 'ENDPOINT' to be more explaining
colnames(couples_unique_endpoints) <- c('id_person_1', 'id_person_2', 'person_1_endpoint', 'person_1_date', 'person_2_endpoint', 'person_2_date')

print(couples_unique_endpoints[1:40,])
print(dim(couples_unique_endpoints))

# replace NA's by 'no_disease'
couples_unique_endpoints$person_1_endpoint <- as.character(couples_unique_endpoints$person_1_endpoint)
couples_unique_endpoints$person_1_endpoint[is.na(couples_unique_endpoints$person_1_endpoint)] <- 'no_disease'

couples_unique_endpoints$person_2_endpoint <- as.character(couples_unique_endpoints$person_2_endpoint)
couples_unique_endpoints$person_2_endpoint[is.na(couples_unique_endpoints$person_2_endpoint)] <- 'no_disease'

# check the data
print(couples_unique_endpoints[1:40,])

print(dim(couples_unique_endpoints))

print(unique(couples_unique_endpoints$person_1_endpoint))

print(unique(couples_unique_endpoints$person_2_endpoint))

# check is there any NA's in id columns
print(any(is.na(couples_unique_endpoints$id_person_1)))

print(any(is.na(couples_unique_endpoints$id_person_2)))

#### Save the data ####
# save as csv 
write.csv(couples_unique_endpoints, '/homes/skuitune/data/married_couples_endpoints.csv', row.names = FALSE)


#### END OF THE SCRIPT ####