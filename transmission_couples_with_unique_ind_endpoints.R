# 13.7.2020
# Sara Kuitunen
# Transmission of non-transimittable diseases
# Data selection
# MARRIED COUPLES including only STRICT INDEX PERSONS that have NOT RE-MARRIED
# and their disease statuses


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

couples_all <- marriage[, c('TUTKHENK_TNRO', 'PUOLISON_TNRO')]

colnames(couples_all) <- c('id_person_1', 'id_person_2')

print(couples_all[1:40,])
print(dim(couples_all))


# load index data
index <- loadRData('/homes/aliu/DSGE_LRS/input/r_files/index.Rdata')
print(class(index))
print(str(index))
print(head(index))
print(tail(index))

# change to data frame format
index <- as.data.frame(index)
print(class(index))


#### Include only couples in which both are STRICTLY INDEX persons ####

### subset index to include only id's of persons that are born in Finland and not moved abroad (STRICTLY INDEX persons)
print(dim(index))
index <- index[index$SYNTYMAKOTIKUNTA!=200 & is.na(index$ULKOMAILLE_MUUTON_PV), ]
print(dim(index))

### include couples in which both are found in the index_data
print(dim(couples_all))

# subset based on id_person_1
couples_index <- couples_all[couples_all$id_person_1 %in% index$KANTAHENKILON_TNRO, ]

# subset based on id_person_2
couples_index <- couples_index[couples_index$id_person_2 %in% index$KANTAHENKILON_TNRO, ]

print(dim(couples_index))

#### Exclude couples incluging re-married individuals and duplicate couples #####

# id's occuring more than once in id_person_1 column
non_unique_id1 <- unique(couples_index[duplicated(couples_index$id_person_1),'id_person_1'])

# id's occuring more than once in id_person_2 column
non_unique_id2 <- unique(couples_index[duplicated(couples_index$id_person_2),'id_person_2'])

# all id's occuring more than once in either of the id columns
non_unique_ind <- unique(c(non_unique_id1, non_unique_id2))

# remove rows including individuals that occure more than once in either id column
# i.e. remove couples involving individuals re-marrying same or other spouse
couples_no_remarried <- couples_index[!(couples_index$id_person_1 %in% non_unique_ind |
couples_index$id_person_2 %in% non_unique_ind),]

# check
print(dim(couples_no_remarried))

# keep only first occurances of duplicates of the same couple
# SORT switches the orientation of the dataframe (rows to columns) and sorts smaller id numbers to row one
# sorting is APPLIED to every row, i.e. every couple
# DUBLICATES are removed, MARGIN = 2 (columns) because sort switches the orientation of the dataframe
couples_unique_no_remarried <- couples_no_remarried[!duplicated(apply(couples_no_remarried[1:2], 1, sort), MARGIN = 2L),]

# check
print(dim(couples_unique_no_remarried))
print(couples_unique_no_remarried[1:40,])


#### Randomize the order of id's within the couples ####

print(couples_unique_no_remarried[1:40,])

# (by Sakari)
for (i in 1:nrow(couples_unique_no_remarried)) {
    couples_unique_no_remarried[i,c("id_person_1", "id_person_2")] <- couples_unique_no_remarried[i,c("id_person_1", "id_person_2")][sample(1:2)]
}

# check
print(dim(couples_unique_no_remarried))
print(couples_unique_no_remarried[1:40,])



#### Add disease statuses for a given disease ####

# endpoint data including only rows with selected disease and 'ID' & 'ENDPOINT' & EVENT_DATE columns

disease = 'J10_COLD' # common cold
selected_endpoint_first <- endpoints_first_full[endpoints_first_full$ENDPOINT == disease, c('ID', 'ENDPOINT', 'EVENT_F_DATE')]

print(selected_endpoint_first[1:40,])
print(dim(selected_endpoint_first))


# change types to character
couples_unique_no_remarried$id_person_1 <- as.character(couples_unique_no_remarried$id_person_1)
couples_unique_no_remarried$id_person_2 <- as.character(couples_unique_no_remarried$id_person_2)

selected_endpoint_first$ID <- as.character(selected_endpoint_first$ID)

print(str(couples_unique_no_remarried))
print(head(couples_unique_no_remarried))

# Add ENDPOINT and EVENT_F_DATE columns of endpoint data that correspond to the id's of person_1 column

couples_unique_endpoints <- dplyr::left_join(couples_unique_no_remarried, selected_endpoint_first, by = c('id_person_1' = 'ID'))

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



