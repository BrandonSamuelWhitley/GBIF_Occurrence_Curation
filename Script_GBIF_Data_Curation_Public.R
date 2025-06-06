#Code Author - Brandon Samuel Whitley
#Author Title - University of Copenhagen PhD Fellow in Arctic Plant Diversity and Pollination Networks
#Author Contact Information - brandon.s.whitley@snm.ku.dk
#Date of Creation - January 2023 - Last Edited April 2025 
#Project Affiliated - "Greenland plant diversity patterns and pollination networks in a changing Arctic"

#Code Description -      This script has been designed to download metacollections of biodiversity plant
#                        nomenclature data from GBIF, usually for a given geographic scope (up to 3 countries).
#                        It then runs the names ascribed to the specimens through some standardization scripts, 
#                        and then checks the names against the KEW Plants of the world (POWO) database. For names which
#                        are recognized, it notes their Accepted Name. For names which are synonyms, it re-assigns
#                        the name to the accepted name. During the process, it stores all known synonym names, and 
#                        all names the taxa are listed as in the GBIF database, to allow for the accessing data 
#                        across many data sources. For cases where the name is not recognized, it is put through a 
#                        manual taxonomic expert review process to correct it, or to further leave it as unrecognized. 
#                        Then, an independent reference flora for the region is uploaded into the data, and the names
#                        are cross-checked against the reference flora. Any unrecognized names which are now recognized
#                        using the reference flora are now re-added, along with any reference flora names which were not
#                        already in the list. Remaining unrecognized names are left for manual review. After, names
#                        are flagged according to criteria of minimum specimen count, most recent contribution to the collection, and minimum data source count,
#                        while also automatically flagging occurrences derived exclusively from human observation. Flagged names
#                        undergo review by experts in the flora to determine if they need to be removed or kept. 
#                        Thereafter, the Taxon List is compared to an external list (default using the POWO database but this can
#                        also be further supplemented). By doing this, species unique to this dataset are filtered, as they contain
#                        the most potential for error. These species are then manually checked and verified or removed as needed. 
#                        The final list of taxa is also checked manually one last time. 

#                        This process produces a curated metacollection of GBIF occurrence data, harmonized by Accepted Name. In doing
#                        this, it also results in a curated Taxon List for the region at hand, which summarizes the contents and state of curation of the metacollection. 
#                        Additionally, it also records edits made to data, and produces an output of any removed names and why, along with 
#                        the GBIF occurrences affiliated to those names. 
#
#                        This script was written as a part of a PhD project. This script has been used for Greenland as a case study, and thus 
#                        contains many examples for Greenland in the code annotation. Note though that these examples can be found in other occurrence
#                        data too, and so they were kept in as examples for the user to understand. 

#                        This code is affiliated to the paper "Harmonising digitised herbarium data to enhance biodiversity knowledge: major steps
#                        towards an updated checklist for the flora of Greenland"

#Input Information Required - 
# (1) GBIF Login Information for the given user
# (2) An external Reference Flora, where 
#Column 1 labelled as Accepted_Name_with_Author, containing the Accepted Name and the author of the Species, in ICN format
#Column 2 labelled as Accepted_Name, containing just the Accepted Name of the species
#Additional n columns labelled as Synonym_n for n synonyms, containing the Synonym Name with author
#Additional columns may contain other relevant information, such as the taxa rank, distribution, etc.
#If it contains a taxa rank column, please call it rank" )
#Optional (3) - External Floras to compare data to - Single column Excel sheet with Accepted Name with Author (POWO standardized - this is critical, as otherwise synonyms will appear as unmatched taxa names)
 

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Start Script - Setting Working Directory and Loading Packages
setwd() #Set working directory here

#Install packages if not already installed
list.of.packages <- c("Taxonstand", "readxl", "plyr","tidyr", "stringr", "ggplot2","reshape2","tibble", "data.table","purrr","dplyr", "taxize", "projmgr", "kewr", "flora", "fuzzyjoin", "tidyverse", "writexl", "rgbif", "progress","stringi", "ggdendro", "ggbreak", "networkD3", "openxlsx", "gridExtra", "plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#Additional Packages from Github:
#devtools::install_github("barnabywalker/kewr") #In case this does not work from list.of.packages
#install_github("gustavobio/flora")             #In case this does not work from list.of.packages

#Load all packages not already loaded 
invisible(lapply(list.of.packages, library, character.only = TRUE))
library("dplyr") #Make sure it loads after plyr
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Create folder for storing the results of today's analysis. 
#The folder shall be named Results_Day_Month_Year. 
{
#Get the current date in the specified format
current_date <- format(Sys.Date(), format = "%d_%b_%Y")

#Create the folder name with the date
folder_name <- paste("Results", current_date, sep = "_")

#Create the folder in the working directory
dir.create(folder_name, showWarnings = FALSE)
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Define Sources and Input Data

#Define a progress bar function to monitor large GBIF downloads, as this takes time
GBIF_download_status <- function(GBIF_Download) {
  pb <- progress_bar$new(
    format = "Downloading data [:bar] :percent",
    total = 100, 
    clear = FALSE
  )
  repeat {
    #Get the current status of the download
    status <- occ_download_meta(GBIF_Download)
    
    #Check if the status is complete or failed
    if (status$status == "SUCCEEDED") {
      pb$tick(100)  #Complete progress
      cat("Download complete!\n")
      break
    } else if (status$status == "FAILED") {
      cat("Download failed.\n")
      break
    }
    #Update progress 
    pb$tick(1) #Increment progress by 1%
    #Wait for 30 seconds before checking again
    Sys.sleep(30)
  }
}


#Extract data directly from GBIF with appropriate filters:

#Define filter for Vascular Plants 
#Retrieve the unique taxon ID for Tracheophyta, to limit search to vascular plants. Use name_backbone to search for name value, and get affiliated usageKey ID:
Taxon_to_Query <- readline(prompt = "Please enter the taxon group of interest for querying GBIF - for all vascular plants please write Tracheophyta (Press ENTER for default Tracheophyta): ")

#Trim any leading or trailing whitespace
Taxon_to_Query <- trimws(Taxon_to_Query)

#If the input is empty or contains specific unwanted text, set default value to 'Tracheophyta'
if (Taxon_to_Query == "" || 
    grepl("^[#].*", Taxon_to_Query) || 
    grepl("^\\s*#.*", Taxon_to_Query)) {
  Taxon_to_Query <- "Tracheophyta"
}

#Print the selected taxon to confirm
cat("Taxon to query:", Taxon_to_Query, "\n")

Tracheophyta_ID <- name_backbone(name = Taxon_to_Query)$usageKey

#Enter the country or countries of interest for querying GBIF
Country_List <- readline(prompt = "Please enter the country name(s) for querying GBIF (separated by commas) - up to 3 country queries can be downloaded at once: ") %>%
  strsplit(",") %>%        #Split the string into separate country names
  .[[1]] %>%               #Access the only element of the split result
  trimws()                #Trim any leftover spaces from each country name

#Search for the country codes as are used in GBIF, specific to the countries being queried 
Country_Codes <- enumeration_country() %>% 
  filter(title %in% Country_List) %>%
  select(iso2)

#Take codes for each country and combine into a list 
Country_Codes <- Country_Codes %>% 
  pull(iso2)

GBIF_username <- readline(prompt = "Please enter your GBIF username: ") 
GBIF_password <- readline(prompt = "Please enter your GBIF password: ") 
GBIF_email <- readline(prompt = "Please enter the email affiliated to your GBIF account: ") 

#Loop through each country to be queried

#List to store results 
all_downloads <- list()

#Prompt the user to decide whether to exclude human observations
exclude_human_observations <- readline(prompt = "Do you want to exclude human observations? (YES/NO, default is NO): ")

#If the input is exactly "YES" (case insensitive), treat it as TRUE, otherwise FALSE
exclude_human <- ifelse(toupper(exclude_human_observations) == "YES", TRUE, FALSE)

#Loop through each country code
for (country_code in Country_Codes) {
#Start GBIF Query 
GBIF_Download <- occ_download(
  pred_and(
    pred("taxonKey", Tracheophyta_ID),
    pred("country", country_code),                     #Country predicates (can be single country or multiple)
    pred("occurrenceStatus", "PRESENT"),               #Only include records where the species is present
    #Exclude human observations if exclude_human is TRUE
    pred_not(pred("basisOfRecord", if (exclude_human) "HUMAN_OBSERVATION" else "FOSSIL_SPECIMEN")),
    #Always exclude fossil specimens - (GBIF records can include living specimen, preserved specimen, fossil specimen, material citation, human observation, and machine observation)
    pred_not(pred("basisOfRecord", "FOSSIL_SPECIMEN"))
  ),
  format = "SIMPLE_CSV",
  user = GBIF_username,         
  pwd = GBIF_password,          
  email = GBIF_email     
)

#Store the download object
all_downloads[[country_code]] <- GBIF_Download
}

#Check the download progress
GBIF_download_status(GBIF_Download)

#!!!#!!!#!!!#

#Wait for download to complete 

#Create a documentation list to store results over time:
Documentation_Results <- list()

#Create a list to store the combined results
Combi_List1 <- list()
DOI_list <- list() #also store the download DOIs for repeatability 

#Loop through each country code to retrieve and import data
for (country_code in names(all_downloads)) {
  #Extract the download object using the download key generated 
  GBIF_Downloaded_File <- occ_download_get(all_downloads[[country_code]], overwrite = TRUE)
  #Import the data using that key
  country_data <- occ_download_import(GBIF_Downloaded_File)
  #Append the results to the Combi_List1 to account for multiple countries 
  Combi_List1[[country_code]] <- country_data
  #Extract metadata for the download
  meta <- occ_download_meta(all_downloads[[country_code]])
  #Retrieve the DOI from the metadata and store it in the list
  DOI_list[[country_code]] <- meta$doi
}
#Combine all data into a single data frame Combi_List
Combi_List <- do.call(rbind, Combi_List1)

Documentation_Results$TotalDownload<-nrow(Combi_List)

save.image(file = "Taxa_List_Save_0.RData") #Save everything 
#load("Taxa_List_Save_0.RData") #Can use these checkpoints to load data in the event of editing later in the script. 

{
#Filter to only keep data with media affiliated to them, as media is essential for being able to validate the ID of the specimen 
Combi_List <- Combi_List[!is.na(Combi_List$mediaType) & Combi_List$mediaType != "", ]

Combi_List <- Combi_List %>%
  mutate(verbatimScientificName = trimws(verbatimScientificName), 
    verbatimScientificNameAuthorship = trimws(verbatimScientificNameAuthorship),
    scientificName = trimws(scientificName))

Combi_List$scientificName <- gsub ("ssp.","subsp.", Combi_List$scientificName)
Combi_List$verbatimScientificName <- gsub ("ssp.","subsp.", Combi_List$verbatimScientificName)

Combi_List$verbatimScientificNameAuthorship <- gsub(", orth\\.", "", Combi_List$verbatimScientificNameAuthorship) #Remove orth signatures 
Combi_List$scientificName <- gsub(", orth\\.", "", Combi_List$scientificName) #Remove orth signatures 
Combi_List$verbatimScientificNameAuthorship <- gsub(", nom\\. cons\\.", "", Combi_List$verbatimScientificNameAuthorship)
Combi_List$scientificName <- gsub(", nom\\. cons\\.", "", Combi_List$scientificName)

Combi_List_All_Media_Downloads<-Combi_List #Store 

save.image(file = "Taxa_List_Save_1.RData") #Save everything 
#load("Taxa_List_Save_1.RData")


#Get the DOI information 
formatted_DOI_list <- sapply(names(DOI_list), function(country_code) {
  paste(country_code, "-", DOI_list[[country_code]], sep = "")
})
}

{
#Convert the result to a single character vector
DOI_list <- as.vector(formatted_DOI_list)

#Document Results
Documentation_Results$Zero<-DOI_list
Documentation_Results$One<-nrow(Combi_List)

#There are cases where a verbatim scientific name is interpreted at Family level instead of species level. 
Combi_List <- Combi_List %>%
  rowwise() %>%
  mutate(scientificName = ifelse(
      str_ends(scientificName, "eae") & 
        !str_ends(verbatimScientificName, "eae") &  
        !str_ends(verbatimScientificName, " sp.$") &  
        !str_ends(verbatimScientificName, " ×$") &  
        !str_detect(verbatimScientificName, fixed("indet", ignore_case = TRUE)),  
      ifelse(
        verbatimScientificNameAuthorship != "",
        paste(verbatimScientificName, verbatimScientificNameAuthorship),  
        verbatimScientificName),
      scientificName)) %>%ungroup()

#Also repair cases of hybrids which may have been lost
Combi_List <- Combi_List %>%
  rowwise() %>%
  mutate(scientificName = ifelse(
      taxonRank == "GENUS" & 
        (str_detect(verbatimScientificName, fixed(" x ", ignore_case = TRUE)) |  
           str_detect(verbatimScientificName, fixed(" ×", ignore_case = TRUE))),  
      ifelse(
        !str_detect(scientificName, fixed(" x ", ignore_case = TRUE)) & 
          !str_detect(scientificName, fixed(" x", ignore_case = TRUE)) &  
          !str_detect(scientificName, fixed(" ×", ignore_case = TRUE)) &  
          !str_detect(scientificName, fixed(" × ", ignore_case = TRUE)) &  
          !str_ends(verbatimScientificName, "×"), 
        verbatimScientificName,  
        scientificName),scientificName)) %>%ungroup()

#Document Results
Documentation_Results$Original_Names <- length(unique(Combi_List$scientificName))

#Note that GBIF will sometimes change the scientificName to a higher taxonomic resolution when it does not 
#recognize the name. However, this can result in names being lost if they are not recognized, but they may still
#be legitimate. These cases are flagged by "TAXON_MATCH_HIGHERRANK" in the issue column. 

#However to further complicate things, there are cases where there is a lower taxonomic resolution specified, such as 
#Potentilla alpestris, but it does not have any authority name associated to it. Then GBIF cannot place it, as it could be
#"Potentilla alpestris Haller f." or "Potentilla alpestris G.Lodd." or "Potentilla alpestris Host"(even though these names are unplaced)
#So instead, GBIF moves to the next highest taxonomic level, such as "Potentilla L.". Doing this reduces the resolution of the data,
#but also increases accuracy at the new, higher resolution. 

#To deal with this, first filter these cases out:
Combi_List_TaxonIssue<-Combi_List %>%
  filter(str_detect(issue, "TAXON_MATCH_HIGHERRANK"))

#Catch remaining cases:
Combi_List_TaxonIssue_Add <- Combi_List %>%
  filter(scientificName == verbatimScientificName &  
      verbatimScientificNameAuthorship == "" &   
      taxonRank == "SPECIES" &                   
      (str_count(scientificName, "\\S+") == 2 |(str_detect(scientificName, "subsp\\.|var\\.| f\\. ") & str_count(scientificName, "\\S+") == 4))) %>%
  mutate(issue = "TAXON_MATCH_HIGHERRANK")

if (nrow(Combi_List_TaxonIssue_Add) > 0) {
  Combi_List_TaxonIssue <- rbind(Combi_List_TaxonIssue, Combi_List_TaxonIssue_Add)
}

#Document Results
Documentation_Results$Two<-nrow(Combi_List_TaxonIssue)
Documentation_Results$Combi_List_TaxonIssue<-length(unique(Combi_List_TaxonIssue$verbatimScientificName))

Combi_List_TaxonIssue <- Combi_List_TaxonIssue %>%
  mutate(verbatimScientificNameAuthorship = na_if(verbatimScientificNameAuthorship, ""))

#Also repair cases when there is a space in the author spelling which should not be present. EG "Potentilla groenlandica R. Br.' should be "Potentilla groenlandica R.Br."
#Use regex to remove the space between a period and a capital letter
Combi_List$scientificName <- str_replace_all(Combi_List$scientificName, "(?<=\\s)[A-Z]\\.\\s[A-Z]", function(x) gsub("\\s", "", x))
Combi_List$scientificName <- str_replace_all(Combi_List$scientificName, "\\.\\s([A-Z])", ".\\1") #catch remaining cases 
Combi_List$scientificName <- str_trim(Combi_List$scientificName)

Combi_List<-Combi_List %>%
  filter(!str_detect(issue, "TAXON_MATCH_HIGHERRANK"))

Combi_List<-Combi_List %>%
  filter(!gbifID %in% Combi_List_TaxonIssue$gbifID) #Removes final cases where TAXON_MATCH_HIGHERRANK not reported but we have removed them nonetheless

if (nrow(Combi_List_TaxonIssue_Add) > 0) {
Combi_List<-Combi_List %>%
  filter(!gbifID %in% Combi_List_TaxonIssue_Add$gbifID)
}
Documentation_Results$No_Taxon_Issue<-length(unique(Combi_List$scientificName))
saveRDS(Documentation_Results$No_Taxon_Issue, "No_Taxon_Issue.rds") #Save

#Now search for cases of TAXON_MATCH_HIGHERRANK. Then, if the verbatimScientificNameAuthorship is present, or if it is a hybrid, 
#paste that exact name into scientificName, replacing the name at a higher taxonomic resolution. Then paste in the 
#author information to it. Then replace ssp. with subspecies. 
#Cases with authorship, and hybrids, are more reliable to determine than cases of general names without authority. 

Combi_List_TaxonIssue <- Combi_List_TaxonIssue %>%
  mutate(scientificName = if_else(
      str_detect(issue, "TAXON_MATCH_HIGHERRANK") & 
        ((!is.na(verbatimScientificNameAuthorship) | verbatimScientificNameAuthorship != "<NULL>") |
           str_detect(verbatimScientificName, "(?<!\\w)([×]|x)(?!\\w|$)")),
      paste0(verbatimScientificName, 
             if_else(!is.na(verbatimScientificNameAuthorship) & verbatimScientificNameAuthorship != "<NULL>",
                     paste0(" ", verbatimScientificNameAuthorship), "")),
      scientificName),
    Edited_Interpreted_Name = if_else(
      str_detect(issue, "TAXON_MATCH_HIGHERRANK") & 
        ((!is.na(verbatimScientificNameAuthorship) & verbatimScientificNameAuthorship != "<NULL>") |
           str_detect(verbatimScientificName, "(?<!\\w)([×]|x)(?!\\w|$)")),
      "YES", 
      "NO"))

Combi_List_TaxonIssue <- Combi_List_TaxonIssue %>%
  mutate(scientificName = str_replace_all(scientificName, "\\bssp\\.\\s*", "subsp. "))

#Separate cases of YES and bind back into Combi_List
Combi_List_TaxonIssue_YES<-Combi_List_TaxonIssue %>%
  filter(Edited_Interpreted_Name =="YES") %>%
  select(-c(Edited_Interpreted_Name)) %>%
  mutate(taxonRank=NA)

#Document Results
Documentation_Results$Three<-nrow(Combi_List_TaxonIssue_YES)
Documentation_Results$Combi_List_TaxonIssue_YES_Names<-length(unique(Combi_List_TaxonIssue_YES$scientificName))
saveRDS(Documentation_Results$Combi_List_TaxonIssue_YES_Names, "Taxon_Issue_One_Author.rds")

}
Combi_List_TaxonIssue_YES <- Combi_List_TaxonIssue_YES %>%
  mutate(taxonRank = case_when(
    str_detect(scientificName, "× ") ~ "Hybrid",
    str_detect(scientificName, "^X ") ~ "Hybrid",
    str_detect(scientificName, " [×xX] ") ~ "Hybrid",
    str_detect(scientificName, " subsp\\. ") ~ "Subspecies",
    str_detect(scientificName, " var\\. ") ~ "Variety",
    str_detect(scientificName, " subvar\\. ") ~ "Subvariety",
    str_detect(scientificName, " morph") ~ "Morph",
    str_detect(scientificName, " f\\. ") ~ "Form",
    str_detect(scientificName, " subf\\. ") ~ "Subform",
    str_detect(scientificName, " agg\\. ") ~ "Aggregate",
    str_detect(scientificName, " aggregate") ~ "Aggregate",
    str_detect(scientificName, " sect\\. ") ~ "Section",
    str_detect(scientificName, " subsect\\. ") ~ "Subsection",
    str_detect(scientificName, " ser\\. ") ~ "Series",
    str_detect(scientificName, " subser\\. ") ~ "Subseries",
    str_detect(scientificName, " tr\\. ") ~ "Tribe",
    str_detect(scientificName, " subtrib\\. ") ~ "Subtribe",
    str_detect(scientificName, " gen\\. ") ~ "Genus",
    str_detect(scientificName, " subg\\. ") ~ "Subgenus",
    str_detect(scientificName, " fam\\. ") ~ "Family",
    str_detect(scientificName, " subfam\\. ") ~ "Subfamily",
    str_detect(scientificName, " ord\\. ") ~ "Order",
    str_detect(scientificName, " subord\\. ") ~ "Suborder",
    str_detect(scientificName, " cl\\. ") ~ "Class",
    str_detect(scientificName, " subcl\\. ") ~ "Subclass",
    TRUE ~ NA_character_  #Keep any remaining entries as NA in taxonRank
  )) %>%
  #Assign taxonRank for cases where it is still NA
  mutate(taxonRank = case_when(
    is.na(taxonRank) & str_count(scientificName, "\\s") >= 1 & 
      str_detect(scientificName, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species", #Detect cases when first word is capitalized and second word is not, label as species, also accounting for accented names 
    is.na(taxonRank) & str_count(scientificName, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ taxonRank
  ))

Combi_List<-rbind(Combi_List, Combi_List_TaxonIssue_YES)

#Filter cases not automatically edited - There are names which have no authority to them, and which therefore cannot be 
#worked with in the same way. 
Combi_List_TaxonIssue_NO<-Combi_List_TaxonIssue %>%
  filter(Edited_Interpreted_Name =="NO") %>%
  select(-c(Edited_Interpreted_Name)) 

Combi_List_TaxonIssue_NO_Names<-Combi_List_TaxonIssue_NO %>%select(verbatimScientificName) %>%distinct()

#Document Results
Documentation_Results$Four<-nrow(Combi_List_TaxonIssue_NO_Names)

save.image(file = "Taxa_List_Save_2.RData") #Save everything 
#load("Taxa_List_Save_2.RData")

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Names without any author affiliation - Repairing GBIF Data - Run species search in KEWR using loop and search_powo command

#Here we processes names without any author affiliation into names with only one author affiliation option (meaning it is reliable to change),
#Names which cannot be reliably altered, and names which are not recognized by POWO and which will ultimately become leftover names. 

#Create three dataframes, one for the species with accepted names, one for species
#where it is a synonym, and one for species names not recognized by KEWR. 
#I make two dataframes because the output differs based on this category. 
{
df_totalA = data.frame() #for accepted species 
df_totalB = data.frame() #for synonyms, which have 2 or more rows because one row is the accepted name for that given synonym
df_totalC = data.frame() #for ones that do not work with the R package KEWR 
Temp2 <- data.frame(accepted=logical(),
                    author=character(),
                    kingdom=character(),
                    family=character(),
                    name=character(),
                    rank=character(),
                    url=character(),
                    fqId=character(),
                    images=numeric(),
                    synonymOf=numeric(),
                    snippet=numeric())

#Progress Bar
total_iterations <- length(unique(Combi_List_TaxonIssue_NO_Names$verbatimScientificName))

#Initialize the progress bar
Leftover_Progress_Bar <- progress_bar$new(
  format = "  Processing [:bar] :percent :current/:total Taxon IDs :elapsed",
  total = total_iterations,  #Total number of iterations
  width = 60,                
  clear = FALSE              
)
}

#Now using the search_powo of the Kewr package, I use each species name in my list and search the KEW database. 
for (sp in unique(Combi_List_TaxonIssue_NO_Names$verbatimScientificName)) {                    #for each sp species in my list search KEW. 
  Temp1 <- search_powo(sp,)                                                                    #Search KEW and store information 
  if (Temp1$total==0) {
    df_totalC <- rbind(df_totalC,Temp1$query)  
  } else {
    Temp2 <- tidy(Temp1)                                                                       #Tidy information 
  }
  x <- c("accepted","author","kingdom","family","name","rank", "url","fqId","images", "synonymOf", "snippet")          #Create category images etc , which some outputs do contain
  Temp2[x[!(x %in% colnames(Temp2))]] = 0                                                    #If a Temp2 query doesn't have this column, then add it in with a value of 0, to allow rbinding later. 
  
  if (nrow(Temp2)>=2) {                                                                      #If there are 2 or more rows, store in df_totalB as this is a synonym species. 
    df_totalB <- rbind(df_totalB,Temp2)  
  } else {
    df_totalA <- rbind(df_totalA,Temp2)                                                      #If there is only one row, store in df_totalA as this is already an accepted name
  }
  
  #Update the progress bar
  Leftover_Progress_Bar$tick()
}
{

  #From Dataframe df_totalA and df_totalB, there are accepted names and synonym names.  
#Since no author is known, we can only work with cases where there is only one name, meaning
#that for a given name, only one name and authority is known. Otherwise, it is unknown which one
#the GBIF specimen belongs to (automatically) 

df_totalA <- df_totalA %>% #Remove duplicates 
  distinct() %>%
  select(name, author) %>%
  group_by(name) %>% 
  filter(n() == 1) %>% 
  ungroup() 

df_totalB <- df_totalB %>% #Remove duplicates 
  distinct() %>%
  select(name, author) %>%
  group_by(name) %>% 
  filter(n() == 1) %>% 
  ungroup() 

df_totalC <- df_totalC %>% #Remove duplicates 
  distinct()

#Combine names with certain authority labels (no other possible options)
No_Author_to_Author_Names<-rbind(df_totalA,df_totalB)

#Now we can match out these names and repair them in Combi_List_TaxonIssue_NO
No_Author_to_Author_Names <- No_Author_to_Author_Names %>%
  mutate(scientificName = if_else(is.na(author), name, paste(name, author)))

#Find cases when Combi_List_TaxonIssue_NO$verbatimScientificName matches to any of the No_Author_to_Author_Names$name and replace in the new corrected name:
Combi_List_TaxonIssue_NO_reAdd <- Combi_List_TaxonIssue_NO %>%
  filter(verbatimScientificName %in% No_Author_to_Author_Names$name) %>%  #Keep only exact matches
  mutate(scientificName = if_else(verbatimScientificName %in% No_Author_to_Author_Names$name,
                                  No_Author_to_Author_Names$scientificName[match(verbatimScientificName, No_Author_to_Author_Names$name)],
                                  scientificName))

#Remove from Combi_List_TaxonIssue_NO
Combi_List_TaxonIssue_NO<-Combi_List_TaxonIssue_NO %>% filter(!gbifID %in% Combi_List_TaxonIssue_NO_reAdd$gbifID)

#Document Results
Documentation_Results$Five<-nrow(Combi_List_TaxonIssue_NO_reAdd)

Combi_List<-rbind(Combi_List, Combi_List_TaxonIssue_NO_reAdd)

colnames(df_totalC)[1] <- "Unrecognized_No_Author_Names"

#Document Results
Documentation_Results$Six<-nrow(df_totalC)

save.image(file = "Taxa_List_Save_3.RData") #Save everything 
#load("Taxa_List_Save_3.RData")

}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Names without any author affiliation - Dealing with Unrecognized names 

#df_totalC contains names which were not picked up by KEW POWO, meaning that regardless of their authority affiliation, they will be unrecognized. 
#This means that these names must be added into the rest of the queries, so they can be investigated by the taxonomic expert. However they need to be distinguished. 

#We will edit the GBIF Data to note that there is no given author, and in a way that these specimens will be flagged for later viewing by the taxonomic expert. 

{
#Find cases when Combi_List_TaxonIssue_NO$verbatimScientificName matches to any of the df_totalC$Unrecognized_No_Author_Names and replace in the new corrected name:
Combi_List_TaxonIssue_NO_unrecognized <- Combi_List_TaxonIssue_NO %>%
  filter(verbatimScientificName %in% df_totalC$Unrecognized_No_Author_Names) %>%
  mutate(scientificName=verbatimScientificName)

#These cases can now be added back into Combi_List and can be removed from Combi_List_TaxonIssue_NO
Combi_List_TaxonIssue_NO<-Combi_List_TaxonIssue_NO %>% filter(!gbifID %in% Combi_List_TaxonIssue_NO_unrecognized$gbifID)
Combi_List<-rbind(Combi_List, Combi_List_TaxonIssue_NO_unrecognized)

#Combi_List_TaxonIssue_NO can be further investigated later, when the final species list is generated. 

#Document Results
Documentation_Results$Seven<-nrow(Combi_List_TaxonIssue_NO)

#Now processing the Combi_List:

#Store for later use:
Combi_List_Storage<-Combi_List
Combi_List_Storage_1<-Combi_List

#Extract species names using the scientificName Column 
Combi_List <- Combi_List %>%
  filter(scientificName != "NA") %>%
  transmute(species = scientificName) %>%
  distinct()  

Combi_List <- unique(Combi_List) #Remove exact duplicates

#Document Results
Documentation_Results$Eight<-nrow(Combi_List)

save.image(file = "Taxa_List_Save_4.RData") #Save everything 
#load("Taxa_List_Save_4.RData")
}

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Dealing with Genera only in the input list (no species names just genera listed)

#When a species name is listed as Genus sp. or Genus L., such as Betula sp. or Betula L., 
#it is not picked up by KEWR. Moreover, when a species is labelled just at the Genus level, such as
#Betula, it is picked up by KEWR, but along with all of its species within the Genus, which is not wanted 
#because it means it will add in species not in Greenland. 

#To run the KEWS search effectively, these should be removed, but then added in later since they represent
#Genus level knowledge where we may not have it for the species itself. 

{
#For names with one word only, aka Genus or Family (Higher Taxon Names)
Genus_Names1 <- Combi_List %>%
  filter(str_count(species, '\\w+') <=1)  
  
#For names with one word only, aka Genus L. 
Genus_Names2 <- Combi_List %>%   
    filter(str_count(species, '\\w+') <=2) %>%
    filter(grepl(pattern = "\\b(L.|sp.| indet.)\\b", species)) #Also removes cases where the species epithet is unknown, and therefore it is only known at a Genus level   

#Now remove from main list for use in KEWR
Combi_List<- Combi_List %>%
  filter(!Combi_List$species %in% Genus_Names1$species)

#Now remove from main list for use in KEWR
Combi_List<- Combi_List %>%
  filter(!Combi_List$species %in% Genus_Names2$species)

#Store the Genus names together for later use - I can run these separately later and extract only Genus information 
Genus_Names<- rbind(Genus_Names1, Genus_Names2 ) #Make sure to check this and manually clean if needed. 
Genus_Names <- Genus_Names %>%  
  transmute(name = species) 

save.image(file = "Taxa_List_Save_5.RData") #Save everything 
#load("Taxa_List_Save_5.RData")
}

#N.B. There will still be some Genera which were not captured by this, as they have an authority name after them,
#But these will either be picked up as Genera later by code, or flagged for manual checking. 

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Editing format of species list to make compatible with KEWR

#Now that I have the input list, I can make some edits to the species within in order to 
#accommodate the searching abilities of KEWR. 
#Namely, I change the word ssp to subsp., as KEWR can recognize subsp. but not the others. 
#Moreover, KEWR does not always recognize the authors names (eg Potentilla crantzii var. hirta Lge. vs without the Lge.), and so I can use the FLORA R package to remove them. 
#In both cases, I want to keep the original names because these are how they are written in our input dataset, which we will eventually search again. 
#So I make sure to store them with their new names so when I have the Master List, I can add them in as a column for each accepted species name to make sure
#We capture these names when running searches between the input data and the Master List. 

Edited_Combi_List_Temp = data.frame(species=NA, original_input_name=NA)  #Create a dataframe for the edited species list 
Edited_Combi_List = data.frame(species=NA, original_input_name=NA)

for (sp in Combi_List$species) {                                         #For each species in my input list -aka Combi_List
  Edited_Combi_List_Temp$species <-(sp)
  Edited_Combi_List_Temp$species <- gsub ("ssp.","subsp.", Edited_Combi_List_Temp$species)   #Change ssp. to subsp - subsp. is recognized by KEWR and also ssp./sp. is removed by FLORA's remove.authors function (removing subspecies information with it) - so this order prevents that 
  for (h in Edited_Combi_List_Temp$species) {
    Edited_Combi_List_Temp$species <-remove.authors(h)                   #Remove any author tags for this species
  }
  Edited_Combi_List_Temp$original_input_name<-(sp)                       #Store original species name as found in data input file to later add as a "synonym" to the Master List 
  Edited_Combi_List <- rbind(Edited_Combi_List,Edited_Combi_List_Temp)  
}

{
#Note that the R package doesn't work well with hybrid species (eg "Festuca rubra x Festuca vivipara ssp. hirsuta" becomes "Festuca rubra subsp. hirsuta")
#And so I just use the original name now in the dataset for these cases: 
#First remove rows with NAs in them
Edited_Combi_List <- drop_na(Edited_Combi_List)
Edited_Combi_List$species[str_detect(Edited_Combi_List$original_input_name, " x ")] <- Edited_Combi_List$original_input_name[str_detect(Edited_Combi_List$original_input_name, " x ")] #x
Edited_Combi_List$species[str_detect(Edited_Combi_List$original_input_name, " × ")] <- Edited_Combi_List$original_input_name[str_detect(Edited_Combi_List$original_input_name, " × ")] #multiplication sign 
Edited_Combi_List$species[str_detect(Edited_Combi_List$original_input_name, " ×")] <- Edited_Combi_List$original_input_name[str_detect(Edited_Combi_List$original_input_name, " ×")] 
Edited_Combi_List$species[str_detect(Edited_Combi_List$original_input_name, " X ")] <- Edited_Combi_List$original_input_name[str_detect(Edited_Combi_List$original_input_name, " X ")] 
  
#Standardize how the hybrid × is written: 
#Function to replace " × " or its variations with " × "
replace_x <- function(x) {
    gsub("\\s*×\\s*", " × ", x)
  }
  
#Apply the function to all columns in the dataframe
Edited_Combi_List <- Edited_Combi_List %>%
  mutate(species = replace_x(species),
         original_input_name = replace_x(original_input_name)) #Standardize the hybrid spelling in the species column, but also the original_input_name, as otherwise the matching from POWO search to the original_input_name will not work and hybrids will be artificially flagged 

Edited_Combi_List$species <- trimws(Edited_Combi_List$species)

#Now further clean the hybrids in Edited_Combi_List$species
Edited_Combi_List <- Edited_Combi_List %>%
  mutate(species = if_else(str_detect(species, " × "), 
    str_replace(
      species,
      "( × [a-z]+)(\\s+[A-Z(].*)$", 
      "\\1" 
    ),
    species 
  ))

#Further clean hybrid cases where there is a hybrid of an infraspecific. 
Edited_Combi_List <- Edited_Combi_List %>%
  mutate(species = if_else(
    str_detect(species, " × [a-z]+\\s+(subsp\\.|var\\.|f\\.)\\s+[a-z]+"), 
    str_replace(
      species,
      "( × [a-z]+\\s+(subsp\\.|var\\.|f\\.)\\s+[a-z]+)\\s+.*$", 
      "\\1" 
    ),
    species 
  ))

Edited_Combi_List$species <- trimws(Edited_Combi_List$species)
}

{

#Fix issues with hypens " - " which causes issues with names  
for (i in 1:nrow(Edited_Combi_List)) {
    species_without_hyphen <- gsub("-", " ", Edited_Combi_List$species[i])
    original_input_name_without_hyphen <- gsub("-", " ", Edited_Combi_List$original_input_name[i])
    species_words <- str_split(species_without_hyphen, "\\s+")[[1]]
    original_input_name_words <- str_split(original_input_name_without_hyphen, "\\s+")[[1]]
    if (length(species_words) == 3 && length(original_input_name_words) == 3 && grepl("\\s-\\s", Edited_Combi_List$original_input_name[i])) {
      Edited_Combi_List$species[i] <- Edited_Combi_List$original_input_name[i]}}  
  
  
Edited_Combi_List$species <- str_replace_all(Edited_Combi_List$species, " - ", "-")
  
#Clean cases where the only 3 words and last 2 are not capital letters, like "Carex capitata arctogena" (also excluding periods)
Edited_Combi_List <- Edited_Combi_List %>%
  mutate(
    species = if_else(
      str_detect(original_input_name, "^[A-Z][a-z]+\\s[a-z]+\\s[a-z]+$") & 
        !str_detect(original_input_name, "\\."), #
      original_input_name, 
      species 
    )
  )


#Clean cases where third word is capital letter (remaining author in species name)
Edited_Combi_List <- Edited_Combi_List %>%
    mutate(species = if_else(
      !str_detect(species, "\\s[x×]\\s") & 
        str_detect(sapply(str_extract_all(species, "\\S+"), function(x) if (length(x) >= 3) x[3] else ""), "^[A-Z]"),
      str_replace(species, "^((\\S+\\s+){2})\\S+", "\\1"),
      species))  
  
Edited_Combi_List <- Edited_Combi_List %>%
    mutate_all(~ str_trim(., side = "both"))  
  
#Remove remaining author names in an automated way:
Edited_Combi_List <- Edited_Combi_List %>%
    mutate(species = case_when(
      str_detect(species, " f\\.$") ~ species,
      str_detect(species, " × | x |× | x| X | X") ~ species,
      str_detect(species, "\\s(subsp.|var.|f.|subvar.|morph.|subf.|agg.|sect.|subsect.|ser.|subser.|tr.|subtr.|gen.|subgen.)\\s") ~ {
        rank_position <- str_locate(species, "\\s(subsp.|var.|f.|subvar.|morph.|subf.|agg.|sect.|subsect.|ser.|subser.|tr.|subtr.|gen.|subgen.)\\s")
        if (!is.na(rank_position[1])) {
          str_trim(str_sub(species, 1, rank_position[1] + nchar(strsplit(species, " ")[[1]][3]) * 2))
        } else {species}},
      str_count(species, "\\s+") > 1 & !str_detect(species, "\\s(subsp.|var.|f.|subvar.|morph.|subf.|agg.|sect.|subsect.|ser.|subser.|tr.|subtr.|gen.|subgen.)\\s") ~ 
        str_trim(str_sub(species, 1, str_locate(species, "\\s+")[,1] - 1)),
      TRUE ~ species))  
  
Edited_Combi_List <- Edited_Combi_List %>%
  mutate(species = case_when(
    str_count(species, "\\s+") == 0 &  
      str_count(original_input_name, "\\s+") == 1 & 
      str_detect(str_extract(original_input_name, "\\S+$"), "[*?#!$%^&]$") ~ 
      original_input_name,  
    TRUE ~ species 
  ))

#Fix issues when erroneous names reduced to Genus only - like Luzula frigida contracta
for (i in 1:nrow(Edited_Combi_List)) {
  original_input_name_words <- str_split(Edited_Combi_List$original_input_name[i], "\\s+")[[1]]
  species_words <- str_split(Edited_Combi_List$species[i], "\\s+")[[1]]
  if (length(original_input_name_words) == 3) {
    first_word_capitalized <- str_detect(original_input_name_words[1], "^[A-Z]")
    second_word_lowercase <- str_detect(original_input_name_words[2], "^[a-z]")
    third_word_lowercase <- str_detect(original_input_name_words[3], "^[a-z]")
    if (length(species_words) == 1 && first_word_capitalized && second_word_lowercase && third_word_lowercase) {
      Edited_Combi_List$species[i] <- Edited_Combi_List$original_input_name[i]}}}

#Fix cases of and/or or "og" in Danish
for (i in 1:nrow(Edited_Combi_List)) {
  if (str_detect(Edited_Combi_List$original_input_name[i], "and/or| og ")) {
    Edited_Combi_List$species[i] <- Edited_Combi_List$original_input_name[i]}}

#Now the Edited_Combi_List$species has a cleaner list of species for input in searching, while Edited_Combi_List$original_input_name has the corresponding name used for that new species in the original input file.    

Combi_List_Cleaned<- Edited_Combi_List
colnames(Combi_List_Cleaned)[1] <- "species"

#Remove more genus names which had authors attached to it: 
#For names with one word only, aka Genus
Genus_Names3 <- Combi_List_Cleaned %>%
  filter(str_count(species, '\\w+') <=1)  

#Now remove from main list for use in KEWR
Combi_List_Cleaned<- Combi_List_Cleaned %>%
  filter(!Combi_List_Cleaned$species %in% Genus_Names3$species)
Genus_Names3 <- Genus_Names3 %>%  
  transmute(name = species) 
#Store the Genus names together for later use 
Genus_Names<- rbind(Genus_Names, Genus_Names3) #Make sure to check this and manually clean if needed. 
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Removing "incertae sedis" - specimens without labels 
Combi_List_Cleaned<- Combi_List_Cleaned[!grepl("incertae sedis", Combi_List_Cleaned$original_input_name),]

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Correct cases where the species name after the "f." for form has been removed, as it was thought to be an authority name: 

#Create a new column to store the extracted form names
Combi_List_Cleaned$form_name <- NA

#Iterate through the rows and extract the form names
for (i in 1:nrow(Combi_List_Cleaned)) {
  species_name <- Combi_List_Cleaned$species[i]
  if (grepl("f\\.$", species_name) && !grepl("\\. f\\.$", Combi_List_Cleaned$original_input_name[i])) {
    form_name <- sub("^.*f\\.\\s*(\\S+).*", "\\1", Combi_List_Cleaned$original_input_name[i])
    Combi_List_Cleaned$form_name[i] <- form_name
    Combi_List_Cleaned$species[i] <- paste0(species_name, " ", form_name)}
  if (grepl("f\\.$", Combi_List_Cleaned$original_input_name[i])) {
    Combi_List_Cleaned$form_name[i] <- NA}
  if (grepl("\\. f\\.$", Combi_List_Cleaned$original_input_name[i]) &&
      grepl(" f\\.$", Combi_List_Cleaned$species[i])) {
    Combi_List_Cleaned$species[i] <- sub(" f\\.$", "", Combi_List_Cleaned$species[i])}}

Combi_List_Cleaned <- Combi_List_Cleaned %>%
  select(species, original_input_name)

save.image(file = "Taxa_List_Save_6.RData") #Save everything 
#load("Taxa_List_Save_6.RData")
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Run initial species search in KEWR using loop and search_powo command
{
Combi_List_Cleaned <- Combi_List_Cleaned %>%
    mutate(species = str_trim(species))  
  
Combi_List_Cleaned <- Combi_List_Cleaned %>%
    mutate(original_input_name = str_trim(original_input_name))   

Combi_List_Cleaned<-Combi_List_Cleaned%>%distinct()

#Run search including progress bar, as the search time can be long. 
#Calculate the total number of iterations based on the length of the species list being used as input for searching 
total_iterations <- length(unique(Combi_List_Cleaned$species))

#Species search progress bar object
KEWR_Progress_Bar <- progress_bar$new(
  format = "  Processing [:bar] :percent :current/:total Taxa Names Searched in KEWR :elapsed",
  total = total_iterations,  #Total number of iterations
  width = 60,                #Width of the progress bar
  clear = FALSE              #Keep the bar after completion
)

#Create three dataframes, one for the species with accepted names, one for species
#where it is a synonym, and one for species names not recognized by KEWR. 
#I make two DFs because the output differs based on this category. 
df_total1 = data.frame() #for accepted species 
df_total2 = data.frame() #for synonyms, which have 2 or more rows because one row is the accepted name for that given synonym
df_total3 = data.frame() #for ones that do not work with the R package KEWR - will include most hybrids, some varieties, and some orthographic variants 
Temp2 <- data.frame(accepted=logical(),
                    author=character(),
                    kingdom=character(),
                    family=character(),
                    name=character(),
                    rank=character(),
                    url=character(),
                    fqId=character(),
                    images=numeric(),
                    synonymOf=numeric(),
                    snippet=numeric())
  }                    
#Now using the search_powo of the Kewr package, I use each species name in my list and search the KEW database. 
for (sp in unique(Combi_List_Cleaned$species)) {                                                     #for each sp unique species in my list search KEW. 
  Temp1 <- search_powo(sp,)                                                                  #Search KEW and store information 
  if (Temp1$total==0) {
    df_total3 <- rbind(df_total3,Temp1$query)  
  } else {
    Temp2 <- tidy(Temp1)                                                                     #Tidy information 
  }
  x <- c("accepted","author","kingdom","family","name","rank", "url","fqId","images", "synonymOf", "snippet")          #Create category images etc , which some outputs do contain
  Temp2[x[!(x %in% colnames(Temp2))]] = 0                                                    #If a Temp2 query doesn't have this column, then add it in with a value of 0, to allow rbinding later. 
  
  if (nrow(Temp2)>=2) {                                                                      #If there are 2 or more rows, store in df_total2 as this is a synonym species. 
    df_total2 <- rbind(df_total2,Temp2)  
  } else {
    df_total1 <- rbind(df_total1,Temp2)                                                      #If there is only one row, store in df_total1 as this is already an accepted name
  }
  KEWR_Progress_Bar$tick() #Progress Bar
}
{
df_total1 <- df_total1 %>% #Remove duplicates 
  distinct()

df_total2 <- df_total2 %>% #Remove duplicates 
  distinct()

df_total3 <- df_total3 %>% #Remove duplicates 
  distinct()
}

save.image(file = "Taxa_List_Save_7.RData") #Save everything 
#load("Taxa_List_Save_7.RData")

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Additional Genera and Family cleaning 

#Note that the dataframes sometimes still contain more ranks than simply species, even after the previous filtering I did. For now I will remove and store anything that 
#is only a Genus rank or higher. 
Genus_Names4 <- df_total1 %>%
  filter(rank=="Genus")
Genus_Names5 <- df_total2 %>%
  filter(rank=="Genus")

suppressPackageStartupMessages(library(plyr))
Genus_Names <-rbind.fill(Genus_Names4,Genus_Names5,Genus_Names)
detach(package:plyr)
library("dplyr")

Genus_Names <- Genus_Names %>%
  distinct(name)

#Family and Higher 
{
  Additional_Ranks1 <- df_total1 %>%
    filter(rank %in% c("Family", "Order", "Class", "Phylum", "Kingdom", "Domain", "FAMILY", "ORDER", "CLASS", "PHYLUM", "KINGDOM", "DOMAIN"))
  Additional_Ranks2 <- df_total2 %>%
    filter(rank %in% c("Family", "Order", "Class", "Phylum", "Kingdom", "Domain", "FAMILY", "ORDER", "CLASS", "PHYLUM", "KINGDOM", "DOMAIN"))

Additional_Ranks<- rbind(Additional_Ranks1, Additional_Ranks2)
Additional_Ranks <- Additional_Ranks %>%
  distinct(name)

df_total1<- df_total1 %>%
  filter(!df_total1$name %in% Genus_Names$name & !df_total1$name %in% Additional_Ranks$name)
  
df_total2<- df_total2 %>%
  filter(!df_total2$name %in% Genus_Names$name & !df_total2$name %in% Additional_Ranks$name)
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Preparing for Master List - Organizing Accepted Names and Synonym Files

#df_total1 now has a list of species from our list which are already using the accepted name. 
#df_total2 now has a list of species from our list which are not yet using the accepted name, but which now contains 
#both the synonym from the list and its corresponding accepted name. 
#df_total3 now has a list of species names not recognized by the KEWR package

#df_total2 now has a mixture of the synonyms, where the accepted category is FALSE,
#and corresponding TRUE accepted names. However due to the KEWR package, some extra names
#may have also entered, and so I will remove them now and add them to df_total1, and as a precaution,
#I will also extract the accepted name for each synonym and also add it, to make sure I get all possible names. 
{
True_Names<- df_total2 %>%
  filter(accepted == "TRUE")

df_total1 <- rbind(df_total1,True_Names)

#Keep synonym names only 
df_total2<- df_total2 %>%
  filter(accepted == "FALSE")
#Un-nest the df to extract the synonym name and corresponding actual name of each species 
df_total2<-tidyr::unnest(df_total2, cols=synonymOf, names_sep="_")

#Within the columns there is a code, for example 124840-2. 
#This code is the Accepted Taxon ID. 

#Extract the Accepted Taxon ID from both dataframes. For df_total1 this will be of the accepted species, aka the original species entered into the system
#For df_total2, this will be the new accepted species name accepted Taxon ID, not the synonym originally entered. 

df_total1.1 <- df_total1 %>%
  transmute(Accepted_Taxon_ID = fqId)  

df_total2.1 <- df_total2 %>%
  transmute(Accepted_Taxon_ID = synonymOf_fqId)  
  
df_total<- data.frame()
df_total <- rbind(df_total1.1, df_total2.1) #List of all species present in the given species list, but with the Taxon ID for their Accepted Species Names, not synonyms 

df_total$Accepted_Taxon_ID <- gsub ("urn:lsid:ipni.org:names:","", df_total$Accepted_Taxon_ID) #remove the jargon

df_total <- df_total %>%
  distinct() #Remove duplicate numbers 
}
#Now search for data on all accepted species, where we also get the list of synonyms etc. 

#Some species have a Basionym and some do not, so need to add this column, even if blank, to run loop properly. 
#Define custom function to add columns to data frame if they do not exist - depending on the species, POWO has extra metadata for it compared to others, and this can break the loop when binding multiple searches. 
add_cols <- function(Temp2, cols) {
  add <- cols[!cols %in% names(Temp2)]
  if(length(add) !=0 ) Temp2[add] <- NA
  return(Temp2)
}

#Add in Progress Bar
total_iterations <- length(unique(df_total$Accepted_Taxon_ID))

#Initialize the progress bar
KEWR_Progress_Bar_Acc <- progress_bar$new(
  format = "  Processing [:bar] :percent :current/:total Accepted Name Query Progress :elapsed",
  total = total_iterations,  #Total number of iterations
  width = 60,                
  clear = FALSE              
)

List1 = data.frame()
for (ac in unique(df_total$Accepted_Taxon_ID)) {   #For each ac accepted species Taxon ID in my list
  Temp1 <- lookup_powo(ac)                         #Search KEW and store information for that accepted Taxon ID
  Temp2 <- tidy(Temp1)                             #Tidy information 
  Temp2 <- add_cols(Temp2, c("modified","bibliographicCitation","genus","taxonomicStatus","kingdom","phylum","clazz","subclass","order","family","nomenclaturalCode","source","namePublishedInYear","taxonRemarks","nomenclaturalStatus",
                             "lifeform","climate","hybrid","synonym","plantae","fungi","locations","fqId","name","authors","species","rank","reference","classification","basionymOf","synonyms","response","queryId", "basionym", "childNameUsages",
                             "nomenclaturalRemarks", "infraspecies", "hybridFormula", "paftolId")) 
  List1 <- rbind(List1,Temp2)  
  
  #Update the progress bar
  KEWR_Progress_Bar_Acc$tick()
  }

List1 <- List1 %>%
  mutate(name = str_trim(name))

save.image(file = "Taxa_List_Save_8.RData") #Save everything 
#load("Taxa_List_Save_8.RData")

#Problem Solving - If this ac loop fails, check the Temp1 dataframe of the ac that failed and see what extra columns it has, then add in here.
#Quickly check this by using "all_options <- names(Temp1)"
#"all_options" - this gives a list of any new columns found for that species name which crashed the code. Then just add that column in. 

#Now we have a list that contains all the accepted names of each species in our files, and all the synonyms and basionyms - Note there are still many duplicates, but we leave here for now 
#But the synonyms and basionyms are stored as lists within the dataframe. I want it all in a clean crisp dataframe for easy use later. 

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Preparing for Master List - Extracting Synonyms 
{
#Synonyms are packed in a list format, so I will do a weird workaround to get to them. 
Synonym_Names2<- do.call(rbind.data.frame, List1$synonyms) #Gives synonyms in order of extracting from List1, but without corresponding names of accepted species 

#I decided to include the author names of the synonyms. 
#For example, Carex oederi Willk. is a synonym of Carex lepidocarpa subsp. nevadensis while
#Carex oederi Retz is its own accepted species. 

Synonym_Names2<-Synonym_Names2%>%mutate(Synonym_with_Author = paste(name, author, sep = " "))      #Now I have the Synonyms with their authority names on them. 
List1<-List1%>%mutate(Accepted_Name_with_Author = paste(name, authors, sep = " "))                 #Now I have the Accepted Name with their authority names on them.
List1$Accepted_Name_with_Author<-gsub("NA","", List1$Accepted_Name_with_Author)                    #Remove NAs that get appended to Accepted_Name_with_Author if the authors category is NA
List1$Accepted_Name_with_Author <- trimws(List1$Accepted_Name_with_Author)

List1 <- List1 %>%
  mutate(Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author))

Synonym_Names<- setDT(List1)[, .(Synonym = c(name, unlist(synonyms,recursive = TRUE))), by = Accepted_Name_with_Author] #This unnests the list (including matching accepted name) but does it for all list columns, and I do not want this. 
Synonym_Names<-Synonym_Names %>%                                                                   #Now I keep the urn:lsid:ipni.org:names codes as these match the Accepted Name to the Synonym 
  filter(grepl("urn:lsid:ipni.org:names",Synonym_Names$Synonym)) %>% #If this doesn't work when re-running, turn Synonym_Names$Synonym to only "Synonym"
  rename("fqId" = "Synonym")

Synonym_Names<-left_join(Synonym_Names, Synonym_Names2, by="fqId")                                 #Now I join the Synonym to the Accepted Name 
Synonym_Names<-Synonym_Names%>%
  transmute(Accepted_Name_with_Author=Accepted_Name_with_Author,
            Synonym_Names_Authors = Synonym_with_Author)

#Repeat for Basionyms - likely already in the synonyms for each species, but will do again for due diligence 
Basionyms<- do.call(rbind.data.frame, List1$basionymOf) #Gives Basionyms in order of extracting from List1, but without corresponding names of accepted species 
Basionyms2<- do.call(rbind.data.frame, List1$basionym)  #second category of Basionyms
Basionyms <- rbind(Basionyms, Basionyms2)
Basionyms <- Basionyms %>%
  drop_na(name) #Remove NAs from columns 
Basionym_Names1<- setDT(List1)[, .(Basionym = c(name, unlist(basionymOf,recursive = TRUE))), by = Accepted_Name_with_Author]
Basionym_Names2<- setDT(List1)[, .(Basionym = c(name, unlist(basionym,recursive = TRUE))), by = Accepted_Name_with_Author]
Basionym_Names <- rbind(Basionym_Names1, Basionym_Names2)
Basionym_Names <- Basionym_Names %>%
  drop_na(Basionym) %>% #Remove NAs from columns 
  filter(grepl("urn:lsid:ipni.org:names", Basionym)) %>%
  rename("fqId" = "Basionym")
Basionym_Names<-left_join(Basionym_Names, Basionyms, by="fqId")                                    #Now I join the Synonym to the Accepted Name 
Basionym_Names<-Basionym_Names%>%mutate(Synonym_with_Author = paste(name, author, sep = " "))      #Now I have the Synonyms with their authority names on them. 
Basionym_Names<-Basionym_Names%>%
  transmute(Accepted_Name_with_Author=Accepted_Name_with_Author,
            Synonym_Names_Authors = Synonym_with_Author)
#Now add into Synonym list:
Synonym_Names <- rbind(Synonym_Names,Basionym_Names) #List of all synonyms and Basionyms from KEW dataset per accepted species 
Synonym_Names <- Synonym_Names %>%
  distinct() #Remove duplicates

#Remove cases where the Accepted Name and the Synonym in a given row are the same, as this is redundant 
Synonym_Names <- Synonym_Names[Synonym_Names$Accepted_Name_with_Author != Synonym_Names$Synonym_Names_Authors,]

Final_Synonym_Names<- dcast(setDT(Synonym_Names), Accepted_Name_with_Author ~ rowid(Accepted_Name_with_Author), value.var = "Synonym_Names_Authors")
colnames(Final_Synonym_Names)[1] <- "Accepted_Name_with_Author"

Final_Synonym_Names <- Final_Synonym_Names %>%
  mutate(Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author))

Final_Synonym_Names<-Final_Synonym_Names %>% rename_at(vars(2:ncol(Final_Synonym_Names)), ~paste0('Synonym_',.)) #Rename Synonym Columns, which start at Column 30
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Preparing for Master List - Creating Master List
{
Master_List<- List1 #Start making Master List
colnames(Master_List)[which(names(Master_List) == "name")] <- "Accepted Name"
colnames(Master_List)[which(names(Master_List) == "fqId")] <- "KEW ID"

Master_List <- left_join(Master_List, Final_Synonym_Names, by = "Accepted_Name_with_Author")  #I use Left Join to ensure any Accepted Names that do not have synonyms are kept in the list. 

#I will reorder the columns to make it easier to see, but keeping the synonyms at the end since there are so many
Master_List <- Master_List %>%
  select("Accepted Name", "Accepted_Name_with_Author", "synonym", "KEW ID", "taxonomicStatus", "kingdom", "phylum", "family", "genus", "species", "rank", "plantae", "fungi", "hybrid", "namePublishedInYear", "authors", "reference", "nomenclaturalCode", "nomenclaturalStatus", "lifeform", "climate", "taxonRemarks", "locations", everything())

Master_List$Accepted_Name_with_Author<-gsub("NA","", Master_List$Accepted_Name_with_Author) #Remove NAs that get appended to Accepted_Name_with_Author if the authors category is NA
Master_List$Accepted_Name_with_Author <- trimws(Master_List$Accepted_Name_with_Author)

}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Handling Unplaced Names

#Some species names are unplaced taxonomically. 
#In some cases, such as with Taraxacum croceum subsp. brachyceras,
#There is no placed option. 
#In other cases such as Potentilla rubricaulis, 
#There is one Accepted option for Taxonomic status, and 
#several unaccepted. Here I decide to remove the unplaced ones when
#there is a placed one. 
{
Unplaced <- Master_List %>%
  filter(taxonomicStatus == "Unplaced") #Filter out unplaced 

Master_List<- Master_List %>%
  filter(!Master_List$`KEW ID` %in% Unplaced$`KEW ID`) #Remove all unplaced 

#Now if the Accepted Name of the Unplaced is still found in the Master List
#Then I can keep it as removed. If it is not found, then I add it back in.  

Unplaced_To_ReAdd <- Unplaced %>%
  filter(!`Accepted Name` %in% Master_List$`Accepted Name`)
#Now I have a list of unplaced names, but in some cases there are more than one of the same Accepted Name. 
#I don't want to lose this information. 
#So I combine it with commas within the relevant columns 

#Now Add these back 
Master_List<-rbind(Master_List, Unplaced_To_ReAdd)
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Connecting spelling of input list to the Master list - enables future cross checking. 

#Now I want to add in the original naming for the species as is found in the input list from GBIF, to include exactly how it is spelled from the source data. 
#This explicitly enables cross-referencing back to the source data. 
#I have this information stored in Combi_List_Cleaned$original_input_name, where each original input name matches to a Combi_List_Cleaned$species name used in our searching. 
#Note that some species names used in the search have multiple original names from the input list of GBIF. For example, 
#Agrostis borealis is also known as Agrostis borealis Hartm. and Agrostis borealis Trin. in our input list. 

#The species name without the author information cannot be matched, for this will result in multiple matches due to the author name being the only distinguishing factor among some species names. 
#Moreover, we cannot assume that all different author spellings belong to one species. For example, Arnica alpina Willd. ex Steud. and Arnica alpina (L.) are synonyms of different species. 
{
Input_Names<-as.data.frame(Combi_List_Cleaned$original_input_name)
names(Input_Names)[1] <- "original_input_name"
Input_Names$original_input_name_backup<-Input_Names$original_input_name

Master_List$Accepted_Name_with_Author <- str_trim(Master_List$Accepted_Name_with_Author)

#Prepare to join to the Master List - Pivot it long by Accepted Name and with synonyms
Master_List_Mini <- Master_List %>%
  select(Accepted_Name_with_Author, starts_with("Synonym_")) %>%
  pivot_longer(
    cols = starts_with("Synonym_"),  
    names_to = "Synonym_Type",      
    values_to = "Synonym") %>%
  filter(!is.na(Synonym)) %>%
  select(-Synonym_Type)

Missing_Accepted_Names <- Master_List %>%
  select(Accepted_Name_with_Author) %>%
  filter(!Accepted_Name_with_Author %in% Master_List_Mini$Accepted_Name_with_Author) %>%
  mutate(Synonym = Accepted_Name_with_Author)

#Combine both datasets
if (nrow(Missing_Accepted_Names) > 0) {
  Master_List_Mini <- bind_rows(Master_List_Mini, Missing_Accepted_Names)  
}  

Master_List_Mini <- Master_List_Mini %>%
  bind_rows(Master_List_Mini %>% 
              mutate(Synonym = Accepted_Name_with_Author) %>% 
              select(Accepted_Name_with_Author, Synonym)) %>%
  distinct() #Add Accepted Name into Synonym row for matching 

Master_List_Mini <- Master_List_Mini %>%
  mutate(Synonym = str_trim(Synonym),
    Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author))

Input_Names <- Input_Names %>%
  mutate(original_input_name_backup = str_trim(original_input_name_backup))

Master_List_Mini <- Master_List_Mini %>%
  left_join(Input_Names, by = c("Synonym" = "original_input_name_backup")) %>%
  filter(!is.na(original_input_name)) %>%
  select(Accepted_Name_with_Author, original_input_name)

#Note that in some cases, the input name can attach to more than one Accepted Name, usually due to it being an erraneous name. For example, "Carex sc" and "Carex marit" can do this. 
#Other times it is cases of even an author name not being able to distinguish a species, like Cerastium vulgatum L.
#Find cases where it is a leftover name and not matching properly, but keep in the other cases like Cerastium vulgatum L. because these are genuine ones 

#Find duplicates in original_input_name
duplicates <- Master_List_Mini %>%
  group_by(original_input_name) %>%
  filter(n() > 1) %>%
  pull(original_input_name)

#Check if any duplicates exist anywhere in Master_List (legitimate names)
matching_names <- Master_List %>%
  filter(rowSums(sapply(., function(x) x %in% duplicates)) > 0) %>%
  unlist() %>%
  unique() 

Master_List_Mini <- Master_List_Mini %>%
  filter(!(original_input_name %in% duplicates & !(original_input_name %in% matching_names)))
}
{
Names_Input<- names(Master_List_Mini)[-1] #Take all column names except first one, for use in Dcast 
Master_List_Mini<- dcast(setDT(Master_List_Mini), Accepted_Name_with_Author ~ rowid(Accepted_Name_with_Author), value.var = Names_Input)
Master_List_Mini<-Master_List_Mini %>% drop_na(Accepted_Name_with_Author)
#Now shift all names to the left to fill in NA cells (tidy it up)
Master_List_Mini <- as.data.frame(t(apply(Master_List_Mini, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
colnames(Master_List_Mini)[1] <- "Accepted_Name_with_Author"
colnames(Master_List_Mini) <- sub("V", "", colnames(Master_List_Mini))
Master_List_Mini<-Master_List_Mini %>% 
  rename_at(vars(2:ncol(Master_List_Mini)), ~paste0('Additional_Input_Name',.)) %>% #Rename Input Name Columns 
  select(where(~!all(is.na(.x)))) #Remove for example any columns where all rows are NA.
#Master_List_Mini now has the accepted name of each species, and all the possible spellings of both that accepted name and its respective synonyms of our input data 
#Now Left join this to the Master_List. 
Master_List<- left_join(Master_List, Master_List_Mini, by= c("Accepted_Name_with_Author")) 
}
#Now we have a Master List which has the Accepted Names, the synonyms, and the other spellings for the accepted names and synonyms all sorted together,
#along with lots of other data. 

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Removal of Extra Species

#However, there will be many species in this list which do not belong. 
#For example, Agrostis pourretii Willd. comes from searching the Powo for Agrostis canina, as 
#The species input list had Agrostis canina L. but there is also an Agrostis canina Ucria, so
#when the authority name was removed, this was picked up too. 

#Now I filter the Master List to only contain species who have an accepted name or synonym in the input list. 
#I can check it against the Combi_List_Cleaned$original_input_name file:
{
#Standardize the Combi_List_Cleaned$original_input_name for proper matching 
input_names <- tolower(unique(Combi_List_Cleaned$original_input_name))
  
#Confirmed Master List - #Now confirm all rows have a matching Accepted Name or Synonym in the input list 
Confirmed_Master_List <- Master_List %>%
    filter(if_any(everything(), ~ tolower(.x) %in% input_names))  #if_any selects rows where any of the columns have the input_names and everything() selects all the columns. 
  
#Removed Master List - not found from the Master_List in the input_names
Removed_Master_List <- Master_List %>%
    filter(!if_any(everything(), ~ tolower(.x) %in% input_names)) 

Removed_Master_List_Storage <- Removed_Master_List #Store in case - used in development of script 

save.image(file = "Taxa_List_Save_9.RData") #Save everything 
#load("Taxa_List_Save_9.RData")
}

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Adding in Genera for specimens where only the Genera is known

#Now we can loop up the Genus List and Extract Genus information from Genus_Names
#To run a successful search, I remove the L., sp., and ssp. from the names 
{
Genus_Names_Edited <- Genus_Names #Keeping original list in case
Genus_Names_Edited$Additional_Input_Name<- Genus_Names_Edited$name #Duplicate column
#There are some cases for family and names where the original input name is not properly captured, like 
#Caryophyllaceae being originally inputted as Caryophyllaceae indet. These cases only occur when the 
#Genus_Names_Edited$Additional_Input_Name is a Single word (eg no author)
#We need to capture this, so 
  
Genus_Names_Edited <- Genus_Names_Edited %>%
  mutate(match_condition = ifelse(grepl("^×?\\w+$", Additional_Input_Name), TRUE, FALSE), #Create condition to check if Additional_Input_Name is a single word or starts with "×" to indicate hybrids for consideration too 
    Additional_Input_Name = ifelse( #For this condition, replace Additional_Input_Name with original_input_name if there is an exact match in Edited_Combi_List$species
      match_condition & Additional_Input_Name %in% Edited_Combi_List$species, 
      Edited_Combi_List$original_input_name[match(Additional_Input_Name, Edited_Combi_List$species)], 
      Additional_Input_Name)) %>%
  select(-match_condition)  #Remove the match_condition column if no longer needed  

Genus_Names_Edited$name <- gsub ("L\\.|sp\\.|ssp\\.|#REF!|indet\\.", "", Genus_Names_Edited$name)
Genus_Names_Edited[] <- lapply(Genus_Names_Edited, trimws)
Genus_Names_Edited<-Genus_Names_Edited %>% distinct() 
Genus_Names_Edited<- Genus_Names_Edited[!(is.na(Genus_Names_Edited$name) | Genus_Names_Edited$name==""), ] #Get rid of any NAs or Blank Cells 
Genus_Names_Edited$name<-str_to_title(Genus_Names_Edited$name)
Genus_Names_Edited$name<- str_trim(Genus_Names_Edited$name, "right") #Remove white spaces in character string 
Genus_Names_Edited<- dcast(setDT(Genus_Names_Edited), name ~ rowid(name), value.var = "Additional_Input_Name")
Genus_Names_Edited<-Genus_Names_Edited %>% rename_at(vars(2:ncol(Genus_Names_Edited)), ~paste0('Additional_Input_Name',.))

#Repair cases where the name appears twice, once on its own, and again as something else (such as Caryophyllaceae and Caryophyllaceae Indet.)
#AKA ensure they are treated as two separate cases. 

Genus_Names_Edited_long <- Genus_Names_Edited %>%
  pivot_longer(cols = starts_with("Additional_Input_Name"),
               names_to = "Additional_Input_Name_Column",
               values_to = "Additional_Input_Name")

#Find incidences where the Additional Input Name is duplicated, indicating the root word has multiple occurrences in the Combi List
Genus_Names_Edited_long <- Genus_Names_Edited_long %>%
  group_by(Additional_Input_Name) %>%
  mutate(is_duplicate = n() > 1) %>%
  ungroup() %>%
  mutate(Additional_Input_Name = ifelse(is_duplicate & Additional_Input_Name != name, #Find cases where the name and Additional name do not match, and correct
                                        name,
                                        Additional_Input_Name)) %>%
  select(-is_duplicate)  #Remove the temporary column

Genus_Names_Edited <- Genus_Names_Edited_long %>%
  pivot_wider(names_from = Additional_Input_Name_Column, values_from = Additional_Input_Name) #Pivot back 

df_total4 = data.frame() #for accepted names
df_total5 = data.frame() #for names not recognized 
}
{
total_iterations <- length(unique(Genus_Names_Edited$name))

#Initialize the progress bar
Progress_Bar_GenusNames <- progress_bar$new(
  format = "  Processing [:bar] :percent :current/:total Taxon IDs :elapsed",
  total = total_iterations,  #Total number of iterations
  width = 60,                
  clear = FALSE              
)
}
#Now using the search_powo of the Kewr package, I use each Genus name in my list and search the KEW database. 
for (ge in unique(Genus_Names_Edited$name)) {                                                                
  Temp1 <- search_powo(ge, filters=c("genera"))                                                #Search KEW and store information 
  if (Temp1$total==0) {
    df_total5 <- rbind(df_total5,Temp1$query)  
  } else {
    Temp2 <- tidy(Temp1)                                                                       #Tidy information 
  }
  x <- c("accepted","author","kingdom","family","name","rank", "url","fqId","images", "synonymOf", "snippet","modified","bibliographicCitation","genus","taxonomicStatus",
         "phylum","nomenclaturalCode","source","namePublishedInYear","taxonRemarks","nomenclaturalStatus","lifeform","climate","hybrid","synonym", "locations", "plantae",
         "fungi","authors","species","reference","classification","basionymOf","synonyms","basionym","childNameUsages","nomenclaturalRemarks","infraspecies","hybridFormula")          #Create category images etc , which some outputs do contain
  Temp2[x[!(x %in% colnames(Temp2))]] = 0                                                      #If a Temp2 query doesn't have this column, then add it in with a value of 0, to allow rbinding later. 
  df_total4 <- rbind(df_total4,Temp2) 
  Progress_Bar_GenusNames$tick()
  }
  
#Edit df_total5 to have the original input names:
df_total5_storage<-df_total5
df_total5 <- Genus_Names_Edited %>%
  filter(name %in% df_total5[, 1]) %>% #Use first column regardless of name 
  pivot_longer(
    cols = -name,  #All columns except name
    names_to = "Additional_Input_Name",  #New column to hold the names of additional columns
    values_to = "value",  #New column to hold the values
    values_drop_na = TRUE   ) %>%  #Drop rows where value is NA
  select(value) %>%
  rename(name = value)

#df_total4 now has the Genus information from each Genus that was successfully searched for, but for each Genus the package also extracted the first 50 species in that Genus, so we remove this. 
#Also when working on a Genus level or higher, the resolution is so broad that the KEWR package can capture additional genera and other ranks not involved in our data set. 
#For example, by having Orchis in our list, the package captured the Genus Stichorkis due to the word Orchis being used in its description as a comparison. So here I also made sure to match
#against the Genus_Names_Edited list again. 
{
df_total4_backup<-df_total4 #Store for later comparing GBIF data 
df_total4<-df_total4[ , !names(df_total4) %in% c("genus","synonym")] #Drop empty genus column 
df_total4 <- df_total4 %>%
  filter(rank=="Genus") %>% #Some Genera in the input file are not accepted Genera names, but this can be the case for some regions and so we don't remove them automatically. 
  filter(name %in% Genus_Names_Edited$name) %>%
  distinct(name, author, .keep_all = TRUE) %>%
  dplyr::rename("genus" = "name")

df_total4 <- df_total4 %>%
  mutate(Accepted_Name_with_Author = paste(genus, author, sep = " ")) #Now I also have the Accepted Names with their authority names on them. 

df_total4 <- df_total4 %>%
  select(-c(taxonomicStatus))

#Now edit the df_total4 to match well with the Confirmed_Master_List
colnames(df_total4)[colnames(df_total4) == "accepted"] <- "taxonomicStatus"
df_total4$taxonomicStatus[df_total4$taxonomicStatus == 'FALSE'] <- 'Unplaced'
df_total4$taxonomicStatus[df_total4$taxonomicStatus == 'TRUE'] <- 'Accepted'

#Now we want to rule out cases where we have the same genus name but with different authority names,
#but we actually do know the authority name from the GBIF data: 

#Keep names with more than one word (here indicating it is a Genus with authority name)
Genus_Names_Edited_Authority <- Genus_Names_Edited %>%
  pivot_longer(cols = everything(), names_to = NULL) %>%
  filter(str_count(value, "\\S+") >= 2) %>%
  select(value)

#Using these genera, search in the df_total4 for matches
Genus_Names_Matches <- df_total4 %>%
  filter(Accepted_Name_with_Author %in% Genus_Names_Edited_Authority$value) %>%
  mutate(`Accepted Name`=genus)

#These ones we know for certain are correct, given that they have matching authority names. 

#Store the ones without matching authority names: 
Genus_Names_Non_Matches <- df_total4 %>%
  filter(!Accepted_Name_with_Author %in% Genus_Names_Edited_Authority$value)

#Also find cases where the Genus name is found, but the authority name in the GBIF data is not - these need to also be manually checked.
Genus_Compare <-df_total4_backup 

Genus_Compare<- Genus_Compare%>% 
  #filter(accepted==TRUE) %>% #We can keep this turned off to also capture synonyms
  mutate(Accepted_Name_with_Author = paste(name, author, sep = " ")) %>% 
  select(Accepted_Name_with_Author, author, family, name, rank) %>% 
  distinct() %>%
  left_join(Genus_Names_Edited, by = "name") %>%
  left_join(Edited_Combi_List, by = c("name" = "species"), relationship = "many-to-many") %>% #Find only cases where the Edited_Combi_List$original_input_name is not valid 
  filter(!is.na(original_input_name) | name != Additional_Input_Name1) %>%
  mutate(original_input_name = if_else(is.na(original_input_name), Additional_Input_Name1, original_input_name))

#Extract additional input names 
Genus_Compare_Extract <- Genus_Compare %>%
  filter(if_any(starts_with("Additional_Input_Name"), ~ . != Additional_Input_Name1 & !is.na(.))) %>%
  select(Accepted_Name_with_Author, author, family, name, rank, Additional_Input_Name1) %>%
  rename(original_input_name = Additional_Input_Name1)

Genus_Compare<-Genus_Compare%>%
  select(Accepted_Name_with_Author, author, family, name, rank, original_input_name)

Genus_Compare<-rbind(Genus_Compare,Genus_Compare_Extract)
Genus_Compare<-Genus_Compare%>%distinct()

#Find cases where there is only one option for the Genus, then we select that option. 
#This avoids manual taxonomic labour but maintaining confidence. But it is still an editing process. 
Genus_Compare_Confident <- Genus_Compare %>%
  group_by(name, original_input_name) %>%             #Group by the 'name' column
  filter(n() == 1) %>%           #Filter for names that appear only once
  ungroup()                      #Ungroup to return to the original structure

#These are the cases where we are confident in the correct authority information for the names. We will append this into the Leftovers after manual editing. 
Genus_Compare<- Genus_Compare%>% 
  filter(!original_input_name %in% Genus_Compare_Confident$original_input_name) %>%
  select(name, original_input_name) %>% distinct()
#Now Genus_Compare contains additional names which need to be manually checked and edited. 

#Edit Genus_Compare_Confident for binding into leftover:

Genus_Compare_Confident<-Genus_Compare_Confident%>%
  select(name, original_input_name, Accepted_Name_with_Author) %>%
  rename(Manually_Edited_Name = Accepted_Name_with_Author)

#Handle cases of unplaced names being synonyms
Genus_Unplaced_Synonyms <- df_total4 %>% 
  filter(taxonomicStatus == "Unplaced") %>% 
  select(Accepted_Name_with_Author, synonymOf)

Genus_Unplaced_Synonyms <- Genus_Unplaced_Synonyms %>%
  unnest_wider(synonymOf) %>% 
  mutate(full_synonym_name = paste(name, author, sep = " "), 
    across(c(Accepted_Name_with_Author, full_synonym_name), ~ trimws(.))) %>% 
  select(Accepted_Name_with_Author, full_synonym_name)    

Genus_Compare_Confident <- Genus_Compare_Confident %>%
  mutate(Manually_Edited_Name = ifelse(
      original_input_name %in% Genus_Unplaced_Synonyms$Accepted_Name_with_Author,
      Genus_Unplaced_Synonyms$full_synonym_name[match(original_input_name, Genus_Unplaced_Synonyms$Accepted_Name_with_Author)],
      Manually_Edited_Name))

}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Adding in Family for specimens where only the Family is known
{
Additional_Ranks<-Additional_Ranks %>% distinct()

df_total6 = data.frame() #for accepted names
df_total7 = data.frame() #for names not recognized 
}

total_iterations <- length(unique(Additional_Ranks$name))

#Initialize the progress bar
Progress_Bar_Family <- progress_bar$new(
  format = "  Processing [:bar] :percent :current/:total Taxon IDs :elapsed",
  total = total_iterations,  #Total number of iterations
  width = 60,                
  clear = FALSE              
)

#Now using the search_powo of the Kewr package, I use each Family name in my list and search the KEW database. 
for (fa in unique(Additional_Ranks$name)) {                                                                
  Temp1 <- search_powo(fa, filters=c("families"))                                                                       #Search KEW and store information 
  if (Temp1$total==0) {
    df_total7 <- rbind(df_total7,Temp1$query)  
  } else {
    Temp2 <- tidy(Temp1)                                                                       #Tidy information 
  }
  x <- c("accepted","author","kingdom","family","name","rank", "url","fqId","images", "synonymOf", "snippet")          #Create category images etc , which some outputs do contain
  Temp2[x[!(x %in% colnames(Temp2))]] = 0                                                      #If a Temp2 query doesn't have this column, then add it in with a value of 0, to allow rbinding later. 
  df_total6 <- rbind(df_total6,Temp2) 
  
  Progress_Bar_Family$tick()
  }

#df_total6 now has the Family information from each Family that was successfully searched for, but for each Family the package also extracted the first 50 species/genus in that Family, so we remove this. 
#Also when working on a Genus level or higher, the resolution is so broad that the KEWR package can campture additional Genus and other ranks not involved in our dataset. 
#So here I also made sure to match against the Additional_Ranks list again. 
{
Confirmed_Additional_Ranks <- df_total6 %>%
  filter(rank=="Family") %>% 
  filter(name %in% Additional_Ranks$name) %>%
  distinct(name, .keep_all = TRUE) 

Confirmed_Additional_Ranks <- Confirmed_Additional_Ranks %>%
  select(-c(images, synonymOf)) %>%
  mutate(Accepted_Name_with_Author = paste(family, author, sep = " ")) #Now I also have the Accepted Names with their authority names on them. 

#Now edit the Confirmed_Additional_Ranks to match well with the Confirmed_Master_List
colnames(Confirmed_Additional_Ranks)[colnames(Confirmed_Additional_Ranks) == "accepted"] <- "taxonomicStatus"
Confirmed_Additional_Ranks$taxonomicStatus[Confirmed_Additional_Ranks$taxonomicStatus == 'FALSE'] <- 'Unplaced'
Confirmed_Additional_Ranks$taxonomicStatus[Confirmed_Additional_Ranks$taxonomicStatus == 'TRUE'] <- 'Accepted'

#Only keep family at the family level if there is no species already of that same family in the Master List:
Confirmed_Additional_Ranks <- Confirmed_Additional_Ranks[!(Confirmed_Additional_Ranks$family %in% Confirmed_Master_List$family), ]

#Now we want to rule out cases where we have the same family name but with different authority names,
#but we actually do know the authority name from the GBIF data: 

#Keep names with more than one word (here indicating it is a family with authority name)
Family_Names_Edited_Authority <- Additional_Ranks %>%
  pivot_longer(cols = everything(), names_to = NULL) %>%
  filter(str_count(value, "\\S+") >= 2) %>%
  select(value)

#Using these families, search in the Confirmed_Additional_Ranks for matches
Family_Names_Matches <- Confirmed_Additional_Ranks %>%
  filter(Accepted_Name_with_Author %in% Family_Names_Edited_Authority$value) %>%
  mutate(`Accepted Name`=family)


#These ones we know for certain are correct, given that they have matching authority names. 

#Store the ones without matching authority names: 
Family_Names_Non_Matches <- Confirmed_Additional_Ranks %>%
  filter(!Accepted_Name_with_Author %in% Family_Names_Edited_Authority$value)

save.image(file = "Taxa_List_Save_10.RData") #Save everything 
#load("Taxa_List_Save_10.RData")
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Adding in Genera and Family Data to Master List

#Genus_Names_Matches can contain accepted names which have other unplaced names as its synonym. 
#We can append this in and remove from downstream manipulation. 
#Account also for multiple synonyms and also account for existing synonyms in the Genus_Names_Matches

#First extract synonyms for the Accepted Name Genera
Genus_Names_Matches_Lookup <- Genus_Names_Matches %>%
  filter(taxonomicStatus=="Accepted") %>%
  mutate(url = gsub("/taxon/urn:lsid:ipni.org:names:", "", url)) %>%  # Remove specific part from the URL
  mutate(url = trimws(url)) %>% select(url)

add_cols <- function(Temp2, cols) {
  add <- cols[!cols %in% names(Temp2)]
  if(length(add) !=0 ) Temp2[add] <- NA
  return(Temp2)
}

total_iterations <- length(unique(Genus_Names_Matches_Lookup$url))

#Initialize the progress bar
KEWR_Progress_Bar_Acc <- progress_bar$new(
  format = "  Processing [:bar] :percent :current/:total Accepted Name Query Progress :elapsed",
  total = total_iterations,  #Total number of iterations
  width = 60,                
  clear = FALSE              
)

List_Genus = data.frame()
for (ac in unique(Genus_Names_Matches_Lookup$url)) {          
  Temp1 <- lookup_powo(ac)                         
  Temp2 <- tidy(Temp1)                            
  Temp2 <- add_cols(Temp2, c("modified","bibliographicCitation","genus","taxonomicStatus","kingdom","phylum","clazz","subclass","order","family","nomenclaturalCode","source","namePublishedInYear","taxonRemarks","nomenclaturalStatus",
                             "lifeform","climate","hybrid","synonym","plantae","fungi","locations","fqId","name","authors","species","rank","reference","classification","basionymOf","synonyms","response","queryId", "basionym", "childNameUsages",
                             "nomenclaturalRemarks", "infraspecies", "hybridFormula", "paftolId")) 
  List_Genus <- rbind(List_Genus,Temp2)  
  

  KEWR_Progress_Bar_Acc$tick()
}

{
List_Genus <- List_Genus %>%
  mutate(name = str_trim(name))  
  
  
Synonym_Genus2<- do.call(rbind.data.frame, List_Genus$synonyms) #Gives synonyms in order of extracting from List_Genus, but without corresponding names of accepted species 

Synonym_Genus2<-Synonym_Genus2%>%mutate(Synonym_with_Author = paste(name, author, sep = " "))      #Now I have the Synonyms with their authority names on them. 
List_Genus<-List_Genus%>%mutate(Accepted_Name_with_Author = paste(name, authors, sep = " "))                 #Now I have the Accepted Name with their authority names on them.
List_Genus$Accepted_Name_with_Author<-gsub("NA","", List_Genus$Accepted_Name_with_Author)                    #Remove NAs that get appended to Accepted_Name_with_Author is the authors category is NA
List_Genus$Accepted_Name_with_Author <- str_trim(List_Genus$Accepted_Name_with_Author)

Synonym_Genus<- setDT(List_Genus)[, .(Synonym = c(name, unlist(synonyms,recursive = TRUE))), by = Accepted_Name_with_Author] #This unnests the list (including matching accepted name) but does it for all list columns, and I do not want this. 
Synonym_Genus<-Synonym_Genus %>%                                                                   #Now I keep the urn:lsid:ipni.org:names codes as these match the Accepted Name to the Synonym 
  filter(grepl("urn:lsid:ipni.org:names",Synonym_Genus$Synonym)) %>% #If this doesn't work when re-running, turn Synonym_Genus$Synonym to only "Synonym"
  rename("fqId" = "Synonym")

Synonym_Genus<-left_join(Synonym_Genus, Synonym_Genus2, by="fqId")                                 #Now I join the Synonym to the Accepted Name 
Synonym_Genus<-Synonym_Genus%>%
  transmute(Accepted_Name_with_Author=Accepted_Name_with_Author,
            Synonym_Genus_Authors = Synonym_with_Author)

#Repeat for Basionyms - likely already in the synonyms for each species, but will do again for due diligence 
Basionyms<- try(do.call(rbind.data.frame, List_Genus$basionymOf), silent = TRUE) #Gives Basionyms in order of extracting from List_Genus, but without corresponding names of accepted species 
Basionyms2 <- try(do.call(rbind.data.frame, List_Genus$basionym), silent = TRUE)  #second category of Basionyms
Basionyms <- rbind(Basionyms, Basionyms2)

if (!all(grepl("Error in do.call", Basionyms))) {
  Basionyms <- Basionyms %>%
    drop_na(name) #Remove NAs from columns 
  Basionym_Names_Genus1<- setDT(List_Genus)[, .(Basionym = c(name, unlist(basionymOf,recursive = TRUE))), by = Accepted_Name_with_Author]
  Basionym_Names_Genus2<- setDT(List_Genus)[, .(Basionym = c(name, unlist(basionym,recursive = TRUE))), by = Accepted_Name_with_Author]
  Basionym_Names_Genus <- rbind(Basionym_Names_Genus1, Basionym_Names_Genus2)
  Basionym_Names_Genus <- Basionym_Names_Genus %>%
    drop_na(Basionym) %>% #Remove NAs from columns 
    filter(grepl("urn:lsid:ipni.org:names", Basionym)) %>%
    rename("fqId" = "Basionym")
  Basionym_Names_Genus<-left_join(Basionym_Names_Genus, Basionyms, by="fqId")                                    #Now I join the Synonym to the Accepted Name 
  Basionym_Names_Genus<-Basionym_Names_Genus%>%mutate(Synonym_with_Author = paste(name, author, sep = " "))      #Now I have the Synonyms with their authority names on them. 
  Basionym_Names_Genus<-Basionym_Names_Genus%>%
    transmute(Accepted_Name_with_Author=Accepted_Name_with_Author,
              Synonym_Genus_Authors = Synonym_with_Author)
  #Now add into Synonym list:
  Synonym_Genus <- rbind(Synonym_Genus,Basionym_Names_Genus) #List of all synonyms and Basionyms from KEW dataset per accepted species 
} else {
  print("Skipping because there are no Basionyms")
}
Synonym_Genus <- Synonym_Genus %>%
  distinct() #Remove duplicates

#Remove cases where the Accepted Name and the Synonym in a given row are the same, as this is redundant 
Synonym_Genus <- Synonym_Genus[Synonym_Genus$Accepted_Name_with_Author != Synonym_Genus$Synonym_Genus_Authors,]

Final_Synonym_Genus<- dcast(setDT(Synonym_Genus), Accepted_Name_with_Author ~ rowid(Accepted_Name_with_Author), value.var = "Synonym_Genus_Authors")
colnames(Final_Synonym_Genus)[1] <- "Accepted_Name_with_Author"

Final_Synonym_Genus<-Final_Synonym_Genus %>% rename_at(vars(2:ncol(Final_Synonym_Genus)), ~paste0('Synonym_',.))  

}
{  
Final_Synonym_Genus$Accepted_Name_with_Author<-str_trim(Final_Synonym_Genus$Accepted_Name_with_Author)
List_Genus$Accepted_Name_with_Author<-str_trim(List_Genus$Accepted_Name_with_Author)
List_Genus<-left_join(List_Genus,Final_Synonym_Genus, by="Accepted_Name_with_Author")
Genus_Names_Matches <- Genus_Names_Matches[, names(Genus_Names_Matches) %in% names(List_Genus)]  
missing_cols <- setdiff(names(List_Genus), names(Genus_Names_Matches))
Genus_Names_Matches[missing_cols] <- NA  

#Make sure columns are same type 
Genus_Names_Matches <- Genus_Names_Matches %>%
  mutate(across(
    names(.)[names(.) %in% names(List_Genus)], 
    ~ {
      col_name <- cur_column()
      target_type <- typeof(List_Genus[[col_name]])
      if (typeof(.) != target_type) {
        match.fun(paste0("as.", target_type))(.)
      } else {.}}))

List_Genus <- bind_rows(
  List_Genus,
  Genus_Names_Matches %>%
    filter(!Accepted_Name_with_Author %in% List_Genus$Accepted_Name_with_Author))

Genus_Names_Matches<-List_Genus

Genus_Unplaced_Synonyms$Accepted_Name_with_Author <- str_trim(Genus_Unplaced_Synonyms$Accepted_Name_with_Author)
Genus_Unplaced_Synonyms$full_synonym_name <- str_trim(Genus_Unplaced_Synonyms$full_synonym_name)  
Genus_Names_Matches$Accepted_Name_with_Author <- str_trim(Genus_Names_Matches$Accepted_Name_with_Author)

Genus_Unplaced_Synonyms_Matches <- Genus_Unplaced_Synonyms %>%
  filter(full_synonym_name %in% Genus_Names_Matches$Accepted_Name_with_Author)

Genus_Unplaced_Synonyms <- Genus_Unplaced_Synonyms %>%
  filter(!full_synonym_name %in% Genus_Names_Matches$Accepted_Name_with_Author)

#Remove the Genus_Unplaced_Synonyms_Matches from the Genus_Names_Matches
Genus_Names_Matches <- Genus_Names_Matches %>%
  filter(!Accepted_Name_with_Author %in% Genus_Unplaced_Synonyms_Matches$Accepted_Name_with_Author)

Genus_Unplaced_Synonyms_Matches <- Genus_Unplaced_Synonyms_Matches %>%
  select(full_synonym_name, Accepted_Name_with_Author) %>%
  pivot_longer(cols = Accepted_Name_with_Author,
               names_to = "Synonym_Column",
               values_to = "Synonym_Name")

synonym_columns <- Genus_Names_Matches %>%
  select(starts_with("Synonym_")) %>%
  colnames()

current_highest_synonym_number <- if (length(synonym_columns) > 0) {
  suppressWarnings(
    synonym_columns %>%
      gsub("Synonym_", "", .) %>%
      as.numeric() %>%
      max(na.rm = TRUE))
} else {
  0
}

Genus_Unplaced_Synonyms_Matches <- Genus_Unplaced_Synonyms_Matches %>%
  rowid_to_column(var = "row_id")

Genus_Unplaced_Synonyms_Matches <- Genus_Unplaced_Synonyms_Matches %>%
  pivot_wider(
    id_cols = full_synonym_name,  # Use full_synonym_name as the identifier
    names_from = row_id,          # Use row_id for creating new column names
    values_from = Synonym_Name,   # Fill the new columns with Synonym_Name
    names_prefix = "Synonym_"     # Prefix for the new columns
  )

Genus_Unplaced_Synonyms_Matches <- as.data.frame(t(apply(Genus_Unplaced_Synonyms_Matches, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
colnames(Genus_Unplaced_Synonyms_Matches)[1] <- "Accepted_Name_with_Author"
colnames(Genus_Unplaced_Synonyms_Matches) <- sub("V", "", colnames(Genus_Unplaced_Synonyms_Matches))

Genus_Unplaced_Synonyms_Matches <- Genus_Unplaced_Synonyms_Matches %>% 
  select(where(~ !all(is.na(.)))) %>%
  rename_with(~ paste0('Synonym_', seq_along(.) - 1 + current_highest_synonym_number + 1),-1)

Genus_Unplaced_Synonyms_Matches <- Genus_Unplaced_Synonyms_Matches %>%
  mutate(across(everything(), ~ str_trim(.)))

Genus_Names_Matches$Accepted_Name_with_Author <- str_trim(Genus_Names_Matches$Accepted_Name_with_Author)

Genus_Names_Matches <- left_join(Genus_Names_Matches, Genus_Unplaced_Synonyms_Matches, by = "Accepted_Name_with_Author")

suppressPackageStartupMessages(library(plyr))
Confirmed_Master_List<-rbind.fill(Confirmed_Master_List,Genus_Names_Matches,Family_Names_Matches)
detach(package:plyr)
library("dplyr")
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Combining leftover names not picked up by the search_powo command: This section requires taxonomic decision making and expertise. 

#First, I need to make a comprehensive list:
{
if (ncol(df_total3) > 0) {
  colnames(df_total3)[1] <- "name"  #The list of input names that did not get accepted
}

if (ncol(df_total5) > 0) {
  colnames(df_total5)[1] <- "name"  #The list of genus names that did not get accepted
}

if (ncol(df_total7) > 0) {
  colnames(df_total7)[1] <- "name" #The list of family names that did not get accepted - but likely this doesn't work since it is empty (ideally)
}

#Define where unrecognized names (leftovers) go:
Leftovers <- NULL
}
#rbind each list of leftover names, so long as that given dataframe is not empty. 
{
if (nrow(df_total3) > 0) {
  Leftovers <- rbind(Leftovers, df_total3)
}
if (nrow(df_total5) > 0) {
  Leftovers <- rbind(Leftovers, df_total5)
}
if (nrow(df_total7) > 0) {
  Leftovers <- rbind(Leftovers, df_total7)
}
}
#Compress all the Master List's Accepted Names with Authority and Synonyms into a list, but using a smaller Dataframe to save computation time: 
{
#Make a vector of all relevant names: 
NamesL<- colnames(Confirmed_Master_List) #Create a list of the column names in the Master List
NamesL <- grep('Accepted_Name_with_Author|Synonym_1|Synonym_2|Synonym_3|Synonym_4|Synonym_5|Synonym_6|Synonym_7|Synonym_8|Synonym_9|Additional_Input_Name', NamesL,value = TRUE) #Filter out to only keep relevant column names and not ones like SynonymchildNameUsages (aka keep the accepted name and all synonym columns)

#First, make smaller Data frame with compressed names list:
Master_List_Mini_Leftovers <- Confirmed_Master_List %>%
  unite("names",NamesL,remove=F,sep=",",na.rm = TRUE) %>%
  transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
            names = names)

#Normally one could fuzzy join the input names to the compressed list of all Accepted Names and Synonyms and Input Names 
#But note that sometimes fuzzyjoin can join when the whole Leftover word is found within the Master List word, but maybe it is not the exact same. 
#For example, Dupontia fisheri f. from the leftover list joins with Dupontia fisheri f. aristata (Mate ex Polunin) Lepage
#This is okay because they are actually both synonyms, but the remove.authors command removed the "psilosantha" from Dupontia fisheri f. psilosantha (Rupr.) Polunin,
#when it should not have, and this caused the problem Also see the example of 
#Myrtillus uliginosus, which is actually Myrtillus uliginosus (L.) Drejer which fuzzy joins incorrectly to Myrtillus uliginosus var. pubescens (Hornem.) Porsild
#To counteract this, I can use the original input name of the species instead. 

Leftovers<- left_join(Leftovers, Combi_List_Cleaned, by=c('name'='species')) #This will not work for the Genera and Families, but this is good because they are too broad and need to be handled manually. 
colnames(Leftovers)[2] <- "names"

#Add in Genus_Compare
colnames(Genus_Compare)[2] <- "names"

if (nrow(Genus_Compare) > 0) {
Leftovers<-rbind(Leftovers,Genus_Compare)

#Also add in missing Family Names that need manual checking:
Families<-Genus_Names_Edited%>%filter(str_ends(name, "eae")) %>%
  select(name, starts_with("Additional_Input_Name")) %>%
  rowwise() %>%
  mutate(Combined_Input = list(c_across(starts_with("Additional_Input_Name")))) %>%
  ungroup() %>%
  select(name, Combined_Input) %>%
  unnest(cols = Combined_Input) %>%
  rename(names = Combined_Input) %>%
  filter(!is.na(names))

if (nrow(Families) > 0) {
  Leftovers<-rbind(Leftovers,Families)
}
}

#However there are some cases where the species name was found by KEWR, but in the end
#it was ascribed to a different authority name than the original input one, and then this got lost
#as it was purged from the confirmed master list due to an incorrect authority name. 
#An example would be "Gnaphalium norvegicum Retz.", which when searched using "Gnaphalium norvegicum"
#did successfully produce "Gnaphalium norvegicum Gunnerus", so the species name was found, but under a different authority. 
#If"Gnaphalium norvegicum Gunnerus" was not also in the GBIF data, then this finding would be removed from the 
#Confirmed Master list because it did not contain the correct authority, but it also would not make it into the Leftovers since it was technically found. 
#So I need to account for this by adding on any names not found in the Master List nor the leftover list. 

#I can extract any names not found in my confirmed list by comparing it to the Combi_List_Cleaned:
#Make a vector of all relevant names: 
Names_Find_Leftovers<- colnames(Confirmed_Master_List) #Create a list of the column names in the Project_List_Others
Names_Find_Leftovers <- grep('Accepted Name|Accepted_Name_with_Author|Synonym_1|Synonym_2|Synonym_3|Synonym_4|Synonym_5|Synonym_6|Synonym_7|Synonym_8|Synonym_9|Additional_Input_Name|Other_Input_Name_|Input_Name_Form|Added_Name', Names_Find_Leftovers ,value = TRUE) 

#Make smaller Data frame with compressed names list:
Leftovers_List_Mini <- Confirmed_Master_List %>%
  unite("Names",Names_Find_Leftovers,remove=F,sep=",",na.rm = TRUE) %>%
  transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
            original_input_name = Names)

#Now for all species we have the species name with the author, and a list of all other 
#possible names affiliated to that species, including synonyms, basionyms, input names found in the input lists,
#subspecies names, variety names, form names, etc. 

Combi_List_Cleaned_Leftovers <- Combi_List_Cleaned #Prepare to have only leftover names in the Combi List 

#Capture names which are missing their author information (aka when the last word is not starting with a capital letter)
#These cases should not be fuzzy joined, as, for example, Elyna bellardi can be joined to both Elyna bellardii Hartm. and Elyna bellardii (All.) K.Koch,
#so they need manual processing in the Leftovers Category. 

#Create a function to find cases where the last word is not capitalized (not an author), and where the specific epithet is not repeated (indicating an autonym) - autonyms are treated differently, see later script 
Leftovers_Function <- function(name) {
  #Split name into components (words)
  components <- unlist(strsplit(name, "\\s+"))
  #Check if the last word starts with a lowercase letter
  last_word_starts_lower <- grepl("^[a-z]", components[length(components)])
  #Check if the second word (specific epithet) appears only once in the string
  if (length(components) >= 2) {
    second_word <- components[2]
    second_word_not_repeated <- sum(components == second_word) == 1
  } else {
    second_word_not_repeated <- FALSE
  }
  #TRUE if both conditions are met
  return(last_word_starts_lower && second_word_not_repeated)
}

#Use function to find these cases and store them. 
#Make a logical vector 
logical_index <- sapply(Combi_List_Cleaned_Leftovers$original_input_name, Leftovers_Function)

#Subset the Combi_List_Cleaned_Leftovers using the logical index
Leftovers_non_Author_non_autonym <- Combi_List_Cleaned_Leftovers[logical_index, ]

#Now remove these cases from the Combi_List_Cleaned_Leftovers before the fuzzy join:
Combi_List_Cleaned_Leftovers<-Combi_List_Cleaned_Leftovers%>%filter(!original_input_name %in% Leftovers_non_Author_non_autonym$original_input_name)

#Next I want to change all names to correspond to the Accepted Name from the Confirmed_Master_List
#Also Need to account for authority names with brackets (fuzzy join cant handle this):
Combi_List_Cleaned_Leftovers$original_input_name_backup<-Combi_List_Cleaned_Leftovers$original_input_name
Combi_List_Cleaned_Leftovers$original_input_name <- gsub("[()]", "", Combi_List_Cleaned_Leftovers$original_input_name)
Leftovers_List_Mini$original_input_name <- gsub("[()]", "", Leftovers_List_Mini$original_input_name)

#Fuzzy join the names to the compressed list of all names from the Leftovers_List_Mini
Leftover_Names<- fuzzy_join(Leftovers_List_Mini, Combi_List_Cleaned_Leftovers, match_fun = str_detect, by = "original_input_name", mode = "full")
}
{
Leftover_Names <- Leftover_Names %>%  #Keep all cases where there is a matched Accepted_Name_with_Author
  filter(is.na(Accepted_Name_with_Author)) %>%
  transmute(Leftovers = original_input_name_backup)

#Compare with Leftovers list to ensure all of the Leftovers from this list are 
#included in the Leftover_Names 
Leftover_Names<- left_join(Leftover_Names, Combi_List_Cleaned, by=c('Leftovers'='original_input_name')) #This will not work for the Genera and Families, but this is good because they are too broad and need to be handled manually. 
colnames(Leftover_Names)[1] <- "names"
colnames(Leftover_Names)[2] <- "name"
Leftovers<- rbind(Leftovers,Leftover_Names)
Leftovers<-Leftovers%>%distinct()

#Temporarily remove the Leftovers_non_Author_non_autonym cases 
Leftovers<-Leftovers%>%filter(!names %in% Leftovers_non_Author_non_autonym$original_input_name)
Master_List_Mini_Leftovers<- fuzzy_join(Master_List_Mini_Leftovers, Leftovers, match_fun = str_detect, by = "names", mode = "full") %>% distinct() #Likely to be empty 
}

{
Master_List_Mini_Leftovers<-subset(Master_List_Mini_Leftovers, select = -c(names.x,name)) #Remove the compressed list now
colnames(Master_List_Mini_Leftovers)[2] <- "Leftover_names"
Leftover_Names_Input<- names(Master_List_Mini_Leftovers)[-1]                              #Take all column names except first one, for use in Dcast 
Master_List_Mini_Leftovers<-Master_List_Mini_Leftovers %>% drop_na()                      #Keep only relevant cases 

#Remove these from the leftovers list 
Leftovers<- Leftovers %>% 
  filter(!Leftovers$names %in% Master_List_Mini_Leftovers$Leftover_names) #Filter names NOT in the Non_Leftovers data frame. 

Master_List_Mini_Leftovers<- dcast(setDT(Master_List_Mini_Leftovers), Accepted_Name_with_Author ~ Leftover_names) #Dcast to have all leftover names for a given Accepted_Name_with_Author
Master_List_Mini_Leftovers<-Master_List_Mini_Leftovers %>% drop_na(Accepted_Name_with_Author)
#Now shift all names to the left to fill in NA cells (tidy it up)

if (nrow(Master_List_Mini_Leftovers) > 0) {  
Master_List_Mini_Leftovers <- as.data.frame(t(apply(Master_List_Mini_Leftovers, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
colnames(Master_List_Mini_Leftovers)[2:ncol(Master_List_Mini_Leftovers)] = ""
features <- c(sprintf("Other_Input_Name_%d", seq(0,ncol(Master_List_Mini_Leftovers)-1))) #Rename columns  
colnames(Master_List_Mini_Leftovers) <- features
colnames(Master_List_Mini_Leftovers)[1] <- "Accepted_Name_with_Author"
#Select to keep only columns where at least one value is not NA
Master_List_Mini_Leftovers<- Master_List_Mini_Leftovers %>%
  select(-where(~all(is.na(.))))

#Left join 
Confirmed_Master_List<- left_join(Confirmed_Master_List,Master_List_Mini_Leftovers, by=c("Accepted_Name_with_Author"))
}

save.image(file = "Taxa_List_Save_11.RData") #Save everything 
#load("Taxa_List_Save_11.RData")
}

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Manually editing leftovers 
{
colnames(Leftovers_non_Author_non_autonym)[1] <- "name"
colnames(Leftovers_non_Author_non_autonym)[2] <- "names"
Leftovers<-rbind(Leftovers, Leftovers_non_Author_non_autonym) #Add back in the Leftovers_non_Author_non_autonym  

Leftovers <- Leftovers %>%
  mutate(names = if_else(is.na(names), name, names))

#Retrieve any forgotten leftover names lost in the fuzzy matching. 
#To do this, compile all names already matched or marked as leftover:
#Select columns from Confirmed_Master_List
Finding_Lost_Leftovers <- Confirmed_Master_List %>%
  select(Accepted_Name_with_Author, starts_with("Synonym_"), starts_with("Additional_Input_Name")) %>%
  pivot_longer(cols = c(starts_with("Synonym_"), starts_with("Additional_Input_Name")),
               names_to = "Name_Type",
               values_to = "names") %>%
  filter(!is.na(names)) 

Finding_Lost_Leftovers <- Finding_Lost_Leftovers %>%
  bind_rows(Confirmed_Master_List %>%
              select(Accepted_Name_with_Author) %>%
              rename(names = Accepted_Name_with_Author) %>%
              mutate(Name_Type = "Accepted_Name_with_Author",
                     Accepted_Name_with_Author = names)) %>%
  select(names) %>%
  distinct()

#Now add in leftovers:
Finding_Lost_Leftovers1<-Leftovers %>% select(names)
Finding_Lost_Leftovers2<-Genus_Compare_Confident %>% select(Manually_Edited_Name) %>% rename(names = Manually_Edited_Name)
Finding_Lost_Leftovers3<-Genus_Compare_Confident %>% select(original_input_name) %>% rename(names = original_input_name)
Finding_Lost_Leftovers<-rbind(Finding_Lost_Leftovers,Finding_Lost_Leftovers1,Finding_Lost_Leftovers2, Finding_Lost_Leftovers3)

#Now Finding_Lost_Leftovers contains all names which have been allocated a place 
Finding_Lost_Leftovers_Test<-Edited_Combi_List %>%
  filter(!original_input_name %in% Finding_Lost_Leftovers$names) %>%
  rename(name = species,
         names = original_input_name) %>% distinct()

#Add these names into Leftovers  
Leftovers<-rbind(Leftovers,Finding_Lost_Leftovers_Test) %>% distinct()

#Pivot to ensure the dataframe is only the "name" and the "original_input_name" 
Leftovers <- Leftovers %>%
  pivot_longer(cols = -name, names_to = "original_input_name", values_to = "synonyms") %>%
  filter(!is.na(synonyms)) %>%
  select(name, original_input_name = synonyms)

Leftovers<-Leftovers%>%distinct()

#Add new column for manually edited names 
Leftovers$Manually_Edited_Name <- NA

#Document Results
Documentation_Results$Nine<-nrow(Leftovers)

#Taxonomic Expert checking for autonyms
#Unrecognized autonym names, which contain names containing the epithet for the typical infraspecific taxon (such as the subspecies) 
#and therefore the same epithet as the specific epithet (e.g. Saxifraga paniculata subsp. paniculata), can be manually checked by a taxonomic expert and re-added into the confirmed master list. 

#Create function to find autonyms, using the rule of spelling species names as: "Genus... species... infraspecific_rank. ... infraspecific_epithet (note that the infraspecific_rank ends in a period), where the species and infraspecific_epithet match 
search_autonym <- function(name) {
  if (grepl(" x ", name)) { #Dont catch hybrids 
    return(FALSE)
  }
  pattern <- "\\b(\\S+)\\s+\\S+\\.\\s+\\1\\b" #Updated to accomodate more white spaces than there should be 
  #pattern <- "\\s(\\S+)\\s+\\S+\\.\\s+\\1$"
  #For the pattern,
  #\\s+ finds the white space before specific epithet (since this is also the first occurring white space), the (\\S+) finds the specific epithet and captures it () to compare to the infraspecific epithet,
  #\\s+ looks for white spaces, which will not occur during the specific epithet and the infraspecific rank, \\S+\\. finds the infraspecific rank, including the period. \\s+\\1$ finds a space between the infra rank and the infra epithet,
  #where 1 used to compare it to the specific epithet stored in the brackets. The $ checks for the match at the end of the string, therefore matching the infraspecific epithet. 
  #Return TRUE if the name matches the pattern (autonym), otherwise FALSE
  grepl(pattern, name)
}

#Apply the function to the original_input_name column and create a new dataframe 
Autonyms <- Leftovers %>%
  filter(sapply(original_input_name, search_autonym))

#Remove the autonyms from the Leftovers list
Leftovers <- Leftovers %>%
  filter(!sapply(original_input_name, search_autonym))

#Make sure autonym is not already in the Master List but without trimmed white spaces
Confirmed_Master_List <- Confirmed_Master_List %>%
  mutate(Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author))

Autonyms <- Autonyms %>%
  mutate(original_input_name = str_trim(original_input_name))

Autonyms <- Autonyms %>%
  filter(!original_input_name %in% Confirmed_Master_List$Accepted_Name_with_Author)

#Remove Accents in Autonyms 
Autonyms <- Autonyms %>% mutate(across(everything(), ~ stri_trans_general(., "Latin-ASCII")))

Autonyms <- Autonyms %>% #Extract genus name and specific epithet 
  mutate(species_name = sapply(strsplit(original_input_name, "\\s+"), function(x) paste(head(x, 2), collapse = " ")))

#However some of these autonyms will have issues with their spelling, just like the other leftovers. 
#For example "Campanula gieseckiana subsp. gieseckiana" should be "Campanula giesekiana subsp. giesekiana"

#This step therefore needs manual checking by a taxonomic expert. 
#First query species names without autonym infraspecific epithets to see if the names find legitimate matches. They therefore do not need to be manually edited. 

#Run search including progress bar, as the search time can be long. 
#Calculate the total number of iterations based on the length of the species list being used as input for searching 
total_iterations <- length(unique(Autonyms$species_name))
}
{
#Species search progress bar object
KEWR_Progress_Bar <- progress_bar$new(
  format = "  Processing [:bar] :percent :current/:total Species Names Searched in KEWR :elapsed",
  total = total_iterations,  #Total number of iterations
  width = 60,                
  clear = FALSE              
)

#Create three dataframes, one for the species with accepted names, one for species
#where it is a synonym, and one for species names not recognized by KEWR. 
#I make two DFs because the output differs based on this category. 
df_Autonyms1 = data.frame() #for accepted species 
df_Autonyms2 = data.frame() #for synonyms, which have 2 or more rows because one row is the accepted name for that given synonym
df_Autonyms3 = data.frame() #for ones not matching to POWO - these are the ones needing manual editing 
Temp2 <- data.frame(accepted=logical(),
                    author=character(),
                    kingdom=character(),
                    family=character(),
                    name=character(),
                    rank=character(),
                    url=character(),
                    fqId=character(),
                    images=numeric(),
                    synonymOf=numeric(),
                    snippet=numeric())
}
#Now using the search_powo of the Kewr package, I use each species name in my list and search the KEW database. 
for (sp in unique(Autonyms$species_name)) {                                                     #for each sp species in my list search KEW. 
  Temp1 <- search_powo(sp,)                                                                  #Search KEW and store information 
  if (Temp1$total==0) {
    df_Autonyms3 <- rbind(df_Autonyms3,Temp1$query)  
  } else {
    Temp2 <- tidy(Temp1)                                                                     #Tidy information 
  }
  x <- c("accepted","author","kingdom","family","name","rank", "url","fqId","images", "synonymOf", "snippet")          #Create category images etc , which some outputs do contain
  Temp2[x[!(x %in% colnames(Temp2))]] = 0                                                    #If a Temp2 query doesn't have this column, then add it in with a value of 0, to allow rbinding later. 
  
  if (nrow(Temp2)>=2) {                                                                      #If there are 2 or more rows, store in df_Autonyms2 as this is a synonym species. 
    df_Autonyms2 <- rbind(df_Autonyms2,Temp2)  
  } else {
    df_Autonyms1 <- rbind(df_Autonyms1,Temp2)                                                      #If there is only one row, store in df_Autonyms1 as this is already an accepted name
  }
  KEWR_Progress_Bar$tick() #Progress Bar
}
{
  df_Autonyms1 <- df_Autonyms1 %>% #Remove duplicates 
    distinct()
  
  df_Autonyms2 <- df_Autonyms2 %>% #Remove duplicates 
    distinct()
  
  df_Autonyms3 <- df_Autonyms3 %>% #Remove duplicates 
    distinct()
}

#From this, cases of spelling inconsistencies could be found from df_Autonyms3
#Use this code to automate the process of filling in the original input names. Then copy and paste in the manually edited name:
#Print the results to the R Console - then copy and paste back in to this section. 
df_Autonyms3 <- setNames(df_Autonyms3, c("original_input_name", names(df_Autonyms3)[-1]))
df_Autonyms3_save_<-df_Autonyms3 #Save for later use 

cat(paste("Autonyms$species_name[Autonyms$species_name == '", df_Autonyms3$original_input_name, "'] <- 'Write_Corrected_POWO_Accepted_Name_Here'\n", sep = ""), sep = "\n") #Changes the Autonyms$species_name accordingly 

#!#!#! Paste in the appropriate code and corrections here:

#Examples:
#Autonyms$species_name[Autonyms$species_name == 'Campanula gieseckiana'] <- 'Campanula giesekiana'
#Autonyms$species_name[Autonyms$species_name == 'Oreomecon radicatum'] <- 'Oreomecon radicata'






#Now re-run the code with spellings corrected and checked by an expert  

#Run search including progress bar, as the search time can be long. 
#Calculate the total number of iterations based on the length of the species list being used as input for searching 
{
total_iterations <- length(unique(Autonyms$species_name))

#Species search progress bar object
KEWR_Progress_Bar <- progress_bar$new(
  format = "  Processing [:bar] :percent :current/:total Species Names Searched in KEWR :elapsed",
  total = total_iterations,  #Total number of iterations
  width = 60,                
  clear = FALSE              
)

df_Autonyms1 = data.frame() #for accepted species 
df_Autonyms2 = data.frame() #for synonyms, which have 2 or more rows because one row is the accepted name for that given synonym
df_Autonyms3 = data.frame() #for ones not matching to POWO - these are the ones needing manual editing 
Temp2 <- data.frame(accepted=logical(),
                    author=character(),
                    kingdom=character(),
                    family=character(),
                    name=character(),
                    rank=character(),
                    url=character(),
                    fqId=character(),
                    images=numeric(),
                    synonymOf=numeric(),
                    snippet=numeric())
}
#Now using the search_powo of the Kewr package, I use each species name in my list and search the KEW database. 
for (sp in unique(Autonyms$species_name)) {                                                     #for each sp species in my list search KEW. 
  Temp1 <- search_powo(sp,)                                                                  #Search KEW and store information 
  if (Temp1$total==0) {
    df_Autonyms3 <- rbind(df_Autonyms3,Temp1$query)  
  } else {
    Temp2 <- tidy(Temp1)                                                                     #Tidy information 
  }
  x <- c("accepted","author","kingdom","family","name","rank", "url","fqId","images", "synonymOf", "snippet")          #Create category images etc , which some outputs do contain
  Temp2[x[!(x %in% colnames(Temp2))]] = 0                                                    #If a Temp2 query doesn't have this column, then add it in with a value of 0, to allow rbinding later. 
  
  if (nrow(Temp2)>=2) {                                                                      #If there are 2 or more rows, store in df_Autonyms2 as this is a synonym species. 
    df_Autonyms2 <- rbind(df_Autonyms2,Temp2)  
  } else {
    df_Autonyms1 <- rbind(df_Autonyms1,Temp2)                                                      #If there is only one row, store in df_Autonyms1 as this is already an accepted name
  }
  KEWR_Progress_Bar$tick() #Progress Bar
}
{
  df_Autonyms1 <- df_Autonyms1 %>% #Remove duplicates 
    distinct()
  
  df_Autonyms2 <- df_Autonyms2 %>% #Remove duplicates 
    distinct()
  
  df_Autonyms3 <- df_Autonyms3 %>% #Remove duplicates 
    distinct()
}

if (nrow(df_Autonyms3) == 0) {
  cat("All species names corrected for spelling. Please proceed.\n")
} else {
  cat("Remaining names here will be sent to manual processing.\n")
}

#Now need to account for autonym names for which there are multiple options as to the species it can apply to.
#(e.g. Calamagrostis stricta subsp. stricta could be the autonym of Calamagrostis stricta (Timm) Koeler or Calamagrostis stricta Hegetschw. 

#These cases need to be sorted out, for they require additional taxonomic checks 
#They are found in df_Autonyms2, in cases where there are more than one TRUE accepted name. 
{
#Need to consider two things. First, cases where the autonym name needs to be changed to the proper accepted name, such as Chamaenerion angustifolium
#to Epilobium angustifolium, and Second, cases where there is more than one accepted name, meaning it is not known for sure which accepted name the autonym belongs to.
  
df_Autonyms2_Accepted_Names<-df_Autonyms2 %>%
    filter(accepted == FALSE) %>%
    mutate(accepted_name = map_chr(synonymOf, "name")) %>%  #Extract accepted names
    select(original_name = name, accepted_name)  
  
  
df_Autonyms2_Accepted_Names <- df_Autonyms2_Accepted_Names %>%
    filter(!original_name %in% df_Autonyms2$name[ df_Autonyms2$accepted == TRUE ])
  
df_Autonyms2<-df_Autonyms2 %>%
  group_by(name) %>%
  filter(sum(accepted) > 1) %>%
  ungroup() %>% #Keep cases where there is more than one accepted name, meaning it is not known for sure which accepted name the autonym belongs to. 
  distinct(name)

#Find cases where the autonym parent name is a synonym, and that synonym could belong to more than one accepted name. 
df_Autonyms2_Accepted_Names_Multiple <- df_Autonyms2_Accepted_Names %>%
  group_by(original_name) %>%                      #Group by original_name
  filter(n() > 1) %>%                              #Filter to keep only those occurring more than once
  ungroup()  %>%
  select(original_name) %>%
  distinct()

Leftover_autonyms <- Autonyms %>%
  filter(species_name %in% df_Autonyms2$name | species_name %in% df_Autonyms2_Accepted_Names_Multiple$original_name) %>%
  select(original_input_name)  %>%
  filter(!str_detect(original_input_name, " x | × "))

#Remove these names from remaining data 
df_Autonyms2_Accepted_Names<-df_Autonyms2_Accepted_Names%>%
  filter(!original_name %in% df_Autonyms2_Accepted_Names_Multiple$original_name)

Autonyms<-Autonyms%>%
  filter(!original_input_name %in% Leftover_autonyms$original_input_name)

save.image(file = "Taxa_List_Save_11.5.RData") #Save everything 
#load("Taxa_List_Save_11.5.RData")

#There will be some cases now where the autonym is a synonym of a species that is at a lower 
#resolution that simply species. 

#Therefore, process it into the Leftovers for manual editing into the correct infraspecific species rank.

df_Autonyms2_Accepted_Names_Leftover <- df_Autonyms2_Accepted_Names %>%
  filter(str_count(accepted_name, "\\S+") > 2) %>%
  select(original_name)

#Format for integration into leftovers 
if (nrow(df_Autonyms3) > 0) {
  colnames(df_Autonyms3)[1] <- "original_input_name"
  df_Autonyms2_Accepted_Names_Leftover <- bind_rows(df_Autonyms2_Accepted_Names_Leftover, df_Autonyms3)
}

df_Autonyms2_Accepted_Names_Leftover <- Autonyms %>%
  filter(species_name %in% df_Autonyms2_Accepted_Names_Leftover$original_name) %>%
  select(name, original_input_name) %>%
  mutate(Manually_Edited_Name=NA)

df_Autonyms2_Accepted_Names<-df_Autonyms2_Accepted_Names%>%
  filter(!original_name %in% df_Autonyms2_Accepted_Names_Leftover$original_input_name)

#Also remove from Autonyms
Autonyms<-Autonyms%>%
  filter(!original_input_name %in% df_Autonyms2_Accepted_Names_Leftover$original_input_name)

#Document Results
Documentation_Results$Ten<-nrow(Leftover_autonyms)

Leftover_autonyms_Publish<-Leftover_autonyms
Leftover_autonyms_Publish$Verified_Name<-Leftover_autonyms_Publish$original_input_name
Leftover_autonyms_Publish$Status<-"Multiple_Author_Autonym"

#Final editing of Rank Data:
Leftover_autonyms_Publish <- Leftover_autonyms_Publish %>%
  mutate(rank = NA_character_) %>% 
  mutate(rank = case_when(
    str_detect(rank, "^[A-Z]+$") ~ NA_character_,  #Replace all-uppercase taxonRanks with NA
    TRUE ~ rank
  )) %>%
  mutate(rank = case_when(
    str_detect(Verified_Name, "× ") ~ "Hybrid",
    str_detect(Verified_Name, "^X ") ~ "Hybrid",
    str_detect(Verified_Name, " [×xX] ") ~ "Hybrid",
    str_detect(Verified_Name, " subsp\\. ") ~ "Subspecies",
    str_detect(Verified_Name, " var\\. ") ~ "Variety",
    str_detect(Verified_Name, " subvar\\. ") ~ "Subvariety",
    str_detect(Verified_Name, " morph") ~ "Morph",
    str_detect(Verified_Name, " f\\. ") ~ "Form",
    str_detect(Verified_Name, " subf\\. ") ~ "Subform",
    str_detect(Verified_Name, " agg\\. ") ~ "Aggregate",
    str_detect(Verified_Name, " aggregate") ~ "Aggregate",
    str_detect(Verified_Name, " sect\\. ") ~ "Section",
    str_detect(Verified_Name, " subsect\\. ") ~ "Subsection",
    str_detect(Verified_Name, " ser\\. ") ~ "Series",
    str_detect(Verified_Name, " subser\\. ") ~ "Subseries",
    str_detect(Verified_Name, " tr\\. ") ~ "Tribe",
    str_detect(Verified_Name, " subtrib\\. ") ~ "Subtribe",
    str_detect(Verified_Name, " gen\\. ") ~ "Genus",
    str_detect(Verified_Name, " subg\\. ") ~ "Subgenus",
    str_detect(Verified_Name, " fam\\. ") ~ "Family",
    str_detect(Verified_Name, "^[A-Z][a-zà-öø-ÿ]+eae\\b") ~ "Family", #If the first word ends with eae
    str_detect(Verified_Name, " subfam\\. ") ~ "Subfamily",
    str_detect(Verified_Name, " ord\\. ") ~ "Order",
    str_detect(Verified_Name, " subord\\. ") ~ "Suborder",
    str_detect(Verified_Name, " cl\\. ") ~ "Class",
    str_detect(Verified_Name, " subcl\\. ") ~ "Subclass",
    TRUE ~ rank  #Keep remaining ranks unchanged
  )) %>%
  #Assign rank for cases where it is still NA
  mutate(rank = case_when(
    is.na(rank) & str_count(Verified_Name, "\\s") >= 1 & 
      str_detect(Verified_Name, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species",  
    is.na(rank) & str_count(Verified_Name, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ rank  
  ))


#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("Multiple_Author_Autonyms_", today_date, ".xlsx")

#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)

#Write to Excel file
write_xlsx(Leftover_autonyms, file_name) #This is an Excel file with names which you did make edits for confidently. This just keeps a record of those edits. 

#Set the working directory back to the original directory
setwd("..")

#For the remaining autonyms, they can be added back into the Confirmed_Master_List
#First update the spelling 

#Add in the correct Accepted Species name for autonyms 
#which are synonyms of only one accepted name (df_Autonyms2_Accepted_Names)

Autonyms <- Autonyms %>%
  left_join(df_Autonyms2_Accepted_Names %>% select(original_name, accepted_name), 
            by = c("species_name" = "original_name")) %>%
  mutate(species_name = if_else(!is.na(accepted_name), accepted_name, species_name)) %>%
  select(-accepted_name) 


Autonyms <- Autonyms %>%
  mutate(infraspecific_rank = sapply(strsplit(name, "\\s+"), function(words) {
      #Find word ending with a period
      words[grep("\\.$", words)]
    })
  )

#Now there will be cases of autonyms which are hybrids, such as "Potentilla anserina subsp. anserina x anserina subsp. groenlandica L. x Tratt."
#In most cases, these are specimens with some sort of error attached to them, or in the very least, they need to be further investigated. 
#Therefore we remove them here and place them back into the Leftovers category. We can spot them by the infraspecific_rank starting with c("
}
{
suppressWarnings({
Autonyms_To_Leftovers <- Autonyms %>%
  filter(str_starts(infraspecific_rank, "c\\(")) %>%
  select(name, original_input_name) %>%
  mutate(Manually_Edited_Name=NA)})

suppressWarnings({  
Autonyms <- Autonyms %>%
  filter(!str_starts(infraspecific_rank, "c\\("))})

#Re-create the autonym name using corrected spelling 
Autonyms <- Autonyms %>%
  #Extract the specific epithet from species_name
  mutate(
    specific_epithet = sapply(strsplit(species_name, " "), `[`, 2), #Go into species name and take second word (specific epithet)
    #Combine infraspecific_rank with the specific epithet
    combined_rank_epithet = paste(infraspecific_rank, specific_epithet),
    #Append combined_rank_epithet to the end of species_name
    Manually_Edited_Name = paste(species_name, combined_rank_epithet)
  )

#Now Manually_Edited_Name has the properly spelled autonym, and original_input_name has the original spelling. 
#Now this can be processed for adding back into the Confirmed_Master_List
Autonyms <- Autonyms %>%
  rename(Accepted_Name_with_Author = Manually_Edited_Name,
         Synonym_1 = original_input_name) %>%
  mutate(synonym = FALSE,
         rank = NA, 
         genus = sapply(strsplit(species_name, " "), `[`, 1),
         species = sapply(strsplit(species_name, " "), `[`, 2)
  )
  
#Add in rank information 
Autonyms <- Autonyms %>%
  mutate(
    rank = case_when(
      infraspecific_rank == "subsp." ~ "SUBSPECIES",
      infraspecific_rank == "var." ~ "VARIETY",
      infraspecific_rank == "f." ~ "FORM",
      TRUE ~ NA_character_  #For cases not matching any of the above
    ))

Autonyms <- Autonyms %>% 
  select(Accepted_Name_with_Author, Synonym_1, synonym, rank, genus, species)

Autonyms$`Accepted Name`<-Autonyms$Accepted_Name_with_Author
}
{
Autonyms_processed <- Autonyms %>%
  group_by(Accepted_Name_with_Author) %>%
  summarize(synonyms = list(na.omit(unique(c(Synonym_1, synonym))))) %>%
  ungroup() %>%
  unnest_wider(synonyms, names_sep = "_", names_repair = "unique") %>%
  rename_with(~ paste0("Synonym_", seq_along(.)), starts_with("synonyms_")) %>%
  mutate(across(starts_with("Synonym_"), ~ ifelse(. == "FALSE" | . == "", NA, .))) %>%
  select(Accepted_Name_with_Author, where(~ !all(is.na(.))))

Autonyms_final <- Autonyms %>%
  select(-c(Synonym_1, synonym)) %>%  
  distinct() %>%                     
  left_join(Autonyms_processed, by = "Accepted_Name_with_Author")

#Filter out cases where the corrected name for the autonym is already in the Confirmed_Master_List
Autonyms_final_ConfirmList<-Autonyms_final%>%
  filter(Accepted_Name_with_Author %in% Confirmed_Master_List$Accepted_Name_with_Author)

Autonyms_final<-Autonyms_final%>%
  filter(!Accepted_Name_with_Author %in% Autonyms_final_ConfirmList$Accepted_Name_with_Author)

#Join Autonyms_final_ConfirmList into Confirmed_Master_List
Autonyms_final_ConfirmList <- Autonyms_final_ConfirmList %>%
  select(Accepted_Name_with_Author, starts_with("Synonym_"))

#Find the highest existing Additional_Input_Namex in Confirmed_Master_List
existing_names <- colnames(Confirmed_Master_List)
last_synonym_number <- max(as.numeric(gsub("Additional_Input_Name", "", grep("Additional_Input_Name", existing_names, value = TRUE))), na.rm = TRUE)

#Rename columns starting with "Synonym_" in Autonyms_final_ConfirmList
Autonyms_final_ConfirmList <- Autonyms_final_ConfirmList %>%
  rename_with(~ paste0("Additional_Input_Name", seq(last_synonym_number + 1, length.out = sum(grepl("^Synonym_", .)))), starts_with("Synonym_"))

#Left join into Confirmed_Master_List
Confirmed_Master_List<-left_join(Confirmed_Master_List,Autonyms_final_ConfirmList, by="Accepted_Name_with_Author")

Autonyms<-Autonyms_final

suppressPackageStartupMessages(library(plyr))
#Now I can these back into my master list: 
Confirmed_Master_List<-rbind.fill(Confirmed_Master_List,Autonyms)
detach(package:plyr)
library("dplyr")
}
{
Confirmed_Master_List <- Confirmed_Master_List %>%
  mutate(Autonym = sapply(`Accepted Name`, search_autonym))  #Apply detect_autonym to Accepted Name in the confirmed master list 
  
#I will reorder the columns to make it easier to see, but keeping the synonyms at the end since there are so many
Confirmed_Master_List <- Confirmed_Master_List %>%
  select("Accepted Name", "Accepted_Name_with_Author", "synonym", "KEW ID", "taxonomicStatus", "kingdom", "phylum", "family", "genus", "species", "rank", "plantae", "fungi", "hybrid", "namePublishedInYear", "authors", "reference", "nomenclaturalCode", "nomenclaturalStatus", "lifeform", "climate", "taxonRemarks", "locations", "Autonym", everything())

#Now the Confirmed_Master_List contains all reliable autonyms, and all autonyms are marked as TRUE in the Autonym column. 
#Now the remaining leftover names can be processed: 

#Add the Autonyms_To_Leftovers back into the leftovers along with the df_Autonyms2_Accepted_Names_Leftover
Leftovers<-rbind(Leftovers, Autonyms_To_Leftovers,df_Autonyms2_Accepted_Names_Leftover)

#Pre-label removed unplaced names (removed because there is an Accepted match at the same taxonomic level, such as the unplaced Potentilla nivea Gunnerus and the Accepted Potentilla nivea L)
Unplaced_Remaining<-Unplaced%>%
  filter(!Accepted_Name_with_Author %in% Unplaced_To_ReAdd$Accepted_Name_with_Author)

Leftovers <- Leftovers %>%
  mutate(Manually_Edited_Name = if_else(original_input_name %in% Unplaced_Remaining$Accepted_Name_with_Author, "Unplaced_Removed", Manually_Edited_Name))

#Remove as output
Unplaced_Removed_Publish <- Leftovers %>%
  filter(Manually_Edited_Name == "Unplaced_Removed")
#Remove from Leftovers
Leftovers <- Leftovers %>%
  filter(is.na(Manually_Edited_Name))

#Manually Add in New Names:
cat("Leftovers$Manually_Edited_Name[Leftovers$original_input_name == 'original input name'] <- 'manually entered name' - Use this line to manually edit the names of taxa on the leftover list.")

cat("Use the code below to generate the lines of code to paste back into this R script for your specific flora. Run the line below so it prints the pre-generated
lines to the R Console, and paste it into the script. Then for each original input name, check associated GBIF records to determine if the name can be corrected. 
When it can be corrected, please replaced Write_Corrected_Name_Here with the full name according to POWO, including author. 
When it cannot be corrected, please replace Write_Corrected_Name_Here with Unknown.") 
}
cat(paste("Leftovers$Manually_Edited_Name[Leftovers$original_input_name == '", Leftovers$original_input_name, "'] <- 'Write_Corrected_Name_Here'\n", sep = ""), sep = "\n")

#Paste the output of this code below. Then manually edit the 'Write_Corrected_Name_Here' text with the specific correct accepted name for the given original name. 
#If you cannot find it, then label 'Write_Corrected_Name_Here' as 'Unknown'. If you can, add a # to the right of the line and annotate with more details to track decision making. 

#I elected to organize my edits by 6 categories:
  #   (1) Names with minor differences in spelling to the species name, which could be corrected:
  #   (2) Names with minor differences in spelling to the author, which could be corrected. 
  #   (3) Names where the specimens' labels differed than the GBIF interpreted scientific name, and this name is recognized by POWO, which could be corrected.
  #   (4) Names which were not recognized by POWO, but which may be specific to the location of query and thus need to be checked in another more localized flora source
  #   (5)	Genera rank or higher rank names which were not recognized by POWO, but which may be specific to the location of query and thus need to be checked in another more localized flora source. 
  #   (6) Names which were incorrectly transcribed, but which are still not recognized by POWO after being corrected 

#Examples
#Leftovers$Manually_Edited_Name[Leftovers$original_input_name == 'Triglochin palustre L.'] <- "Triglochin palustris L." #Same spelling on herbarium sheet. Alternate spelling. 
#Leftovers$Manually_Edited_Name[Leftovers$original_input_name == 'Antennaria glabrata (J.Vahl) Greene'] <- "Antennaria glabrata Greene"  
#Leftovers$Manually_Edited_Name[Leftovers$original_input_name == 'Festuca vivipara var. vivipara'] <- "Festuca vivipara subsp. vivipara" #Herbarium Sheet is actually listed as  "Festuca vivipara ssp. vivipara"
#Leftovers$Manually_Edited_Name[Leftovers$original_input_name == 'Antennaria canescens f. canescens'] <- "Unknown" #No form of this kind in KEW POWO database. 
#Leftovers$Manually_Edited_Name[Leftovers$original_input_name == 'Ranunculus instar subsp. callianthus'] <- 'Unknown' #Same spelling on herbarium sheet - all are cases of one person making change in 1982 and not registering to IPNI. 

{
  
  #Add in here the code printed, and correct the names when possible. 


       }
{
  Leftovers$Manually_Edited_Name <- trimws(Leftovers$Manually_Edited_Name)
  
#Edit the Genus_Compare_Confident to not contain any manually edited Higher Taxon names which were corrected
#to something other than Genera level, such as Downingia being corrected to Downingia laeta (Greene) Greene and not Downingia Torr.
#However sometimes these cases cannot be avoided because of how the data was entered into GBIF, and would require manually checking each specimen, which cannot be done.. 
Genus_Compare_Confident <- Genus_Compare_Confident %>% filter(!name %in% Leftovers$name) #Should do nothing 

Leftovers<-rbind(Leftovers,Genus_Compare_Confident) #Add back in 

#Find any new cases of Unplaced_Removed
Leftovers <- Leftovers %>%
  mutate(Manually_Edited_Name = if_else(Manually_Edited_Name %in% Unplaced_Remaining$Accepted_Name_with_Author, "Unplaced_Removed", Manually_Edited_Name))

#Remove as output again if needed 
Unplaced_Removed_add <- Leftovers %>%
  filter(Manually_Edited_Name == "Unplaced_Removed")
#Remove from Leftovers
Leftovers <- Leftovers %>%
  filter(Manually_Edited_Name != "Unplaced_Removed")

if (nrow(Unplaced_Removed_add) > 0) {
  Unplaced_Removed_Publish <- rbind(Unplaced_Removed_Publish, Unplaced_Removed_add)
}

#Prepare for later integration into final removed list 
Unplaced_Removed_Publish <- Unplaced_Removed_Publish %>%
  rename(Status = Manually_Edited_Name) %>%
  mutate(
    Verified_Name = original_input_name,
    rank = NA,
    Comment = "Unplaced name removed because another Accepted Name at same taxonomic level available"
  ) %>%
  select(-name)

Unplaced_Removed_Publish <- Unplaced_Removed_Publish %>%
  mutate(rank = NA_character_) %>% 
  mutate(rank = case_when(
    str_detect(rank, "^[A-Z]+$") ~ NA_character_,  #Replace all-uppercase taxonRanks with NA
    TRUE ~ rank
  )) %>%
  mutate(rank = case_when(
    str_detect(Verified_Name, "× ") ~ "Hybrid",
    str_detect(Verified_Name, "^X ") ~ "Hybrid",
    str_detect(Verified_Name, " [×xX] ") ~ "Hybrid",
    str_detect(Verified_Name, " subsp\\. ") ~ "Subspecies",
    str_detect(Verified_Name, " var\\. ") ~ "Variety",
    str_detect(Verified_Name, " subvar\\. ") ~ "Subvariety",
    str_detect(Verified_Name, " morph") ~ "Morph",
    str_detect(Verified_Name, " f\\. ") ~ "Form",
    str_detect(Verified_Name, " subf\\. ") ~ "Subform",
    str_detect(Verified_Name, " agg\\. ") ~ "Aggregate",
    str_detect(Verified_Name, " aggregate") ~ "Aggregate",
    str_detect(Verified_Name, " sect\\. ") ~ "Section",
    str_detect(Verified_Name, " subsect\\. ") ~ "Subsection",
    str_detect(Verified_Name, " ser\\. ") ~ "Series",
    str_detect(Verified_Name, " subser\\. ") ~ "Subseries",
    str_detect(Verified_Name, " tr\\. ") ~ "Tribe",
    str_detect(Verified_Name, " subtrib\\. ") ~ "Subtribe",
    str_detect(Verified_Name, " gen\\. ") ~ "Genus",
    str_detect(Verified_Name, " subg\\. ") ~ "Subgenus",
    str_detect(Verified_Name, " fam\\. ") ~ "Family",
    str_detect(Verified_Name, "^[A-Z][a-zà-öø-ÿ]+eae\\b") ~ "Family", #If the first word ends with eae
    str_detect(Verified_Name, " subfam\\. ") ~ "Subfamily",
    str_detect(Verified_Name, " ord\\. ") ~ "Order",
    str_detect(Verified_Name, " subord\\. ") ~ "Suborder",
    str_detect(Verified_Name, " cl\\. ") ~ "Class",
    str_detect(Verified_Name, " subcl\\. ") ~ "Subclass",
    TRUE ~ rank  #Keep remaining ranks unchanged
  )) %>%
  #Assign rank for cases where it is still NA
  mutate(rank = case_when(
    is.na(rank) & str_count(Verified_Name, "\\s") >= 1 & 
      str_detect(Verified_Name, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species",  
    is.na(rank) & str_count(Verified_Name, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ rank  
  ))

#Added in to remove white spaces for unique cases of names starting with hybrid x   
Leftovers_Confidently_Edited <- Leftovers %>%
  filter(str_trim(Manually_Edited_Name) != "Unknown", 
         str_trim(Manually_Edited_Name) != str_trim(original_input_name))

#Document Results
Documentation_Results$Eleven<-nrow(Leftovers_Confidently_Edited)

#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("Record_of_Corrected_Leftover_Names_", today_date, ".xlsx")

#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)

#Write to Excel file
write_xlsx(Leftovers_Confidently_Edited, file_name) #This is an Excel file with names which you did make edits for confidently. This just keeps a record of those edits. 

#Set the working directory back to the original directory
setwd("..")

save.image(file = "Taxa_List_Save_12.RData") #Save everything 
#load("Taxa_List_Save_12.RData")

}
#Now the ones that have been manually edited can be (1) checked to see if they already match to a hit in our list, and then (2) see if they can now be found by POWO. 
{
Leftovers_Wide<-Leftovers %>%
  select (-c(name)) %>%
  filter(!grepl('Unknown', Manually_Edited_Name)) %>%
  drop_na(Manually_Edited_Name) 

#First I pivot the data into longer format, and then I Dcast it:
Leftovers_Wide<-Leftovers_Wide %>% 
  pivot_longer(cols = -c(Manually_Edited_Name), values_to = "original_input_name") %>%                                          
  select(-c(name))                                                   
                                           
Leftovers_Wide<-dcast(setDT(Leftovers_Wide), Manually_Edited_Name ~ rowid(Manually_Edited_Name), value.var = "original_input_name")
colnames(Leftovers_Wide)[1] <- "Accepted_Name_with_Author" 
Project_Leftovers_Wide<-Leftovers_Wide #Store for later use 

Final_Leftovers <- Leftovers %>%
  filter(!Leftovers$Manually_Edited_Name %in% Leftovers_Wide$Accepted_Name_with_Author) #final List of Leftovers. This dataframe may include Genera and other higher ranks. 

#Document Results
Documentation_Results$Twelve<-nrow(Final_Leftovers)
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Checking against the Confirmed_Master_List 

#With the manually edited names, some of these new names may now match against the Confirmed_Master_List.
#For example, the leftover "Achillea millefolium var. millefolium" was manually confirmed to be "Achillea millefolium subsp. millefolium"
#and Achillea millefolium subsp. millefolium is already in the Confirmed_Master_List. 
{
#Prepare to join to the Master List - Pivot it long by Accepted Name and with synonyms
Confirmed_Master_List_Mini_LeftoverCheck <- Confirmed_Master_List %>%
  select(Accepted_Name_with_Author, starts_with("Synonym_"), starts_with("Additional_"), starts_with("Other_")) %>%
  pivot_longer(
    cols = starts_with("Synonym_") | starts_with("Additional_") | starts_with("Other_"),  
    names_to = "Column_Type",      
    values_to = "Synonym") %>%
  filter(!is.na(Synonym)) %>%
  select(-Column_Type)

Missing_Accepted_Names <- Confirmed_Master_List %>%
    select(Accepted_Name_with_Author) %>%
    filter(!Accepted_Name_with_Author %in% Confirmed_Master_List_Mini_LeftoverCheck$Accepted_Name_with_Author) %>%
    mutate(Synonym = Accepted_Name_with_Author)
  
#Combine both datasets
  if (nrow(Missing_Accepted_Names) > 0) {
    Confirmed_Master_List_Mini_LeftoverCheck <- bind_rows(Confirmed_Master_List_Mini_LeftoverCheck, Missing_Accepted_Names)  
  }  

Confirmed_Master_List_Mini_LeftoverCheck <- Confirmed_Master_List_Mini_LeftoverCheck %>%
  bind_rows(Confirmed_Master_List_Mini_LeftoverCheck %>% 
              mutate(Synonym = Accepted_Name_with_Author) %>% 
              select(Accepted_Name_with_Author, Synonym)) %>%
  distinct() #Add Accepted Name into Synonym row for matching 

#Trim white spaces as a precaution 
Confirmed_Master_List_Mini_LeftoverCheck$Accepted_Name_with_Author <- str_trim(Confirmed_Master_List_Mini_LeftoverCheck$Accepted_Name_with_Author)
Confirmed_Master_List_Mini_LeftoverCheck$Synonym <- str_trim(Confirmed_Master_List_Mini_LeftoverCheck$Synonym)
Leftovers_Wide$Accepted_Name_with_Author <- str_trim(Leftovers_Wide$Accepted_Name_with_Author)
Leftovers_Wide_Check<-Leftovers_Wide 
colnames(Leftovers_Wide_Check) <- c("Accepted_Name_with_Author", paste0("Leftover_", seq_along(Leftovers_Wide_Check)[-1]))

Confirmed_Master_List_Mini_LeftoverCheck <- Confirmed_Master_List_Mini_LeftoverCheck %>%
  left_join(Leftovers_Wide_Check, by = c("Synonym" = "Accepted_Name_with_Author")) %>%
  filter(!if_all(starts_with("Leftover_"), is.na)) 

#Document Results
Documentation_Results$Total_Leftovers_Wide <- nrow(Leftovers_Wide) #Total number of Leftovers which were edited and matched to an Accepted Name. N.B. This is total number of matching Accepted Names. 

#Remove matched leftovers from Leftovers Wide
Leftovers_Wide<- Leftovers_Wide %>%
  filter(!Accepted_Name_with_Author %in% Confirmed_Master_List_Mini_LeftoverCheck$Synonym) #Total number of Leftovers which were edited and matched to an Accepted Name, and which not matched to existing names in Confirmed Master List. N.B. This is total number of matching Accepted Names. 

#Document Results
Documentation_Results$Leftovers_Wide_AfterCompareCMasterList <- nrow(Leftovers_Wide)

Confirmed_Master_List_Mini_LeftoverCheck <- Confirmed_Master_List_Mini_LeftoverCheck %>%
  select(-Synonym)

#Pivot again for many names will match to synonyms of one Accepted Name
Confirmed_Master_List_Mini_LeftoverCheck <- Confirmed_Master_List_Mini_LeftoverCheck %>%
  select(Accepted_Name_with_Author, starts_with("Leftover_")) %>%
  pivot_longer(
    cols = starts_with("Leftover_"),  
    names_to = "Column_Type",      
    values_to = "Leftover") %>%
  filter(!is.na(Leftover)) %>%
  select(-Column_Type)

Confirmed_Master_List_Mini_LeftoverCheck_Names<- names(Confirmed_Master_List_Mini_LeftoverCheck)[-1] #Take all column names except first one, for use in Dcast 
Confirmed_Master_List_Mini_LeftoverCheck<- dcast(setDT(Confirmed_Master_List_Mini_LeftoverCheck), Accepted_Name_with_Author ~ rowid(Accepted_Name_with_Author), value.var = "Leftover")
Confirmed_Master_List_Mini_LeftoverCheck<-Confirmed_Master_List_Mini_LeftoverCheck %>% drop_na(Accepted_Name_with_Author)
#Now shift all names to the left to fill in NA cells (tidy it up)
Confirmed_Master_List_Mini_LeftoverCheck <- as.data.frame(t(apply(Confirmed_Master_List_Mini_LeftoverCheck, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
colnames(Confirmed_Master_List_Mini_LeftoverCheck)[1] <- "Accepted_Name_with_Author"
colnames(Confirmed_Master_List_Mini_LeftoverCheck) <- sub("V", "", colnames(Confirmed_Master_List_Mini_LeftoverCheck))
Confirmed_Master_List_Mini_LeftoverCheck<-Confirmed_Master_List_Mini_LeftoverCheck %>% 
  rename_at(vars(2:ncol(Confirmed_Master_List_Mini_LeftoverCheck)), ~paste0('Manually_Added_Input_Name',.)) %>% #Rename Input Name Columns 
  select(where(~!all(is.na(.x)))) #Remove for example any columns where all rows are NA.

#Left join them into the master list since they have known matches to the Accepted_Name_with_Author
Confirmed_Master_List<-left_join(Confirmed_Master_List, Confirmed_Master_List_Mini_LeftoverCheck, by="Accepted_Name_with_Author")
}
#Now a lot of the leftover names have been added into the Confirmed_Master_List without needing to re-run a search for them in POWO. 

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Checking against the Removed_Master_List to reduce computation time. 

#Using Leftovers_Wide, I can first check if any of these names were in the Removed_Master_List:
#In theory, these can simply be re-extracted and added back into the Confirmed_Master_List, 
#since the Confirmed_Master_List was made including the removed ones, and so the removed ones
#would not be considered synonyms of the species remaining within the Confirmed_Master_List. 
{  
#Prepare to join to the Master List - Pivot it long by Accepted Name and with synonyms
Removed_Master_List_Mini_Leftovers <- Removed_Master_List %>%
  select(Accepted_Name_with_Author, starts_with("Synonym_"), starts_with("Additional_"), starts_with("Other_")) %>%
  pivot_longer(
    cols = starts_with("Synonym_") | starts_with("Additional_") | starts_with("Other_"),  
    names_to = "Column_Type",      
    values_to = "Synonym") %>%
  filter(!is.na(Synonym)) %>%
  select(-Column_Type)
  
Missing_Accepted_Names <- Removed_Master_List %>%
    select(Accepted_Name_with_Author) %>%
    filter(!Accepted_Name_with_Author %in% Removed_Master_List_Mini_Leftovers$Accepted_Name_with_Author) %>%
    mutate(Synonym = Accepted_Name_with_Author)
  
#Combine both datasets
if (nrow(Missing_Accepted_Names) > 0) {
Removed_Master_List_Mini_Leftovers <- bind_rows(Removed_Master_List_Mini_Leftovers, Missing_Accepted_Names)  
}

Removed_Master_List_Mini_Leftovers <- Removed_Master_List_Mini_Leftovers %>%
  bind_rows(Removed_Master_List_Mini_Leftovers %>% 
              mutate(Synonym = Accepted_Name_with_Author) %>% 
              select(Accepted_Name_with_Author, Synonym)) %>%
  distinct() #Add Accepted Name into Synonym row for matching 

#Trim white spaces as a precaution 
Removed_Master_List_Mini_Leftovers$Accepted_Name_with_Author <- str_trim(Removed_Master_List_Mini_Leftovers$Accepted_Name_with_Author)
Removed_Master_List_Mini_Leftovers$Synonym <- str_trim(Removed_Master_List_Mini_Leftovers$Synonym)
Leftovers_Wide$Accepted_Name_with_Author <- str_trim(Leftovers_Wide$Accepted_Name_with_Author)

Leftovers_Wide1<-Leftovers_Wide 
colnames(Leftovers_Wide1) <- c("Accepted_Name_with_Author", paste0("Leftover_", seq_along(Leftovers_Wide1)[-1]))

Removed_Master_List_Mini_Leftovers <- Removed_Master_List_Mini_Leftovers %>%
  left_join(Leftovers_Wide1, by = c("Synonym" = "Accepted_Name_with_Author")) %>%
  filter(!if_all(starts_with("Leftover_"), is.na)) 

#Remove matched leftovers from Leftovers Wide
Leftovers_Wide<- Leftovers_Wide %>%
  filter(!Accepted_Name_with_Author %in% Removed_Master_List_Mini_Leftovers$Synonym)

#Document Results
Documentation_Results$Total_Leftovers_Wide_AfterRemovedList <- nrow(Leftovers_Wide)  
  
#Pivot again for many names will match to synonyms of one Accepted Name
Removed_Master_List_Mini_Leftovers <- Removed_Master_List_Mini_Leftovers %>%
  select(Accepted_Name_with_Author, starts_with("Leftover_")) %>%
  pivot_longer(
    cols = starts_with("Leftover_"),  
    names_to = "Column_Type",      
    values_to = "Leftover") %>%
  filter(!is.na(Leftover)) %>%
  select(-Column_Type)

Removed_Master_List_Mini_Leftovers_Names<- names(Removed_Master_List_Mini_Leftovers)[-1] #Take all column names except first one, for use in Dcast 
Removed_Master_List_Mini_Leftovers<- dcast(setDT(Removed_Master_List_Mini_Leftovers), Accepted_Name_with_Author ~ rowid(Accepted_Name_with_Author), value.var = "Leftover")
Removed_Master_List_Mini_Leftovers<-Removed_Master_List_Mini_Leftovers %>% drop_na(Accepted_Name_with_Author)
#Now shift all names to the left to fill in NA cells (tidy it up)
Removed_Master_List_Mini_Leftovers <- as.data.frame(t(apply(Removed_Master_List_Mini_Leftovers, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
colnames(Removed_Master_List_Mini_Leftovers)[1] <- "Accepted_Name_with_Author"
colnames(Removed_Master_List_Mini_Leftovers) <- sub("V", "", colnames(Removed_Master_List_Mini_Leftovers))
Removed_Master_List_Mini_Leftovers<-Removed_Master_List_Mini_Leftovers %>% 
  rename_at(vars(2:ncol(Removed_Master_List_Mini_Leftovers)), ~paste0('Manually_Added_Input_Name',.)) %>% #Rename Input Name Columns 
  select(where(~!all(is.na(.x)))) #Remove for example any columns where all rows are NA.  
  
Removed_Master_List$Accepted_Name_with_Author <- str_trim(Removed_Master_List$Accepted_Name_with_Author)
Removed_Master_List_Mini_Leftovers$Accepted_Name_with_Author <- str_trim(Removed_Master_List_Mini_Leftovers$Accepted_Name_with_Author)

Removed_Master_List<-Removed_Master_List%>%
  filter(Accepted_Name_with_Author %in% Removed_Master_List_Mini_Leftovers$Accepted_Name_with_Author)
  
Removed_Master_List<-left_join(Removed_Master_List,Removed_Master_List_Mini_Leftovers, by="Accepted_Name_with_Author")  

suppressPackageStartupMessages(library(plyr))
#Now I can these back into my master list: 
Confirmed_Master_List<-rbind.fill(Confirmed_Master_List,Removed_Master_List)
detach(package:plyr)
library("dplyr")
}
#Now we could recycle that existing data, and further reduce the searching time for any remaining POWO names. 

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Searching KEWR for remaining species of Leftovers_Wide

{
  
Leftovers_Wide<-Leftovers_Wide%>%
  rename_at(vars(2:ncol(Leftovers_Wide)), ~paste0('Additional_Input_Name',.))

Leftovers_List_Temp = data.frame(species=NA, original_input_name=NA)                         #Create a dataframe for the edited species list 
Leftovers_List = data.frame(species=NA, original_input_name=NA)
}

for (sp in Leftovers_Wide$Accepted_Name_with_Author) {                                       #For each species in my leftovers list 
  Leftovers_List_Temp$species <-(sp)
  Leftovers_List_Temp$species <- gsub ("ssp.","subsp.", Leftovers_List_Temp$species)   #Change ssp. to subsp - subsp. is recognized by KEWR and also ssp./sp. is removed by FLORA's remove.authors function (removing subspecies information with it) - so this order prevents that 
  for (h in Leftovers_List_Temp$species) {
    Leftovers_List_Temp$species <-remove.authors(h)               #Remove any author tags for this species
  }
  Leftovers_List_Temp$original_input_name<-(sp)                   #Store original species name as found in data input file to later add as a "synonym" to the Master List 
  Leftovers_List <- rbind(Leftovers_List,Leftovers_List_Temp)  
  
}
{
Leftovers_List <- drop_na(Leftovers_List)
Leftovers_List_Storage <- Leftovers_List #Store for later use
Leftovers_List$species[str_detect(Leftovers_List$original_input_name, "sect.")] <- Leftovers_List$original_input_name[str_detect(Leftovers_List$original_input_name, "sect.")] 

#Handle hybrids 
Leftovers_List$species[str_detect(Leftovers_List$original_input_name, " x ")] <- Leftovers_List$original_input_name[str_detect(Leftovers_List$original_input_name, " x ")] #x
Leftovers_List$species[str_detect(Leftovers_List$original_input_name, " × ")] <- Leftovers_List$original_input_name[str_detect(Leftovers_List$original_input_name, " × ")] #multiplication sign 
Leftovers_List$species[str_detect(Leftovers_List$original_input_name, " ×")] <- Leftovers_List$original_input_name[str_detect(Leftovers_List$original_input_name, " ×")] 

#Handle Genera hybrids which have been cleaned before
Leftovers_List <- Leftovers_List %>%
  mutate(species = ifelse(
    species == "×" & original_input_name %in% Genus_Compare_Confident$Manually_Edited_Name, 
    Genus_Compare_Confident$name[match(original_input_name, Genus_Compare_Confident$Manually_Edited_Name)], 
    species  #Keep the original species for non-hybrids
  ))

#Process cases of "ad"
Leftovers_List <- Leftovers_List %>%
  mutate(species = if_else(str_detect(original_input_name, "\\b(ad|Ad|AD)\\.?\\b"),original_input_name,species))

#Process casses ending in an infraspecific rank designation 
Leftovers_List <- Leftovers_List %>%
  mutate(species = if_else(str_detect(original_input_name, "\\b(var\\.|subsp\\.|f\\.|agg\\.)$"),original_input_name,species))
Leftovers_List <- Leftovers_List %>%
  mutate(species = if_else(str_detect(original_input_name, "\\b(var\\,|subsp\\,|f\\,)$"),original_input_name,species))


#Clean cases where third word is capital letter (remaining author in species name)
Leftovers_List <- Leftovers_List %>%
  mutate(species = if_else(
    !str_detect(species, "\\s[x×]\\s") & 
      str_detect(sapply(str_extract_all(species, "\\S+"), function(x) if (length(x) >= 3) x[3] else ""), "^[A-Z]"),
    str_replace(species, "^((\\S+\\s+){2})\\S+", "\\1"),
    species))  

Leftovers_List <- Leftovers_List %>%
  mutate_all(~ str_trim(., side = "both"))  

Leftovers_List <- Leftovers_List %>%
  mutate(species = case_when(
    str_count(species, "\\s+") == 0 &  
      str_count(original_input_name, "\\s+") == 1 & 
      str_detect(str_extract(original_input_name, "\\S+$"), "[*?#!$%^&]$") ~ 
      original_input_name,  
    TRUE ~ species 
  ))

#Fix cases of and/or or "og" in Danish
for (i in 1:nrow(Leftovers_List)) {
  if (str_detect(Leftovers_List$original_input_name[i], "and/or| og ")) {
    Leftovers_List$species[i] <- Leftovers_List$original_input_name[i]}}

}

{
#Correct cases where the species name after the "f." for form has been removed, as it was thought to be an authority name: 

#Create a new column to store the extracted form names
Leftovers_List$form_name <- NA

#Iterate through the rows and extract the form names
for (i in 1:nrow(Leftovers_List)) {
  species_name <- Leftovers_List$species[i]
  if (grepl("f\\.$", species_name) && !grepl("\\. f\\.$", Leftovers_List$original_input_name[i])) {
    form_name <- sub("^.*f\\.\\s*(\\S+).*", "\\1", Leftovers_List$original_input_name[i])
    Leftovers_List$form_name[i] <- form_name
    Leftovers_List$species[i] <- paste0(species_name, " ", form_name)}
  if (grepl("f\\.$", Leftovers_List$original_input_name[i])) {
    Leftovers_List$form_name[i] <- NA}
  if (grepl("\\. f\\.$", Leftovers_List$original_input_name[i]) &&
      grepl(" f\\.$", Leftovers_List$species[i])) {
    Leftovers_List$species[i] <- sub(" f\\.$", "", Leftovers_List$species[i])}}

Leftovers_List <- Leftovers_List %>%
  select(species, original_input_name)

Leftovers_Cleaned<- Leftovers_List
colnames(Leftovers_Cleaned)[1] <- "species"

Leftovers_Cleaned$species <- str_trim(Leftovers_Cleaned$species)
Leftovers_Cleaned$original_input_name <- str_trim(Leftovers_Cleaned$original_input_name)

save.image(file = "Taxa_List_Save_13.RData") #Save everything 
#load("Taxa_List_Save_13.RData")
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Run initial species search in KEWR using loop and search_powo command

#Create three dataframes, one for the species with accepted names, one for species
#where it is a synonym, and one for species names not recognized by KEWR. 
#I make two DFs because the output differs based on this category. 
{
df_total12 = data.frame() #for accepted species 
df_total13 = data.frame() #for synonyms, which have 2 or more rows because one row is the accepted name for that given synonym
df_total14 = data.frame() #for ones that do not work with the R package KEWR - will include most hybrids, some varieties, and some orthographic variants 
Temp2 <- data.frame(accepted=logical(),
                    author=character(),
                    kingdom=character(),
                    family=character(),
                    name=character(),
                    rank=character(),
                    url=character(),
                    fqId=character(),
                    images=numeric(),
                    synonymOf=numeric(),
                    snippet=numeric())

#Progress Bar
total_iterations <- length(unique(Leftovers_Cleaned$species))

#Initialize the progress bar
Leftover_Progress_Bar <- progress_bar$new(
  format = "  Processing [:bar] :percent :current/:total Taxon IDs :elapsed",
  total = total_iterations,  #Total number of iterations
  width = 60,                
  clear = FALSE              
)
}
#Now using the search_powo of the Kewr package, I use each species name in my list and search the KEW database. 
for (sp in unique(Leftovers_Cleaned$species)) {                                                       #for each sp species in my list search KEW. 
  Temp1 <- search_powo(sp,)                                                                    #Search KEW and store information 
  if (Temp1$total==0) {
    df_total14 <- rbind(df_total14,Temp1$query)  
  } else {
    Temp2 <- tidy(Temp1)                                                                      #Tidy information 
  }
  x <- c("accepted","author","kingdom","family","name","rank", "url","fqId","images", "synonymOf", "snippet")          #Create category images etc , which some outputs do contain
  Temp2[x[!(x %in% colnames(Temp2))]] = 0                                                    #If a Temp2 query doesn't have this column, then add it in with a value of 0, to allow rbinding later. 
  
  if (nrow(Temp2)>=2) {                                                                      #If there are 2 or more rows, store in df_total13 as this is a synonym species. 
    df_total13 <- rbind(df_total13,Temp2)  
  } else {
    df_total12 <- rbind(df_total12,Temp2)                                                      #If there is only one row, store in df_total12 as this is already an accepted name
  }
  
  #Update the progress bar
  Leftover_Progress_Bar$tick()
}
{
df_total12 <- df_total12 %>% #Remove duplicates 
  distinct()

df_total13 <- df_total13 %>% #Remove duplicates 
  distinct()

df_total14 <- df_total14 %>% #Remove duplicates 
  distinct()

save.image(file = "Taxa_List_Save_14.RData") #Save everything 
#load("Taxa_List_Save_14.RData")
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Organizing Accepted Names and Synonym Files
{
#However, because some names were queried that are Genus level or higher, the POWO package finds many more cases
#than needed, which can dramatically increase computation time. Therefore, filter these cases:

df_total12<-df_total12 %>%
    filter(name %in% Leftovers_Cleaned$species)

df_total13<-df_total13 %>%
    filter(name %in% Leftovers_Cleaned$species)  
  
True_Names2<- df_total13 %>%
  filter(accepted == "TRUE")
df_total12 <- rbind(df_total12,True_Names2)

#Keep synonym names only 
df_total13<- df_total13 %>%
  filter(accepted == "FALSE")
#Un-nest the df to extract the synonym name and corresponding actual name of each species 
df_total13<-tidyr::unnest(df_total13, cols=synonymOf, names_sep="_")

#Within the columns there is a code, for example 124840-2. 
#This code is the Accepted Taxon ID. 
#Extract the Accepted Taxon ID from dataframes. 
df_total12.1 <- df_total12 %>%
  transmute(Accepted_Taxon_ID = fqId)  

df_total13.1 <- if("synonymOf_fqId" %in% colnames(df_total13)) {
  df_total13 %>%
    transmute(Accepted_Taxon_ID = synonymOf_fqId)
} else {
  df_total13[0, ] #Create an empty dataframe with the same structure but no rows
}

df_total<- data.frame()
df_total <- rbind(df_total12.1, df_total13.1) #List of all species present in the given species list, but with the Taxon ID for their Accepted Species Names, not synonyms 
df_total$Accepted_Taxon_ID <- gsub ("urn:lsid:ipni.org:names:","", df_total$Accepted_Taxon_ID) #remove the jargon
df_total <- df_total %>%
  distinct() #Remove duplicate numbers 

#Now search for data on all accepted species, where we also get the list of synonyms etc. 

#Some species have a Basionym and some do not, so need to add this column, even if blank, to run loop properly. 
#Define custom function to add columns to data frame if they do not exist - specifically columns for "basionymOf" and 
add_cols <- function(Temp2, cols) {
  add <- cols[!cols %in% names(Temp2)]
  if(length(add) !=0 ) Temp2[add] <- NA
  return(Temp2)
}

List1 = data.frame()

#Progress Bar
total_iterations <- length(unique(df_total$Accepted_Taxon_ID))

#Initialize the progress bar
Progress_Bar_Leftover <- progress_bar$new(
  format = "  Processing [:bar] :percent :current/:total Taxon IDs :elapsed",
  total = total_iterations,  #Total number of iterations
  width = 60,                
  clear = FALSE              
)

}
for (ac in unique(df_total$Accepted_Taxon_ID)) {   #for each ac accepted species Taxon ID in my list
  Temp1 <- lookup_powo(ac)                         #Search KEW and store information for that accepted Taxon ID
  Temp2 <- tidy(Temp1)                             #Tidy information 
  Temp2 <- add_cols(Temp2, c('basionym','basionymOf','modified','bibliographicCitation','genus','taxonomicStatus','kingdom','phylum','family','nomenclaturalCode','source','namePublishedInYear','taxonRemarks',
                             'nomenclaturalStatus','lifeform','climate', 'hybrid','childNameUsages','synonym','locations','plantae','fungi','fqId', 'name', 'authors','species','rank','reference','classification',
                             'synonyms', 'nomenclaturalRemarks','infraspecies', 'hybridFormula', "paftolId")) #If this fails, check the code of the ac that failed and see what extra columns it has, then add in here. 
  List1 <- rbind(List1,Temp2)  
  
  Progress_Bar_Leftover$tick()
}

#Now we have a list that contains all the accepted names of each species in our files, and all the synonyms and basionym - Note there are still many duplicates, but we leave here for now 
#But the synonyms and basionym are stored as lists within the dataframe. I want it all in a clean crisp dataframe for easy use later. 

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Extracting Synonyms 
{
#Wipe old DFs
Synonym_Names2<-NA
Synonym_Names<-NA
Basionyms<-NA
Basionyms2<-NA
Synonym_Names_Authors<-NA
Final_Synonym_Names<-NA

#Preparing for Master List - Extracting Synonyms 
#Synonyms are packed in a list format, so I will do a weird workaround to get to them. 
Synonym_Names2<- do.call(rbind.data.frame, List1$synonyms) #Gives synonyms in order of extracting from List1, but without corresponding names of accepted species 

Synonym_Names2<-Synonym_Names2%>%mutate(Synonym_with_Author = paste(name, author, sep = " "))      #Now I have the Synonyms with their authority names on them. 
List1<-List1%>%mutate(Accepted_Name_with_Author = paste(name, authors, sep = " "))                 #Now I have the Accepted Name with their authority names on them.
List1$Accepted_Name_with_Author<-gsub("NA","", List1$Accepted_Name_with_Author)                    #Remove NAs that get appended to Accepted_Name_with_Author is the authors category is NA
List1$Accepted_Name_with_Author <- str_trim(List1$Accepted_Name_with_Author)

Synonym_Names<- setDT(List1)[, .(Synonym = c(name, unlist(synonyms,recursive = TRUE))), by = Accepted_Name_with_Author] #This unnests the list (including matching accepted name) but does it for all list columns, and I do not want this. 
Synonym_Names<-Synonym_Names %>%                                                                   #Now I keep the urn:lsid:ipni.org:names codes as these match the Accepted Name to the Synonym 
  filter(grepl("urn:lsid:ipni.org:names",Synonym_Names$Synonym)) %>% #If this doesn't work when re-running, turn Synonym_Names$Synonym to only "Synonym"
  rename("fqId" = "Synonym")

Synonym_Names<-left_join(Synonym_Names, Synonym_Names2, by="fqId")                                 #Now I join the Synonym to the Accepted Name 
Synonym_Names<-Synonym_Names%>%
  transmute(Accepted_Name_with_Author=Accepted_Name_with_Author,
            Synonym_Names_Authors = Synonym_with_Author)

#Repeat for Basionyms - likely already in the synonyms for each species, but will do again for due diligence 
Basionyms<- try(do.call(rbind.data.frame, List1$basionymOf), silent = TRUE) #Gives Basionyms in order of extracting from List1, but without corresponding names of accepted species 
Basionyms2 <- try(do.call(rbind.data.frame, List1$basionym), silent = TRUE)  #second category of Basionyms
Basionyms <- rbind(Basionyms, Basionyms2)

if (!all(grepl("Error in do.call", Basionyms))) {
Basionyms <- Basionyms %>%
  drop_na(name) #Remove NAs from columns 
Basionym_Names1<- setDT(List1)[, .(Basionym = c(name, unlist(basionymOf,recursive = TRUE))), by = Accepted_Name_with_Author]
Basionym_Names2<- setDT(List1)[, .(Basionym = c(name, unlist(basionym,recursive = TRUE))), by = Accepted_Name_with_Author]
Basionym_Names <- rbind(Basionym_Names1, Basionym_Names2)
Basionym_Names <- Basionym_Names %>%
  drop_na(Basionym) %>% #Remove NAs from columns 
  filter(grepl("urn:lsid:ipni.org:names", Basionym)) %>%
  rename("fqId" = "Basionym")
Basionym_Names<-left_join(Basionym_Names, Basionyms, by="fqId")                                    #Now I join the Synonym to the Accepted Name 
Basionym_Names<-Basionym_Names%>%mutate(Synonym_with_Author = paste(name, author, sep = " "))      #Now I have the Synonyms with their authority names on them. 
Basionym_Names<-Basionym_Names%>%
  transmute(Accepted_Name_with_Author=Accepted_Name_with_Author,
            Synonym_Names_Authors = Synonym_with_Author)
#Now add into Synonym list:
Synonym_Names <- rbind(Synonym_Names,Basionym_Names) #List of all synonyms and Basionyms from KEW dataset per accepted species 
} else {
  print("Skipping because there are no Basionyms")
}
Synonym_Names <- Synonym_Names %>%
  distinct() #Remove duplicates

#Remove cases where the Accepted Name and the Synonym in a given row are the same, as this is redundant 
Synonym_Names <- Synonym_Names[Synonym_Names$Accepted_Name_with_Author != Synonym_Names$Synonym_Names_Authors,]

Final_Synonym_Names<- dcast(setDT(Synonym_Names), Accepted_Name_with_Author ~ rowid(Accepted_Name_with_Author), value.var = "Synonym_Names_Authors")
colnames(Final_Synonym_Names)[1] <- "Accepted_Name_with_Author"

Final_Synonym_Names<-Final_Synonym_Names %>% rename_at(vars(2:ncol(Final_Synonym_Names)), ~paste0('Synonym_',.)) #Rename Synonym Columns, which start at Column 2
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Creating Master List
{
Master_List1<- List1 #Start making Master List
colnames(Master_List1)[which(names(Master_List1) == "name")] <- "Accepted Name"
colnames(Master_List1)[which(names(Master_List1) == "fqId")] <- "KEW ID"

Master_List1$Accepted_Name_with_Author <- str_trim(Master_List1$Accepted_Name_with_Author)
Final_Synonym_Names$Accepted_Name_with_Author <- str_trim(Final_Synonym_Names$Accepted_Name_with_Author)

Master_List1 <- left_join(Master_List1, Final_Synonym_Names, by = c("Accepted_Name_with_Author"))  #I use Left Join to ensure any Accepted Names that do not have synonyms are kept in the list. 

#I will reorder the columns to make it easier to see, but keeping the synonyms at the end since there are so many
Master_List1 <- Master_List1 %>%
  mutate(Accepted_Name_with_Author = paste(`Accepted Name`, authors, sep = " ")) %>% #Now I also have the Accepted Names with their authority names on them. 
  select("Accepted Name", "Accepted_Name_with_Author", "synonym", "KEW ID", "taxonomicStatus", "kingdom", "phylum", "family", "genus", "species", "rank", "plantae", "fungi", "hybrid", "namePublishedInYear", "authors", "reference", "nomenclaturalCode", "nomenclaturalStatus", "lifeform", "climate", "taxonRemarks", "locations", everything())

Master_List1$Accepted_Name_with_Author<-gsub("NA","", Master_List1$Accepted_Name_with_Author) #Remove NAs that get appended to Accepted_Name_with_Author is the authors category is NA

Master_List1$Accepted_Name_with_Author <- trimws(Master_List1$Accepted_Name_with_Author)


}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Handling Unplaced Names

#Some species names are unplaced taxonomically. 
#In some cases, such as with Taraxacum croceum subsp. brachyceras,
#There is no placed option. 
#In other cases such as Potentilla rubricaulis, 
#There is one Accepted option for Taxonomic status, and 
#several unaccepted. Here I decide to remove the unplaced ones when
#there is a placed one. 
{
Unplaced <- Master_List1 %>%
  filter(taxonomicStatus == "Unplaced") #Filter out unplaced 

Master_List1<- Master_List1 %>%
  filter(!Master_List1$`KEW ID` %in% Unplaced$`KEW ID`) #Remove all unplaced 

#Now if the Accepted Name of the Unplaced is still found in the Master List
#Then I can keep it as removed. If it is not found, then I add it back in.  !!! Check with Natalie 

Unplaced_To_ReAdd <- Unplaced %>%
  filter(!Accepted_Name_with_Author %in% Master_List1$Accepted_Name_with_Author)

#Now Add these back 
Master_List1<-rbind(Master_List1, Unplaced_To_ReAdd)

save.image(file = "Taxa_List_Save_14.5.RData") #Save everything 
#load("Taxa_List_Save_14.5.RData")

}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Connecting spelling of input list using Leftovers_Wide
{
#Prepare to join to the Master List - Pivot it long by Accepted Name and with synonyms
Master_List_Mini1 <- Master_List1 %>%
    select(Accepted_Name_with_Author, starts_with("Synonym_"), starts_with("Additional_"), starts_with("Other_Input_Name_"), starts_with("Manually_Added_")) %>%
    pivot_longer(
      cols = starts_with("Synonym_") | starts_with("Additional_") | starts_with("Other_Input_Name_") | starts_with("Manually_Added_"),  
      names_to = "Synonym_Type",      
      values_to = "Synonym") %>%
    filter(!is.na(Synonym)) %>%
    select(-Synonym_Type)
  
Missing_Accepted_Names <- Master_List1 %>%
  select(Accepted_Name_with_Author) %>%
  filter(!Accepted_Name_with_Author %in% Master_List_Mini1$Accepted_Name_with_Author) %>%
  mutate(Synonym = Accepted_Name_with_Author)

#Combine both datasets
Master_List_Mini1 <- bind_rows(Master_List_Mini1, Missing_Accepted_Names)


Master_List_Mini1 <- Master_List_Mini1 %>%
    bind_rows(Master_List_Mini1 %>% 
                mutate(Synonym = Accepted_Name_with_Author) %>% 
                select(Accepted_Name_with_Author, Synonym)) %>%
    distinct() #Add Accepted Name into Synonym row for matching 
  
Master_List_Mini1 <- Master_List_Mini1 %>%
  mutate(Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author),
    Synonym = str_trim(Synonym)) %>% distinct()
  
Leftovers_Wide <- Leftovers_Wide %>%
  mutate(Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author))  
  
Master_List_Mini1 <- Master_List_Mini1 %>%
  left_join(Leftovers_Wide, by = c("Synonym" = "Accepted_Name_with_Author")) %>%
  filter(!is.na(Additional_Input_Name1)) 

#Clean matched Leftovers from Leftovers_Wide
Leftovers_Wide<-Leftovers_Wide%>%
  filter(!Accepted_Name_with_Author %in% Master_List_Mini1$Synonym)

Master_List_Mini1 <- Master_List_Mini1 %>%
  select(-Synonym)  

Final_Leftovers1<-Leftovers_Wide 

Names_Input1<- names(Master_List_Mini1)[-1] #Take all column names except first one, for use in Dcast 

Master_List_Mini1<- dcast(setDT(Master_List_Mini1), Accepted_Name_with_Author ~ rowid(Accepted_Name_with_Author), value.var = Names_Input1)
Master_List_Mini1<-Master_List_Mini1 %>% drop_na(Accepted_Name_with_Author)
#Now shift all names to the left to fill in NA cells (tidy it up)
Master_List_Mini1 <- as.data.frame(t(apply(Master_List_Mini1, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
colnames(Master_List_Mini1)[1] <- "Accepted_Name_with_Author"
colnames(Master_List_Mini1) <- sub("V", "", colnames(Master_List_Mini1))

Master_List_Mini1<-Master_List_Mini1 %>% 
  rename_at(vars(2:ncol(Master_List_Mini1)), ~paste0('Additional_Input_Name',.)) %>% #Rename Input Name Columns 
  select(where(~!all(is.na(.x)))) #Remove for example any columns where all rows are NA.

#Now Left join this to the Master_List1 
Master_List1<- left_join(Master_List1, Master_List_Mini1, by= c("Accepted_Name_with_Author")) 
}
#Now we have a Master List which has the Accepted Names, the synonyms, and the other spellings for the accepted names and synonyms all sorted together,
#along with lots of other data. 

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Removal of Extra Species
{
  #Standardize the Leftovers_Cleaned$original_input_name for proper matching 
  input_names1 <- tolower(unique(Leftovers_Cleaned$original_input_name))
  
  #Confirmed Master List 1 - #Now confirm all rows have a matching Accepted Name or Synonym in the input list 
  Confirmed_Master_List1 <- Master_List1 %>%
    filter(if_any(everything(), ~ tolower(.x) %in% input_names1))  #if_any selects rows where any of the columns have the input_names1 and everything() selects all the columns. 
  
  #Removed Master List - not found from the Master_List1 in the input_names1 (many of these names are already in the Confirmed_Master_List)
  Removed_Master_List1 <- Master_List1 %>%
    filter(!if_any(everything(), ~ tolower(.x) %in% input_names1)) 
}

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Update Final_Leftovers list:

#We have the Final_Leftovers but also we have df_total14 and Final_Leftovers1 which contains leftovers again, 
{
colnames(df_total14)[1] <- "name"  
df_total14$Manually_Edited_Name <- "Unknown"
#Add in the original_input_name from Leftovers_List_Storage
df_total14<- left_join(df_total14, Leftovers_List_Storage, by= c("name" = "species"))
#If the Original Input Name is NA (in the case of a Sect. for example), then just use the manually edited name. 
df_total14$original_input_name <- ifelse(is.na(df_total14$original_input_name), df_total14$name, df_total14$original_input_name)

#Make sure Final_Leftovers is properly structured
Final_Leftovers<- Final_Leftovers %>%
  select(name, original_input_name, Manually_Edited_Name)

#Make sure Final_Leftovers1 is properly structured
colnames(Final_Leftovers1)[1] <- "name"  
colnames(Final_Leftovers1)[2] <- "original_input_name"  
Final_Leftovers1$Manually_Edited_Name <- "Unknown"
Final_Leftovers1 <- Final_Leftovers1 %>% select_if(~any(!is.na(.))) #Remove remaining columns if all values are NA

Final_Leftovers <- bind_rows(Final_Leftovers,Final_Leftovers1, df_total14)
#However keep in mind that df_total14 contains Leftover Names which were manually edited, so the 
#original inputs for these names should be removed from this Final_Leftovers list, as these 
#names have been manually corrected by the taxonomic expert to be what is found in df_total14.
#To handle this:
#First remove cases where Final_Leftovers$original_input_name and Leftovers$Manually_Edited_Name are the same, these are cases 
#where the original_input_name is not true (it was manually changed but not recognized still by POWO - example is Aira cespitosa var. pumila)
Final_Leftovers <- Final_Leftovers %>%
  filter(!original_input_name %in% Leftovers$Manually_Edited_Name) %>% distinct()
    
#N.B that in Final_Leftovers now, Final_Leftovers$name is the Manually_Edited_Name

#Final_Leftovers now represents the final list of leftover names which have not been captured by KEWR even post-manual editing. 

#Write to Excel File:
#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("Unresolved_Leftover_Names_Pre_ExtValidation", today_date, ".xlsx")

#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)

#Write to Excel file
write_xlsx(Final_Leftovers, file_name)

#Set the working directory back to the original directory
setwd("..")

save.image(file = "Taxa_List_Save_15.RData") #Save everything 
#load("Taxa_List_Save_15.RData")
}

#Document Results
Documentation_Results$Thirteen <- length(unique(Final_Leftovers$name))
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Leftover Names - Update Confirmed_Master_List:
{
suppressPackageStartupMessages(library(plyr))
Confirmed_Master_List<-rbind.fill(Confirmed_Master_List,Confirmed_Master_List1)
detach(package:plyr)
library("dplyr")

Confirmed_Master_List$Accepted_Name_with_Author <- trimws(Confirmed_Master_List$Accepted_Name_with_Author)

#Confirm no duplicate Accepted_Name_with_Author names:
n_occur <- data.frame(table(Confirmed_Master_List$Accepted_Name_with_Author)) #Check frequencies of appearance of each Accepted_Name_with_Author
result <- n_occur[n_occur$Freq > 1,] #Check if there are more than 1 Accepted_Name_with_Author appearances. If worked, should be zero.

if (nrow(result) > 0) {
  cat("There are one or more species which occur more than once in this dataframe. Please check. The species are:\n")
  print(result)
} else {
  cat("All Accepted Names occur only once in this dataset. Hurrah!\n")
}
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Confirmed_Master_List_Tidy where all synonyms, basionyms, additional input names, manually changed names are put into same category. 

#To make this list nicer to work with, I can remove many of the empty cells and 
#place all synonyms, input names, etc under one category. 
{
#Make a vector of all relevant names: 
Names_Tidy<- colnames(Confirmed_Master_List) #Create a list of the column names in the Project_List_Others
Names_Tidy <- grep('Accepted Name|Accepted_Name_with_Author|Synonym_|Additional_Input_Name|Other_Input_Name_|Manually_Added_Input_Name', Names_Tidy ,value = TRUE) 

Confirmed_Master_List_Tidy_2<- Confirmed_Master_List %>%           #Select only columns that contain synonyms or other names of some form. 
  select(matches(Names_Tidy)) %>%  #Match to vector list of names. 
  select(-c('Accepted Name'))

#Now shift all names to the left to fill in NA cells (tidy it up)
Confirmed_Master_List_Tidy_2 <- as.data.frame(t(apply(Confirmed_Master_List_Tidy_2, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
colnames(Confirmed_Master_List_Tidy_2)[1] <- "Accepted_Name_with_Author"
colnames(Confirmed_Master_List_Tidy_2) <- sub("V", "", colnames(Confirmed_Master_List_Tidy_2))

Confirmed_Master_List_Tidy_2<-Confirmed_Master_List_Tidy_2 %>% 
  rename_at(vars(2:ncol(Confirmed_Master_List_Tidy_2)), ~paste0('Additional_Name',.)) %>% #Rename Input Name Columns 
  select(where(~!all(is.na(.x)))) #Remove any columns where all rows are NA.

#Catch final cases of corrected Genus names which are synonyms of existing Genera:
additional_cols <- grep("^Additional_", names(Confirmed_Master_List_Tidy_2), value = TRUE)
#Filter rows where all "Additional_" columns are NA
rows_with_na_only <- Confirmed_Master_List_Tidy_2 %>%
  filter(rowSums(is.na(select(., all_of(additional_cols)))) == length(additional_cols)) %>%
  pull(Accepted_Name_with_Author)
#Filter Confirmed_Master_List_Tidy_2 for any cases where that Accepted Name is found in a row other than the Accepted Name row and it is a genus rank
Confirmed_Master_List_Tidy_2 <- Confirmed_Master_List_Tidy_2 %>%
  rowwise() %>%
  filter({
    is_genus <- Accepted_Name_with_Author %in% Confirmed_Master_List$Accepted_Name_with_Author &
      tolower(Confirmed_Master_List$rank[Confirmed_Master_List$Accepted_Name_with_Author == Accepted_Name_with_Author]) == "genus"
    !Accepted_Name_with_Author %in% rows_with_na_only | 
      !any(Accepted_Name_with_Author == c_across(-Accepted_Name_with_Author)) | 
      !is_genus}) %>%ungroup()

Confirmed_Master_List<-Confirmed_Master_List%>%
  filter(Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy_2$Accepted_Name_with_Author)


#Now pivot the data long to remove duplicate synonyms that have accrued during the different processes. 
Confirmed_Master_List_Tidy_2 <- Confirmed_Master_List_Tidy_2 %>%
  select(Accepted_Name_with_Author, starts_with("Additional_")) %>%
  pivot_longer(cols = starts_with("Additional_"),  
               names_to = "Additional_Name_Value",  
               values_to = "Additional_Name") %>%
  select(Accepted_Name_with_Author, Additional_Name) %>%
  drop_na(Additional_Name) %>%
  mutate(Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author),  
    Additional_Name = str_trim(Additional_Name)) %>%
  distinct()

Confirmed_Master_List_Tidy_2 <- Confirmed_Master_List_Tidy_2 %>%
  rowid_to_column(var = "row_id")

#Pivot the data to wide format
Confirmed_Master_List_Tidy_2 <- Confirmed_Master_List_Tidy_2 %>%
  pivot_wider(
    id_cols = Accepted_Name_with_Author,
    names_from = row_id,
    values_from = Additional_Name,
    names_prefix = "Additional_Name")
}
Confirmed_Master_List_Tidy_2 <- as.data.frame(t(apply(Confirmed_Master_List_Tidy_2, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
{
colnames(Confirmed_Master_List_Tidy_2)[1] <- "Accepted_Name_with_Author"
colnames(Confirmed_Master_List_Tidy_2) <- sub("V", "", colnames(Confirmed_Master_List_Tidy_2))

Confirmed_Master_List_Tidy_2<-Confirmed_Master_List_Tidy_2 %>% 
  select(where(~ !all(is.na(.)))) %>%
  rename_with(~ paste0('Additional_Name', seq_along(.) - 1 + 1), -1) #Ignores first column but starts naming with 1
}
{
#Make a vector of all relevant names: #Excluding Accepted_Name_with_Author
Names_Tidy_1<- colnames(Confirmed_Master_List) #Create a list of the column names in the Project_List_Others
Names_Tidy_1 <- grep('Accepted Name|Synonym_1|Synonym_2|Synonym_3|Synonym_4|Synonym_5|Synonym_6|Synonym_7|Synonym_8|Synonym_9|Additional_Input_Name|Other_Input_Name_|Input_Name_Form|Added_Name|More_Added_Name_|Manually_Added_Input_Name|Original_Spelling_GBIF_Name', Names_Tidy_1 ,value = TRUE) 

Confirmed_Master_List_Tidy_1 <- Confirmed_Master_List %>%
  select(!matches(Names_Tidy_1)) %>%
  select(-any_of(c("fqId", "author", "url", "images", "synonymOf", "snippet", "name")))

#Now Left join
Confirmed_Master_List_Tidy<- left_join(Confirmed_Master_List_Tidy_1, Confirmed_Master_List_Tidy_2, by= c("Accepted_Name_with_Author")) 

#Write to Excel File:
#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("Master_Taxa_List_GBIF_only_", today_date, ".xlsx")

#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)

#Write to Excel file
write_xlsx(Confirmed_Master_List_Tidy, file_name)

#Set the working directory back to the original directory
setwd("..")

#Now the Confirmed_Master_List_Tidy has the same names as Confirmed_Master_List, but it has been tidied up so that all the different names, regardless of their 
#source, are labelled the same, and this allowed for the columns to be reduced. The only compromise is now knowing the label of each additional
#name, aka is it a syonynm, a basionym, another name for a form, a name I manually changed, etc. 

#Document Results
Documentation_Results$Fourteen<-nrow(Confirmed_Master_List_Tidy)

save.image(file = "Taxa_List_Save_16.RData") #Save everything 
#load("Taxa_List_Save_16.RData")
}

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Processing Autonym - Update Confirmed_Master_List:

#Autonyms are finally processed here because it is possible that more are added or edited during other stages of processing leftover names.

#A subspecific taxon in autonym form can only exist when there are other subspecific taxa under the same parent species. 
#Therefore, for cases when an autonym exists in the occurrence data but there are no other subspecies (based on different types) 
#according to POWO, then that subspecies does not exist and is a synonym of the parent species. 

#Therefore, find cases where an autonym is the only subspecies rank for a given species (now using the POWO data we have integrated). 
#If this is the case, then reallocate that subspecies to being a synonym of the parent species and not a separate taxon in the list. 

#Will need to compare only to POWO data and not other input names, as they may be unrecognized infraspecific rank names. 
#Therefore, access the Confirmed_Master_List_Tidy$childNameUsages 
#Can also ignore cases when they are autonyms but have an IPNI, these are verified true. 

Autonym_reassignment_check <- Confirmed_Master_List_Tidy %>%
  filter(Autonym == TRUE, is.na(`KEW ID`) | `KEW ID` == "") %>%
  mutate(Accepted_Name = sub("^([[:alpha:]-]+ [[:alpha:]-]+).*", "\\1", Accepted_Name_with_Author)) %>%
  select(Accepted_Name, Accepted_Name_with_Author) %>%
  left_join(Autonyms %>% select(Accepted_Name_with_Author, starts_with("Synonym_")), 
          by = "Accepted_Name_with_Author") %>%
  rowwise() %>%
  mutate(Autonym_Synonym_Check = any(c_across(starts_with("Synonym_")) != Accepted_Name_with_Author, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-starts_with("Synonym_")) 

#Account for manually edited names - check back to manually edited ones 
if (nrow(Autonym_reassignment_check) > 0) {
  if (nrow(df_Autonyms3_save_) > 0) {
Autonym_reassignment_check <- Autonym_reassignment_check %>%
  mutate(Autonym_Synonym_Check = if_else(
    Accepted_Name_with_Author %in% (Autonyms %>%filter(if_any(starts_with("Synonym_"), ~ str_detect(., str_c("\\b", df_Autonyms3_save_$original_input_name, "\\b", collapse = "|")))) %>%
    pull(Accepted_Name_with_Author)) & Autonym_Synonym_Check == TRUE, 
    FALSE,Autonym_Synonym_Check))
}}
#Now cases of FALSE have not had their names edited, and cases of TRUE were originally another synoynm name. 
#These cases must be treated differently. 

#For the FALSE cases, check if their parent species have other infrapsecific designations or not. If yes,
#keep the autonym. If no, reclassify it as the parent species (and check if this is in the Master List already)

#For TRUE cases, also need to check if their parent species have other infrapsecific designations or not. If yes,
#then we cannot ascertain to which infraspecific designation the autonym belongs (this is a taxonomic decision not nomenclature)
#and we therefore need to remove it for later study. If no, reclassify it as the parent species (and check if this is in the Master List already)

#First, for all cases, check into the parent species:
#Do not assume that parent species is included in master list
#But do assume that autonym parent species in this list have only one parent option (not multiple authorities), as this 
#was already filtered out earlier (those cases added to removed names list)
{
  if (nrow(Autonym_reassignment_check) > 0) {
df_Autonyms4 = data.frame() #for accepted species 
df_Autonyms5 = data.frame() #for synonyms, which have 2 or more rows because one row is the accepted name for that given synonym
df_Autonyms6 = data.frame() #for ones that do not work with the R package KEWR - will include most hybrids, some varieties, and some orthographic variants 
Temp2 <- data.frame(accepted=logical(),
                    author=character(),
                    kingdom=character(),
                    family=character(),
                    name=character(),
                    rank=character(),
                    url=character(),
                    fqId=character(),
                    images=numeric(),
                    synonymOf=numeric(),
                    snippet=numeric())

#Progress Bar
total_iterations <- length(unique(Autonym_reassignment_check$Accepted_Name))

#Initialize the progress bar
Leftover_Progress_Bar <- progress_bar$new(
  format = "  Processing [:bar] :percent :current/:total Autonym IDs :elapsed",
  total = total_iterations,  #Total number of iterations
  width = 60,                
  clear = FALSE              
)
  }}

#Now using the search_powo of the Kewr package, I use each species name in my list and search the KEW database. 
if (nrow(Autonym_reassignment_check) > 0) {
for (sp in unique(Autonym_reassignment_check$Accepted_Name)) {                                                       #for each sp Accepted_Name in my list search KEW. 
  Temp1 <- search_powo(sp,)                                                                    #Search KEW and store information 
  if (Temp1$total==0) {
    df_Autonyms6 <- rbind(df_Autonyms6,Temp1$query)  
  } else {
    Temp2 <- tidy(Temp1)                                                                      #Tidy information 
  }
  x <- c("accepted","author","kingdom","family","name","rank", "url","fqId","images", "synonymOf", "snippet")          #Create category images etc , which some outputs do contain
  Temp2[x[!(x %in% colnames(Temp2))]] = 0                                                    #If a Temp2 query doesn't have this column, then add it in with a value of 0, to allow rbinding later. 
  
  if (nrow(Temp2)>=2) {                                                                      #If there are 2 or more rows, store in df_Autonyms5 as this is a synonym species. 
    df_Autonyms5 <- rbind(df_Autonyms5,Temp2)  
  } else {
    df_Autonyms4 <- rbind(df_Autonyms4,Temp2)                                                      #If there is only one row, store in df_Autonyms4 as this is already an accepted name
  }
  
  #Update the progress bar
  Leftover_Progress_Bar$tick()
}}
{
  if (nrow(Autonym_reassignment_check) > 0) {
df_Autonyms4 <- df_Autonyms4 %>% #Remove duplicates 
    distinct()
  
df_Autonyms5 <- df_Autonyms5 %>% #Remove duplicates 
    distinct() %>%
  filter(accepted==TRUE) %>%
  filter(rank=="Species")
  
df_Autonyms6 <- df_Autonyms6 %>% #Remove duplicates 
    distinct()

df_Autoynms_Check<-rbind(df_Autonyms4,df_Autonyms5)

#Extract Genus name (querying at species or lower does not capture all cases)
df_Autoynms_Check <- df_Autoynms_Check %>%
  mutate(name = str_trim(name), Genus = word(name, 1))

#Now find all cases of infraspecies under the species names:
Species_Children <- map_dfr(unique(df_Autoynms_Check$Genus), ~ {
  search_powo(.x, filters = c("infraspecies", "accepted"), limit = 1000) %>%
    tidy() })

Species_Children<-Species_Children %>%  
    mutate(name = str_trim(name), Species_Name = word(name, 1, 2)) %>%
  filter(Species_Name %in% df_Autoynms_Check$name) %>%
  filter(!rank=="Infraspecificname")
}}

save.image(file = "Taxa_List_Save_16.25.RData") #Save everything 
#load("Taxa_List_Save_16.25.RData")

#Species_Children now contains infraspecific ranks belonging to the species we are interested in. 
#Now it can be used in multiple ways:
{
#Some editing first
if (nrow(Autonym_reassignment_check) > 0) {
  Autonym_reassignment_check <- Autonym_reassignment_check %>%
    mutate(rank = case_when(
      str_detect(Accepted_Name_with_Author, " var\\. ") ~ "Variety",
      str_detect(Accepted_Name_with_Author, " subsp\\. ") ~ "Subspecies",
      str_detect(Accepted_Name_with_Author, " f\\. ") ~ "Form",
      TRUE ~ "OTHER"
    )) %>%
    filter(rank != "OTHER")

Species_Children <- Species_Children %>%
  semi_join(Autonym_reassignment_check, 
            by = c("Species_Name" = "Accepted_Name", "rank" = "rank"))
}

#Now Species_Children only contains cases of autonyms under the correct infraspecific rank.
#Meaning that, for example, if we are only looking for the variety autonym of a given species, then the subspecies ones have been removed. 

#Process Cases:
#First, cases where the autonym is not originally a synonym name in the occurrence data (FALSE in Autonym_reassignment_check), and 
#the parents DO have other infraspecific designations for the given rank - keep the autonym as it is:

#Remove these cases from Autonym_reassignment_check, we do not touch them in the Confirmed Master List. 
if (nrow(Autonym_reassignment_check) > 0) {
Autonym_reassignment_check <- Autonym_reassignment_check %>%
  filter(!(Autonym_Synonym_Check == FALSE & 
             Accepted_Name %in% Species_Children$Species_Name & 
             rank %in% Species_Children$rank))}

#Second, cases where the autonym is not originally a synonym name in the occurrence data (FALSE in Autonym_reassignment_check), and 
#the parents do NOT have other infraspecific designations for the given rank - reclassify the autonym as its parent species:
if (nrow(Autonym_reassignment_check) > 0) {
Autonym_reassignment_check <- Autonym_reassignment_check %>%
  left_join(df_Autoynms_Check %>% 
              select(name, author), by = c("Accepted_Name" = "name")) %>%
  mutate(Parent_Reclassify = if_else(
    !Autonym_Synonym_Check & 
      !(Accepted_Name %in% Species_Children$Species_Name & rank %in% Species_Children$rank),
    str_trim(paste(Accepted_Name, author)),
    NA_character_)) %>%
  select(-author)}   

#Third, cases where the autonym is originally a synonym name in the occurrence data (TRUE in Autonym_reassignment_check), and 
#the parents do NOT have other infraspecific designations for the given rank - reclassify the autonym as its parent species:
if (nrow(Autonym_reassignment_check) > 0) {
  Autonym_reassignment_check <- Autonym_reassignment_check %>%
    left_join(df_Autoynms_Check %>% 
                select(name, author), 
              by = c("Accepted_Name" = "name")) %>%
    mutate(Parent_Reclassify = if_else(
      Autonym_Synonym_Check & 
        !(Accepted_Name %in% Species_Children$Species_Name & rank %in% Species_Children$rank),
      str_trim(paste(Accepted_Name, author)),
      Parent_Reclassify  # Keep existing values from previous step
    )) %>%
    select(-author)}

#In these cases, change the synonym back to the original occurrence record name, as now it has a new synonym, the parent species.
if (nrow(Autonym_reassignment_check) > 0) {
expanded_autonyms <- Autonym_reassignment_check %>%
  filter(Autonym_Synonym_Check, !is.na(Parent_Reclassify) & Parent_Reclassify != "") %>%
  left_join(Autonyms, by = "Accepted_Name_with_Author") %>%
  select(-Accepted_Name_with_Author) %>%  #Remove old column before expanding
  pivot_longer(cols = starts_with("Synonym_"), names_to = "Synonym_Column", values_to = "New_Accepted_Name") %>%
  filter(!is.na(New_Accepted_Name) & New_Accepted_Name != "") %>%
  mutate(Accepted_Name_with_Author = New_Accepted_Name) %>%
  select(-Synonym_Column, -New_Accepted_Name)

Autonym_reassignment_check <- Autonym_reassignment_check %>%
  filter(!(Autonym_Synonym_Check & !is.na(Parent_Reclassify) & Parent_Reclassify != "")) %>%
  bind_rows(expanded_autonyms) %>%
  select(Parent_Reclassify, Accepted_Name_with_Author, Autonym_Synonym_Check)
}
}
#Fourth, cases where the autonym is originally a synonym name in the occurrence data (TRUE in Autonym_reassignment_check), and 
#the parents DO have other infraspecific designations for the given rank, an example is Micranthes stellaris variants, where
#the occurrence data contained "Saxifraga stellaris var. stellaris" and this parent species is a synonym of Micranthes stellaris,
#but Micranthes stellaris has additional varient options, so we do not know to which it belongs to. In these specific cases, we need to 
#remove the name as this is a taxonomic decision and not a nomenclatural one. 
if (nrow(Autonym_reassignment_check) > 0) {
Taxonomic_Autonyms_Publish<-Autonym_reassignment_check%>%
  filter(is.na(Parent_Reclassify)) %>%
  left_join(Autonyms, by = c("Accepted_Name_with_Author" = "Accepted_Name_with_Author")) %>%
  select(-Accepted_Name_with_Author) %>%
  pivot_longer(cols = starts_with("Synonym_"), names_to = "Synonym_Column", values_to = "New_Name") %>%
  filter(!is.na(New_Name) & New_Name != "") %>%
  mutate(Accepted_Name_with_Author = New_Name) %>%
  select(Accepted_Name_with_Author) %>%
  rename(original_input_name = Accepted_Name_with_Author) %>%
  mutate(Verified_Name = original_input_name,
         Status = "Taxonomic_Uncertainty_Autonym_Synonym",
         rank=NA)
}

#Remove from Confirmed Master List and Autonym_reassignment_check (these names now stored in Taxonomic_Autonyms_Publish):
if (nrow(Taxonomic_Autonyms_Publish) > 0) {
  remove_names <- Autonym_reassignment_check %>%
    filter(is.na(Parent_Reclassify) & Autonym_Synonym_Check) %>%
    pull(Accepted_Name_with_Author)
  if (length(remove_names) > 0) {  #Ensure it's not empty before filtering
    Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
      filter(!Accepted_Name_with_Author %in% remove_names)
  }
  
  Autonym_reassignment_check<-Autonym_reassignment_check%>%
    filter(!is.na(Parent_Reclassify))
}

#Autonym_reassignment_check now contains names, and only names, which need to be changed for the Master List
if (nrow(Autonym_reassignment_check) > 0) {
Autonym_reassignment_check <- Autonym_reassignment_check %>%
  left_join(
    Confirmed_Master_List_Tidy %>%
      select(Accepted_Name_with_Author, starts_with("Additional_")),
    by = "Accepted_Name_with_Author") %>%
  select(where(~ !all(is.na(.)))) %>%
  select(-Autonym_Synonym_Check)

Autonym_reassignment_check <- Autonym_reassignment_check %>%
  pivot_longer(
    cols = -Parent_Reclassify,  #Pivot all columns except Parent_Reclassify
    names_to = "Column_Type",
    values_to = "Value") %>%
  select(Parent_Reclassify, Value) %>%
  distinct() %>%
  drop_na(Value) %>%
  rename(Accepted_Name_with_Author = Parent_Reclassify,
         Additional_Name = Value)

#Remove the Accepted_Name_with_Author Autonyms from the Confirmed Master List now
Confirmed_Master_List_Tidy<-Confirmed_Master_List_Tidy%>%
  filter(!Accepted_Name_with_Author %in% Autonym_reassignment_check$Additional_Name)

Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  filter(!if_any(starts_with("Additional_"), ~ . %in% Autonym_reassignment_check$Additional_Name))

#Quick clean of specific autonym error that occurs with authority where duplicate authority name is added. 
Autonym_reassignment_check <- Autonym_reassignment_check %>%
  bind_rows(filter(., str_detect(Additional_Name, "\\b([A-Z][a-zA-Z]*)\\.\\s+\\1\\.")) %>%
              mutate(Additional_Name = str_replace(Additional_Name, "(\\b[A-Z][a-zA-Z]*)\\.\\s+\\1\\.", "\\1.")))

}

#Now can add these names in as synonyms to existing Species Names, or need to add in as new species: 
if (nrow(Autonym_reassignment_check) > 0) {
Autonym_reassignment_check_existing_parents<-Autonym_reassignment_check%>%
  filter(Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy$Accepted_Name_with_Author)

Autonym_reassignment_check_NOT_existing_parents<-Autonym_reassignment_check%>%
  filter(!Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy$Accepted_Name_with_Author) %>%
  mutate(Accepted_Name = str_extract(Accepted_Name_with_Author, "^[A-Za-z-]+\\s+[A-Za-z-]+")) 

Autonym_reassignment_check_NOT_existing_parents <- Autonym_reassignment_check_NOT_existing_parents %>%
  left_join(df_Autoynms_Check %>% 
              select(name, fqId), by = c("Accepted_Name" = "name")) %>%
  mutate(Accepted_Taxon_ID = gsub("urn:lsid:ipni.org:names:", "", fqId))

#Find information for any cases of Autonym_reassignment_check_NOT_existing_parents
if (nrow(Autonym_reassignment_check_NOT_existing_parents) > 0) {
  
  List1 = data.frame()
  #Progress Bar
  total_iterations <- length(unique(Autonym_reassignment_check_NOT_existing_parents$Accepted_Taxon_ID))
  #Initialize the progress bar
  Progress_Bar_Leftover <- progress_bar$new(
    format = "  Processing [:bar] :percent :current/:total Taxon IDs :elapsed",
    total = total_iterations,  #Total number of iterations
    width = 60,                #Width of the progress bar
    clear = FALSE              #Keep the bar after completion
  )
  
  for (ac in unique(Autonym_reassignment_check_NOT_existing_parents$Accepted_Taxon_ID)) {           #for each ac accepted species Taxon ID in my list
  Temp1 <- lookup_powo(ac)                         #Search KEW and store information for that accepted Taxon ID
  Temp2 <- tidy(Temp1)                             #Tidy information 
  Temp2 <- add_cols(Temp2, c('basionym','basionymOf','modified','bibliographicCitation','genus','taxonomicStatus','kingdom','phylum','family','nomenclaturalCode','source','namePublishedInYear','taxonRemarks',
                             'nomenclaturalStatus','lifeform','climate', 'hybrid','childNameUsages','synonym','locations','plantae','fungi','fqId', 'name', 'authors','species','rank','reference','classification',
                             'synonyms', 'nomenclaturalRemarks','infraspecies', 'hybridFormula', "paftolId")) #If this fails, check the code of the ac that failed and see what extra columns it has, then add in here. 
  List1 <- rbind(List1,Temp2)  
  Progress_Bar_Leftover$tick()
}
{
    #Wipe old DFs
    Synonym_Names2<-NA
    Synonym_Names<-NA
    Basionyms<-NA
    Basionyms2<-NA
    Synonym_Names_Authors<-NA
    Final_Synonym_Names<-NA
    
    #Preparing for Master List - Extracting Synonyms 
    #Synonyms are packed in a list format, so I will do a weird workaround to get to them. 
    Synonym_Names2<- do.call(rbind.data.frame, List1$synonyms) #Gives synonyms in order of extracting from List1, but without corresponding names of accepted species 
    
    Synonym_Names2<-Synonym_Names2%>%mutate(Synonym_with_Author = paste(name, author, sep = " "))      #Now I have the Synonyms with their authority names on them. 
    List1<-List1%>%mutate(Accepted_Name_with_Author = paste(name, authors, sep = " "))                 #Now I have the Accepted Name with their authority names on them.
    List1$Accepted_Name_with_Author<-gsub("NA","", List1$Accepted_Name_with_Author)                    #Remove NAs that get appended to Accepted_Name_with_Author is the authors category is NA
    List1$Accepted_Name_with_Author <- str_trim(List1$Accepted_Name_with_Author)
    
    Synonym_Names<- setDT(List1)[, .(Synonym = c(name, unlist(synonyms,recursive = TRUE))), by = Accepted_Name_with_Author] #This unnests the list (including matching accepted name) but does it for all list columns, and I do not want this. 
    Synonym_Names<-Synonym_Names %>%                                                                   #Now I keep the urn:lsid:ipni.org:names codes as these match the Accepted Name to the Synonym 
      filter(grepl("urn:lsid:ipni.org:names",Synonym_Names$Synonym)) %>% #If this doesn't work when re-running, turn Synonym_Names$Synonym to only "Synonym"
      rename("fqId" = "Synonym")
    
    Synonym_Names<-left_join(Synonym_Names, Synonym_Names2, by="fqId")                                 #Now I join the Synonym to the Accepted Name 
    Synonym_Names<-Synonym_Names%>%
      transmute(Accepted_Name_with_Author=Accepted_Name_with_Author,
                Synonym_Names_Authors = Synonym_with_Author)
    
    #Repeat for Basionyms - likely already in the synonyms for each species, but will do again for due diligence 
    Basionyms<- try(do.call(rbind.data.frame, List1$basionymOf), silent = TRUE) #Gives Basionyms in order of extracting from List1, but without corresponding names of accepted species 
    Basionyms2 <- try(do.call(rbind.data.frame, List1$basionym), silent = TRUE)  #second category of Basionyms
    Basionyms <- rbind(Basionyms, Basionyms2)
    
    if (!all(grepl("Error in do.call", Basionyms))) {
      Basionyms <- Basionyms %>%
        drop_na(name) #Remove NAs from columns 
      Basionym_Names1<- setDT(List1)[, .(Basionym = c(name, unlist(basionymOf,recursive = TRUE))), by = Accepted_Name_with_Author]
      Basionym_Names2<- setDT(List1)[, .(Basionym = c(name, unlist(basionym,recursive = TRUE))), by = Accepted_Name_with_Author]
      Basionym_Names <- rbind(Basionym_Names1, Basionym_Names2)
      Basionym_Names <- Basionym_Names %>%
        drop_na(Basionym) %>% #Remove NAs from columns 
        filter(grepl("urn:lsid:ipni.org:names", Basionym)) %>%
        rename("fqId" = "Basionym")
      Basionym_Names<-left_join(Basionym_Names, Basionyms, by="fqId")                                    #Now I join the Synonym to the Accepted Name 
      Basionym_Names<-Basionym_Names%>%mutate(Synonym_with_Author = paste(name, author, sep = " "))      #Now I have the Synonyms with their authority names on them. 
      Basionym_Names<-Basionym_Names%>%
        transmute(Accepted_Name_with_Author=Accepted_Name_with_Author,
                  Synonym_Names_Authors = Synonym_with_Author)
      #Now add into Synonym list:
      Synonym_Names <- rbind(Synonym_Names,Basionym_Names) #List of all synonyms and Basionyms from KEW dataset per accepted species 
    } else {
      print("Skipping because there are no Basionyms")
    }
    Synonym_Names <- Synonym_Names %>%
      distinct() #Remove duplicates
    
    #Remove cases where the Accepted Name and the Synonym in a given row are the same, as this is redundant 
    Synonym_Names <- Synonym_Names[Synonym_Names$Accepted_Name_with_Author != Synonym_Names$Synonym_Names_Authors,]
    
    Final_Synonym_Names<- dcast(setDT(Synonym_Names), Accepted_Name_with_Author ~ rowid(Accepted_Name_with_Author), value.var = "Synonym_Names_Authors")
    colnames(Final_Synonym_Names)[1] <- "Accepted_Name_with_Author"
    
    Final_Synonym_Names<-Final_Synonym_Names %>% rename_at(vars(2:ncol(Final_Synonym_Names)), ~paste0('Additional_Name',.)) #Rename Synonym Columns, which start at Column 2
  }

  List1<-List1%>%mutate(Accepted_Name_with_Author = paste(name, authors, sep = " "))                 #Now I have the Accepted Name with their authority names on them.
  List1$Accepted_Name_with_Author<-gsub("NA","", List1$Accepted_Name_with_Author)                    #Remove NAs that get appended to Accepted_Name_with_Author if the authors category is NA
  List1$Accepted_Name_with_Author <- trimws(List1$Accepted_Name_with_Author)
  Autonym_Parent_Add<-List1
  Autonym_Parent_Add <- left_join(Autonym_Parent_Add, Final_Synonym_Names, by = c("Accepted_Name_with_Author"))

  colnames(Autonym_Parent_Add)[which(names(Autonym_Parent_Add) == "fqId")] <- "KEW ID"
  Autonym_Parent_Add <- Autonym_Parent_Add %>%
    mutate(Autonym = FALSE) %>%
    mutate(response = NA)

suppressPackageStartupMessages(library(plyr))
Confirmed_Master_List_Tidy<-rbind.fill(Confirmed_Master_List_Tidy,Autonym_Parent_Add)
detach(package:plyr)
library("dplyr")
}

#Now add in Autonym_reassignment_check_existing_parents names 
{
  #Make a vector of all relevant names: 
  Names_Tidy<- colnames(Confirmed_Master_List_Tidy) #Create a list of the column names in the Project_List_Others
  Names_Tidy <- grep('Accepted Name|Accepted_Name_with_Author|Synonym_|Additional_Input_Name|Other_Input_Name_|Manually_Added_Input_Name|Additional', Names_Tidy ,value = TRUE) 
  
  Confirmed_Master_List_Tidy_2<- Confirmed_Master_List_Tidy %>%           #Select only columns that contain synonyms or other names of some form. 
    select(matches(Names_Tidy))   #Match to vector list of names. 
 
  
  #Now shift all names to the left to fill in NA cells (tidy it up)
  Confirmed_Master_List_Tidy_2 <- as.data.frame(t(apply(Confirmed_Master_List_Tidy_2, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
  colnames(Confirmed_Master_List_Tidy_2)[1] <- "Accepted_Name_with_Author"
  colnames(Confirmed_Master_List_Tidy_2) <- sub("V", "", colnames(Confirmed_Master_List_Tidy_2))
  
  Confirmed_Master_List_Tidy_2<-Confirmed_Master_List_Tidy_2 %>% 
    rename_at(vars(2:ncol(Confirmed_Master_List_Tidy_2)), ~paste0('Additional_Name',.)) %>% #Rename Input Name Columns 
    select(where(~!all(is.na(.x)))) #Remove any columns where all rows are NA.
  
  #Catch final cases of corrected Genus names which are synonyms of existing Genera:
  additional_cols <- grep("^Additional_", names(Confirmed_Master_List_Tidy_2), value = TRUE)
  #Filter rows where all "Additional_" columns are NA
  rows_with_na_only <- Confirmed_Master_List_Tidy_2 %>%
    filter(rowSums(is.na(select(., all_of(additional_cols)))) == length(additional_cols)) %>%
    pull(Accepted_Name_with_Author)
  #Filter Confirmed_Master_List_Tidy_2 for any cases where that Accepted Name is found in a row other than the Accepted Name row and it is a genus rank
  Confirmed_Master_List_Tidy_2 <- Confirmed_Master_List_Tidy_2 %>%
    rowwise() %>%
    filter({
      is_genus <- Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy$Accepted_Name_with_Author &
        tolower(Confirmed_Master_List_Tidy$rank[Confirmed_Master_List_Tidy$Accepted_Name_with_Author == Accepted_Name_with_Author]) == "genus"
      !Accepted_Name_with_Author %in% rows_with_na_only | 
        !any(Accepted_Name_with_Author == c_across(-Accepted_Name_with_Author)) | 
        !is_genus}) %>%ungroup()
  
  Confirmed_Master_List_Tidy<-Confirmed_Master_List_Tidy%>%
    filter(Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy_2$Accepted_Name_with_Author)
  
  
  #Now pivot the data long to remove duplicate synonyms that have accrued during the different processes. 
  Confirmed_Master_List_Tidy_2 <- Confirmed_Master_List_Tidy_2 %>%
    select(Accepted_Name_with_Author, starts_with("Additional_")) %>%
    pivot_longer(cols = starts_with("Additional_"),  
                 names_to = "Additional_Name_Value",  
                 values_to = "Additional_Name") %>%
    select(Accepted_Name_with_Author, Additional_Name) %>%
    drop_na(Additional_Name) %>%
    mutate(Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author),  
           Additional_Name = str_trim(Additional_Name)) %>%
    distinct()
  
  #Now add in names from Autonym_reassignment_check_existing_parents
  Confirmed_Master_List_Tidy_2<-rbind(Confirmed_Master_List_Tidy_2,Autonym_reassignment_check_existing_parents)
  Confirmed_Master_List_Tidy_2 <- Confirmed_Master_List_Tidy_2 %>%
    mutate(across(everything(), ~ str_trim(.)))
  Confirmed_Master_List_Tidy_2 <- Confirmed_Master_List_Tidy_2 %>%
    distinct()
  
  Confirmed_Master_List_Tidy_2 <- Confirmed_Master_List_Tidy_2 %>%
    rowid_to_column(var = "row_id")
  
  #Pivot the data to wide format
  Confirmed_Master_List_Tidy_2 <- Confirmed_Master_List_Tidy_2 %>%
    pivot_wider(
      id_cols = Accepted_Name_with_Author,
      names_from = row_id,
      values_from = Additional_Name,
      names_prefix = "Additional_Name")
}
Confirmed_Master_List_Tidy_2 <- as.data.frame(t(apply(Confirmed_Master_List_Tidy_2, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
{
  colnames(Confirmed_Master_List_Tidy_2)[1] <- "Accepted_Name_with_Author"
  colnames(Confirmed_Master_List_Tidy_2) <- sub("V", "", colnames(Confirmed_Master_List_Tidy_2))
  
  Confirmed_Master_List_Tidy_2<-Confirmed_Master_List_Tidy_2 %>% 
    select(where(~ !all(is.na(.)))) %>%
    rename_with(~ paste0('Additional_Name', seq_along(.) - 1 + 1), -1) #Ignores first column but starts naming with 1
}
{
  #Make a vector of all relevant names: #Excluding Accepted_Name_with_Author
  Names_Tidy_1<- colnames(Confirmed_Master_List_Tidy) #Create a list of the column names in the Project_List_Others
  Names_Tidy_1 <- grep('Accepted Name|Synonym_1|Synonym_2|Synonym_3|Synonym_4|Synonym_5|Synonym_6|Synonym_7|Synonym_8|Synonym_9|Additional_Input_Name|Other_Input_Name_|Input_Name_Form|Added_Name|More_Added_Name_|Manually_Added_Input_Name|Original_Spelling_GBIF_Name|Additional', Names_Tidy_1 ,value = TRUE) 
  
  Confirmed_Master_List_Tidy_1 <- Confirmed_Master_List_Tidy %>%
    select(!matches(Names_Tidy_1)) %>%
    select(-any_of(c("fqId", "author", "url", "images", "synonymOf", "snippet", "name")))
  
  #Now Left join
  Confirmed_Master_List_Tidy<-NA
  Confirmed_Master_List_Tidy<- left_join(Confirmed_Master_List_Tidy_1, Confirmed_Master_List_Tidy_2, by= c("Accepted_Name_with_Author")) 
  
  #Re-Write to Excel File:
  #Get today's date (e.g., Feb28_2024)
  today_date<- format(Sys.Date(), "%b%d_%Y")
  #Concatenate the date with the rest of the file name
  file_name <- paste0("Master_Taxa_List_GBIF_only_", today_date, ".xlsx")
  
  #Now, set the working directory to the folder created in the start of the script
  setwd(folder_name)
  
  #Write to Excel file
  write_xlsx(Confirmed_Master_List_Tidy, file_name)
  
  #Set the working directory back to the original directory
  setwd("..")
  
  #Now the Confirmed_Master_List_Tidy has the same names as Confirmed_Master_List, but it has been tidied up so that all the different names, regardless of their 
  #source, are labelled the same, and this allowed for the columns to be reduced. The only compromise is now knowing the label of each additional
  #name, aka is it a syonynm, a basionym, another name for a form, a name I manually changed, etc. 
  
  #Document Results
  Documentation_Results$FourteenHalf<-nrow(Confirmed_Master_List_Tidy)
  
  save.image(file = "Taxa_List_Save_16.5.RData") #Save everything 
  #load("Taxa_List_Save_16.5.RData")
}
}

###########################################################
### END of Generating Species List and Leftovers List #####
###########################################################
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#




#######################################################################
####### Validation Against External Flora - Species and Leftovers #####
#######################################################################
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#

#Before counting the number of GBIF specimens found for each accepted name, it is important to first compare the species list
#against an additional local reference flora, local to the region(s) under investigation. This will allow for potentially missed names
#to be added into the main list, and may also provide evidence for leftover names to be re-added back into the main list. 

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#External Validation: Step 1 - Compare to Local Flora - Setting up References 

#Prepare reference database: 
reference_flora_name <- readline(prompt = "Please write in the name of the reference flora to be used: ")
#Document Results
Documentation_Results$Fifteen<-reference_flora_name

cat("During this step, the Species List and the Leftover Names will be compared to another database, ideally a local flora\n")
cat("The local flora should be formatted as follows:\n
    Column 1 labelled as Accepted_Name_with_Author, containing the Accepted Name and the author of the Species, in ICN format
    Column 2 labelled as Accepted_Name, containing just the Accepted Name of the species
    Additional n columns labelled as Synonym_n for n synonyms, containing the Synonym Name with author
    Additional columns may contain other relevent information, such as the taxa rank, distribution, etc.
    If it contains a taxa rank column, please call it rank" )

cat("As needed, please write code here to clean the local flora data, customized to the local flora being used. Below is an example using the Panarctic Flora of Vascular Plants\n")




Reference_List<-read_excel("") #Insert here code for reference list



#Reference_List now contains the Accepted Name with and without author, along with synonyms. 

#Note that when there are disparities between the Reference List and POWO, POWO will take precedent 
#For example, PAF lists Antennaria intermedia (Rosenv.) Porsild as an accepted name. However in POWO, 
#this name is a synonym of Antennaria alpina (L.) Gaertn. Therefore, in this process, POWO will take precedent over the local reference. 

#Document Results
Documentation_Results$Sixteen<-nrow(Reference_List)

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#External Validation: Step 2 - Comparing the local reference to the leftovers list to see if anything is validated - Expert Taxonomist check 
{
#Create long format of reference database 
Names_Reference_List<- colnames(Reference_List) #Create a list of the column names in the Project_List_Others
Names_Reference_List <- grep('Accepted_Name_with_Author|Synonym_1|Synonym_2|Synonym_3|Synonym_4|Synonym_5|Synonym_6|Synonym_7|Synonym_8|Synonym_9', Names_Reference_List,value = TRUE) 

Reference_List_Long<- Reference_List %>%           #Select only columns that contain the accepted name and other names 
  select(matches(Names_Reference_List))

Reference_List_Long$Accepted_Name_Backup<- Reference_List_Long$Accepted_Name_with_Author

#Reshape Reference_List into long format
Reference_List_Long <- Reference_List_Long %>%
  pivot_longer(cols = -Accepted_Name_with_Author,  #Pivot all columns except Accepted_Name_with_Author
               names_to = "Ref_Name_Type", values_to = "Ref_Species_Name") %>%
  drop_na(Ref_Species_Name)  #Remove rows where the Species_Name is NA

#The final leftovers have both the original inputted name, which can have errors, and the species name but with the author removed. 
#In general cases of working with these names, they should only be considered with the authority. However here they are being compared to 
#another flora which is verified. Therefore, to incur more matches and reduce manual labour, the bare species names can be matched and then checked 
#by a taxonomic expert to confirm, rather than checking each minor difference in authority name.
 
#Fuzzy join witha 90% similarity to catch differences in spelling and author (this value can be adjusted)
Reference_List_CompLeftovers <- fuzzyjoin::stringdist_full_join(
  Reference_List_Long,
  Final_Leftovers,
  by = c("Ref_Species_Name" = "name"),  
  max_dist = 0.1,                        #90% similar 
  method = "jw"                          #Jaro-Winkler distance method
) %>%
  drop_na(Ref_Species_Name, name)       #Drop rows where there is no match
}
Reference_List_CompLeftovers1<-Reference_List_CompLeftovers %>% select(Ref_Species_Name, original_input_name) #Make cleaner dataframe for manual taxonomic expert check step 

#Reference_List_CompLeftovers1 now has potential matches from the leftovers list. 
cat("Manual check by taxonomic expert required. Please go through Reference_List_CompLeftovers1
    For each matched leftover name, please check if the original input name is the same
    taxa as the Ref_Species_Name. If it is, do nothing. If it is not, then note down the 
    column number.")

cat("Make sure to check for multiple matches (e.g. Carex krausei matching to both Carex krausei
     and Carex krausei subsp. porsildiana - note down any of these row numbers which are incorrect too" )

view(Reference_List_CompLeftovers1)

incorrect_rows <- as.numeric(strsplit(readline(prompt = "Enter the row numbers of incorrect matches (comma-separated): "), ",")[[1]]) #Enter here row numbers for Greenland -1,2,3,4,5,7,8,10,12,13,14,17,19,20,23

#If there are 1 or more rows to be removed
if (length(incorrect_rows) > 0) {
  #then remove them from the dataframe
  Reference_List_CompLeftovers1 <- Reference_List_CompLeftovers1[-incorrect_rows, ]
}
{
Reference_List_CompLeftovers<-Reference_List_CompLeftovers %>% 
  filter(original_input_name %in% Reference_List_CompLeftovers1$original_input_name)

#Now only cases remain where the taxonomic expert has determined that these are matches. 

#First, remove these names from the leftover list
Final_Leftovers <- Final_Leftovers %>% 
  select(name, original_input_name) %>%
  filter(!original_input_name %in% Reference_List_CompLeftovers$original_input_name) %>%
  distinct()
#Now these are names not found in the external validation 

#Now the Leftover Names which were found in the reference flora can be appended into it and later into the species list. 
Reference_List_CompLeftovers<- Reference_List_CompLeftovers %>%
  select(Accepted_Name_with_Author, original_input_name)

#Add a unique row identifier
Reference_List_CompLeftovers <- Reference_List_CompLeftovers %>%
  rowid_to_column(var = "row_id")

#Pivot the data to wide format
Reference_List_CompLeftovers <- Reference_List_CompLeftovers %>%
  pivot_wider(
    id_cols = Accepted_Name_with_Author,
    names_from = row_id,
    values_from = original_input_name,
    names_prefix = "original_input_name_"
  )

Reference_List_CompLeftovers <- as.data.frame(t(apply(Reference_List_CompLeftovers, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
colnames(Reference_List_CompLeftovers)[1] <- "Accepted_Name_with_Author"
colnames(Reference_List_CompLeftovers) <- sub("V", "", colnames(Reference_List_CompLeftovers))

Reference_List_CompLeftovers<-Reference_List_CompLeftovers %>% 
  select(where(~ !all(is.na(.)))) %>%
  rename_with(~ paste0('original_input_name_', seq_along(.) - 1 + 1), -1) #Ignores first column but starts naming with 1

Reference_List<-left_join(Reference_List,Reference_List_CompLeftovers, by="Accepted_Name_with_Author" )

#Now the reference list contains the reference Accepted name, its synonyms, and also any leftover names which were picked up. 
#Now the reference list can be compared to the species list. 
}

save.image(file = "Taxa_List_Save_17.RData") #Save everything 
#load("Taxa_List_Save_17.RData")
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#External Validation: Step 3 - Comparing the local reference to the species list 

#There are cases where the leftover name will be a synonym of an accepted name, but only according to the reference flora and not POWO. 
#An example is Dryopteris austriaca Schinz & Thell., which according to the reference flora is a synonym of Dryopteris expansa (C.Presl) Fraser-Jenk. & Jermy, 
#but which POWO did not have in its system. In this case, that name can be added back into the master list, along with the other synonyms for that accepted name. 

#There are also cases where Accepted names from the reference flora, such as Braya glabella subsp. purpurascens (R.Br.) Cody, are actually considered synonyms by POWO, 
#where Braya glabella subsp. purpurascens (R.Br.) Cody is a synonym of Braya purpurascens (R.Br.) Bunge ex Ledeb.

#To handle this, the Confirmed_Master_List_Tidy names must be compared to the Reference_List names. 


#First re-create the long reference list, now with leftover names in it.
{
#Create long format of reference database 
Names_Reference_List<- colnames(Reference_List) #Create a list of the column names in the Project_List_Others
Names_Reference_List <- grep('Accepted_Name_with_Author|Synonym_|original_input_name_', Names_Reference_List,value = TRUE) 

Reference_List_Long<- Reference_List %>%           #Select only columns that contain the accepted name and other names 
  select(matches(Names_Reference_List))

Reference_List_Long$Accepted_Name_Backup<- Reference_List_Long$Accepted_Name_with_Author

#Reshape Reference_List into long format
Reference_List_Long <- Reference_List_Long %>%
  pivot_longer(cols = -Accepted_Name_with_Author,  #Pivot all columns except Accepted_Name_with_Author
               names_to = "Ref_Name_Type", values_to = "Ref_Species_Name") %>%
  drop_na(Ref_Species_Name)  #Remove rows where the Species_Name is NA

#Now do the same for the Confirmed_Master_List_Tidy
#Make a vector of all relevant names: #Excluding Accepted_Name_with_Author
Names_Reference_Check<- colnames(Confirmed_Master_List_Tidy) #Create a list of the column names in the Project_List_Others
Names_Reference_Check <- grep('Accepted_Name_with_Author|Additional_Name', Names_Reference_Check,value = TRUE) 

Master_List_Reference_Check<- Confirmed_Master_List_Tidy %>%           #Select only columns that contain the accepted name and other names 
  select(matches(Names_Reference_Check))

Master_List_Reference_Check$Accepted_Name_Backup<- Master_List_Reference_Check$Accepted_Name_with_Author

#Reshape Master_List_Reference_Check into long format
Master_List_Reference_Check <- Master_List_Reference_Check %>%
  pivot_longer(cols = -Accepted_Name_with_Author,  #Pivot all columns except Accepted_Name_with_Author
               names_to = "Name_Type", values_to = "Additional_Species_Name") %>%
  drop_na(Additional_Species_Name) %>% #Remove rows where the Species_Name is NA
  distinct() #Remove any repeated names 

#To compare to the reference list, we need to account for both Accepted Names and Synonyms matching
#But there can also be cases when a reference list name or its synonym match to more than one 
#POWO name or its synonyms, and this needs to be known too, as then the reference list name cannot be used. 
Master_List_Reference_Check <- Master_List_Reference_Check %>%
  mutate_all(~ str_trim(.)) #Twim White Spaces in All of dataframe 

Master_List_Reference_Check<-Master_List_Reference_Check%>%
  distinct()

Reference_List_Long<-Reference_List_Long %>%
  mutate_all(~ str_trim(.)) #Twim White Spaces in All of dataframe 

Reference_List_Long<-Reference_List_Long %>%
  distinct(Accepted_Name_with_Author,Ref_Species_Name)

#Also note that sometimes the reference list can contain taxa names with "auct." as the author. 
#This indicates that there is an issue with the correct author name being attributed to the correct species of that name
#In this case for matching, we will not match these cases because we do not know the context of the "auct."

Reference_List_Long_Storage<-Reference_List_Long

Reference_List_Long$Match_Status<-NA

Reference_List_Long <- Reference_List_Long %>%
  select(Ref_Species_Name, Match_Status)

#First do exact matching, and note that warnings may occur for multiple matches. This is ok. 
#Create a normalized version of the names by removing spaces and accents for matching
Reference_List_Long <- Reference_List_Long %>%
  mutate(Ref_Species_Name_Normalized = stri_replace_all_fixed(stri_trans_general(Ref_Species_Name, "latin-ascii"), " ", ""))

Master_List_Reference_Check <- Master_List_Reference_Check %>%
  mutate(Additional_Species_Name_Normalized = stri_replace_all_fixed(stri_trans_general(Additional_Species_Name, "latin-ascii"), " ", ""))
}

#Now perform the matching based on the normalized names
Reference_List_Long <- Reference_List_Long %>%
  left_join(Master_List_Reference_Check %>%
              select(Additional_Species_Name_Normalized, Accepted_Name_with_Author),  
            by = c("Ref_Species_Name_Normalized" = "Additional_Species_Name_Normalized")) %>%
  #Clean up by removing the temporary columns used for matching
  select(-Ref_Species_Name_Normalized)
{
Master_List_Reference_Check<-Master_List_Reference_Check%>%
  select(-Additional_Species_Name_Normalized)

Reference_List_Long <- Reference_List_Long %>%
  mutate(Match_Status = case_when(
    !is.na(Accepted_Name_with_Author) ~ "MATCH",  #If Accepted_Name_with_Author is not NA, set Match_Status to MATCH
    stri_detect_fixed(Ref_Species_Name, " auct.") ~ "NO_MATCH",  #If Ref_Species_Name contains " auct.", set Match_Status to NO_MATCH
    TRUE ~ NA_character_  #All other cases remain NA
  ))

#Check for cases of Aggregate, Morph, and Taxon, as these are unlikely to be in a POWO Taxa List
#but possibly in a reference flora, and will automatically not match. 

#Morph
Reference_List_Long <- Reference_List_Long %>%
  mutate(Match_Status = case_when(
    #Check if " morph " is NOT found in Master_List_Reference_Check$Additional_Species_Name
    !any(stri_detect_fixed(Master_List_Reference_Check$Additional_Species_Name, " morph ")) & 
      stri_detect_fixed(Ref_Species_Name, " morph ") ~ "NO_MATCH",  #If " morph " is in Ref_Species_Name, set NO_MATCH
    TRUE ~ Match_Status  
  ))
#Aggregate
Reference_List_Long <- Reference_List_Long %>%
  mutate(Match_Status = case_when(
    !any(stri_detect_fixed(Master_List_Reference_Check$Additional_Species_Name, " aggregate")) & 
      stri_detect_fixed(Ref_Species_Name, " aggregate") ~ "NO_MATCH",  
    TRUE ~ Match_Status  
  ))
#Taxon
Reference_List_Long <- Reference_List_Long %>%
  mutate(Match_Status = case_when(
    !any(stri_detect_fixed(Master_List_Reference_Check$Additional_Species_Name, " taxon ")) & 
      stri_detect_fixed(Ref_Species_Name, " taxon ") ~ "NO_MATCH",  
    TRUE ~ Match_Status  
  ))

#Now store cases which were automatically edited:
Reference_List_Long_Automated<-Reference_List_Long%>%
  filter(!is.na(Match_Status)) %>% distinct()

Reference_List_Long<-Reference_List_Long%>%
  filter(is.na(Match_Status))

#Now fuzzy match the remaining cases, and manual checking is required. Takes some time to run. 
matched_names <- Reference_List_Long %>%
  fuzzy_inner_join(
    Master_List_Reference_Check,
    by = c("Ref_Species_Name" = "Additional_Species_Name"),
    match_fun = function(x, y) stringdist::stringdist(x, y, method = "jw") <= 0.5  #Jaro-Winkler distance
  )

}

#Keep only the best match for each entry (sometimes more than one occur if both equally matching)
best_matches <- matched_names %>%
  group_by(Ref_Species_Name) %>%
  slice_min(order_by = stringdist::stringdist(Ref_Species_Name, Additional_Species_Name, method = "jw"), n = 1) %>%
  ungroup()

{
matched_names_check <- best_matches %>%
  select(Ref_Species_Name,Additional_Species_Name, Accepted_Name_with_Author = Accepted_Name_with_Author.y)

matched_names_check$Ref_Species_Name <- trimws(gsub("\u00A0", " ", matched_names_check$Ref_Species_Name))
matched_names_check$Additional_Species_Name <- trimws(gsub("\u00A0", " ", matched_names_check$Additional_Species_Name))

#Check if Ref_Species_Name contains " subsp. " and Additional_Species_Name doesn't match, then delete the Additional_Species_Name as we 
#already know it is not a match 
matched_names_check$Additional_Species_Name[grepl(" subsp. ", matched_names_check$Ref_Species_Name) & 
                                              !grepl(" subsp. ", matched_names_check$Additional_Species_Name)] <- NA
matched_names_check$Additional_Species_Name[grepl(" var. ", matched_names_check$Ref_Species_Name) & 
                                              !grepl(" var. ", matched_names_check$Additional_Species_Name)] <- NA
matched_names_check$Additional_Species_Name[grepl(" f. ", matched_names_check$Ref_Species_Name) & 
                                              !grepl(" f. ", matched_names_check$Additional_Species_Name)] <- NA

matched_names_check<-matched_names_check%>%distinct() #Remove exact same rows 

matched_names_check<-matched_names_check%>%
  drop_na(Additional_Species_Name) #If this is NA, there is no reasonable match so drop anyways 

}

cat("Now matched_names_check will be downloaded as an Excel File called 'Reference_List_Match_Check.xlsx'. Please check it and when Ref_Species_Name does NOT match to Additional_Species_Name, please DELETE row or all text in that row.")



write.xlsx(matched_names_check, "Reference_List_Match_Check.xlsx")



save.image(file = "Taxa_List_Save_18.RData") #Save everything 
#load("Taxa_List_Save_18.RData")



#Now read in edited one
matched_names_check <- read_excel("Reference_List_Match_Check.xlsx")




#Remove rows with any NA values
matched_names_check <- na.omit(matched_names_check)

Reference_List_Long_Storage_Names<-Reference_List_Long_Storage %>%
  select(Accepted_Name_with_Author, Ref_Species_Name) %>%
  left_join(matched_names_check, by="Ref_Species_Name")

Reference_List_Long_Storage_Names<-Reference_List_Long_Storage_Names%>%
  rename(Accepted_Name_with_Author=Accepted_Name_with_Author.x) %>%
  left_join(Reference_List_Long_Automated, by="Ref_Species_Name", relationship = "many-to-many") #Set as many to many because there are sometimes duplicate synonyms such as "Cerastium vulgatum L." which match to more than one accepted name

Reference_List_Long_Storage_Names <- Reference_List_Long_Storage_Names %>%
  mutate(Accepted_Name_with_Author = coalesce(Accepted_Name_with_Author.y, Accepted_Name_with_Author.y.y)) %>%
  select(-Accepted_Name_with_Author.y, -Accepted_Name_with_Author.y.y) 

Reference_List_Long_Storage_Names <- Reference_List_Long_Storage_Names %>%
  mutate(Match_Status = ifelse(!is.na(Accepted_Name_with_Author) & is.na(Match_Status), "MATCH", Match_Status))

Reference_List_Long_Storage_Names<-Reference_List_Long_Storage_Names%>%
  select(-Additional_Species_Name, -Ref_Species_Name)

Reference_List_Long_Storage_Names <- Reference_List_Long_Storage_Names %>%
  mutate(Match_Status = ifelse(is.na(Match_Status), "NO_MATCH", Match_Status))

Reference_List_Long_Storage_Names <- Reference_List_Long_Storage_Names %>%
  group_by(Accepted_Name_with_Author.x) %>%
  mutate(Match_Status = ifelse(any(Match_Status == "MATCH"), "MATCH", Match_Status)) %>%
  ungroup()

Reference_List_Long_Storage_Names <- Reference_List_Long_Storage_Names %>%
  filter(!(Match_Status == "MATCH" & is.na(Accepted_Name_with_Author)))

Reference_List_Long_Storage_Names<-Reference_List_Long_Storage_Names%>%
  distinct()

#Reference_List_Long_Storage_Names now contains each name and what it matches to. 
#However there are some names which will have matched to multiple POWO Accepted Names,
#We cannot use these names

Ref_Names_Multiple_POWO <- Reference_List_Long_Storage_Names %>%
  group_by(Accepted_Name_with_Author.x) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  select(Accepted_Name_with_Author.x) %>% distinct() #64 names double or more matched

Reference_List_Long_Storage_Names<-Reference_List_Long_Storage_Names%>%
  filter(!Accepted_Name_with_Author.x %in% Ref_Names_Multiple_POWO$Accepted_Name_with_Author.x)

#For use in later code:
#Create a function to remove spaces and accents
normalize_names <- function(name) {
    name %>%
      str_replace_all(" ", "") %>%  #Remove all spaces in the string 
      str_replace_all("×", "×") %>%  #Keep the multiplication sign
      stri_trans_general("Latin-ASCII") %>%  #Remove all accents in the string 
      str_replace_all("\\*", "×")  #Replace asterisk back to multiplication sign 
  } 
  
save.image(file = "Taxa_List_Save_19.RData") #Save everything 
#load("Taxa_List_Save_19.RData")

#Extract the cases of multiple matches to annotate the Confirmed Master List Species with these multiple matches:

#First extract multiple matches from the manual match check step
{
if (nrow(matched_names_check) > 0) {
Multiple_Match_Annotation_1<-matched_names_check%>%
  filter(Ref_Species_Name %in% Reference_List_Long_Storage$Ref_Species_Name) %>%
  select(-Additional_Species_Name)

Multiple_Match_Annotation_1 <- left_join(
  Multiple_Match_Annotation_1, 
  Reference_List_Long_Storage %>% rename(Reference_Accepted_Name_with_Author = Accepted_Name_with_Author), 
  by = "Ref_Species_Name") %>%
  select(-Ref_Species_Name) %>%
  distinct()
}

#Now extract multiple matches from the automated match check step
if (nrow(Reference_List_Long_Automated) > 0) {
Multiple_Match_Annotation_2 <- Reference_List_Long_Automated %>%
  left_join(
    Reference_List_Long_Storage %>% rename(Reference_Accepted_Name_with_Author = Accepted_Name_with_Author), 
    by = "Ref_Species_Name") %>%
  select(-Ref_Species_Name, -Match_Status) %>%
  drop_na(Accepted_Name_with_Author) %>%
  distinct() %>%
  filter(Reference_Accepted_Name_with_Author %in% Ref_Names_Multiple_POWO$Accepted_Name_with_Author.x)
}

Multiple_Match_Annotation <- bind_rows(Multiple_Match_Annotation_1, Multiple_Match_Annotation_2) 

if (nrow(Multiple_Match_Annotation) > 0) {
Multiple_Match_Annotation<-Multiple_Match_Annotation%>%distinct() %>%
  filter(Reference_Accepted_Name_with_Author %in% Ref_Names_Multiple_POWO$Accepted_Name_with_Author.x)}

#Multiple_Match_Annotation now contains the matching POWO Accepted Names for which a given Reference Flora Accepted Name has matched
}

{
#Ref_Names_Multiple_POWO contains Reference Flora Accepted Names which match to more than one POWO name. 
#These names must be removed, and cannot be used. They can be stored as an output. 
Ref_Names_Multiple_POWO_Publish <- Ref_Names_Multiple_POWO %>%
  rename(original_input_name = Accepted_Name_with_Author.x)

Ref_Names_Multiple_POWO_Publish$Verified_Name<-Ref_Names_Multiple_POWO_Publish$original_input_name
Ref_Names_Multiple_POWO_Publish$Status<-"Reference_List_Multiple_POWO_Matches"

Ref_Names_Multiple_POWO_Publish <- Ref_Names_Multiple_POWO_Publish %>%
  mutate(rank = NA_character_) %>% 
  mutate(rank = case_when(
    str_detect(rank, "^[A-Z]+$") ~ NA_character_,  #Replace all-uppercase taxonRanks with NA
    TRUE ~ rank
  )) %>%
  mutate(rank = case_when(
    str_detect(Verified_Name, "× ") ~ "Hybrid",
    str_detect(Verified_Name, "^X ") ~ "Hybrid",
    str_detect(Verified_Name, " [×xX] ") ~ "Hybrid",
    str_detect(Verified_Name, " subsp\\. ") ~ "Subspecies",
    str_detect(Verified_Name, " var\\. ") ~ "Variety",
    str_detect(Verified_Name, " subvar\\. ") ~ "Subvariety",
    str_detect(Verified_Name, " morph") ~ "Morph",
    str_detect(Verified_Name, " f\\. ") ~ "Form",
    str_detect(Verified_Name, " subf\\. ") ~ "Subform",
    str_detect(Verified_Name, " agg\\. ") ~ "Aggregate",
    str_detect(Verified_Name, " aggregate") ~ "Aggregate",
    str_detect(Verified_Name, " sect\\. ") ~ "Section",
    str_detect(Verified_Name, " subsect\\. ") ~ "Subsection",
    str_detect(Verified_Name, " ser\\. ") ~ "Series",
    str_detect(Verified_Name, " subser\\. ") ~ "Subseries",
    str_detect(Verified_Name, " tr\\. ") ~ "Tribe",
    str_detect(Verified_Name, " subtrib\\. ") ~ "Subtribe",
    str_detect(Verified_Name, " gen\\. ") ~ "Genus",
    str_detect(Verified_Name, " subg\\. ") ~ "Subgenus",
    str_detect(Verified_Name, " fam\\. ") ~ "Family",
    str_detect(Verified_Name, "^[A-Z][a-zà-öø-ÿ]+eae\\b") ~ "Family", #If the first word ends with eae
    str_detect(Verified_Name, " subfam\\. ") ~ "Subfamily",
    str_detect(Verified_Name, " ord\\. ") ~ "Order",
    str_detect(Verified_Name, " subord\\. ") ~ "Suborder",
    str_detect(Verified_Name, " cl\\. ") ~ "Class",
    str_detect(Verified_Name, " subcl\\. ") ~ "Subclass",
    TRUE ~ rank  #Keep remaining ranks unchanged
  )) %>%
  #Assign rank for cases where it is still NA
  mutate(rank = case_when(
    is.na(rank) & str_count(Verified_Name, "\\s") >= 1 & 
      str_detect(Verified_Name, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species",  
    is.na(rank) & str_count(Verified_Name, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ rank  
  ))

#Write to Excel File:
#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("ReferenceFlora_Removed_Names_", today_date, ".xlsx")

#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)

#Write to Excel file
write_xlsx(Ref_Names_Multiple_POWO_Publish, file_name)

#Set the working directory back to the original directory
setwd("..")

#Append on all the synonym names to the Reference_List_Accepted_Name.
Reference_List_Long_Storage_Names<-Reference_List_Long_Storage_Names%>%
  rename(Reference_List_Accepted_Name=Accepted_Name_with_Author.x) %>%
  left_join(Reference_List, by= c("Reference_List_Accepted_Name" = "Accepted_Name_with_Author"))

Reference_List_Long_Storage_Names_Umnatched<-Reference_List_Long_Storage_Names%>% #Now contains only non-matching names to taxa list
  filter(Match_Status=="NO_MATCH") %>%
  select(-Accepted_Name_with_Author,-Match_Status)

Reference_List_Long_Storage_Names<-Reference_List_Long_Storage_Names%>% #Now contains only matching names to taxa list
  filter(Match_Status=="MATCH") %>%
  select(-Match_Status)

patterns <- "Accepted_Name_with_Author|Reference_List_Accepted_Name|Synonym_[1-9]|original_input_name_[1-9]"

Reference_List_Long_Storage_Names <- Reference_List_Long_Storage_Names %>%
  select(Accepted_Name_with_Author, matches(patterns))%>%
  select(where(~ !all(is.na(.)))) %>%
  rename_with(~ paste0('reference_flora_name', seq_along(.) - 1 + 1), -1) #Ignores first column but starts naming with 1

#Now shift all names to the left to fill in NA cells (tidy it up)
Reference_List_Long_Storage_Names <- as.data.frame(t(apply(Reference_List_Long_Storage_Names, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
colnames(Reference_List_Long_Storage_Names)[1] <- "Accepted_Name_with_Author"
colnames(Reference_List_Long_Storage_Names) <- sub("V", "", colnames(Reference_List_Long_Storage_Names))
Reference_List_Long_Storage_Names<-Reference_List_Long_Storage_Names %>% 
  rename_at(vars(2:ncol(Reference_List_Long_Storage_Names)), ~paste0('reference_flora_name',.)) %>% #Rename Input Name Columns 
  select(where(~!all(is.na(.x)))) #Remove for example any columns where all rows are NA.

#Pivot data 
Reference_List_Long_Storage_Names <- Reference_List_Long_Storage_Names %>%
  pivot_longer(
    cols = starts_with("reference_flora_name"),
    names_to = "Reference_Flora_Type",     #Name for the new column that holds the reference_flora_name column names
    values_to = "Synonym") %>%
  select(Accepted_Name_with_Author, Synonym) %>%
  drop_na(Synonym) %>%
  group_by(Accepted_Name_with_Author) %>%              #Group by Accepted_Name_with_Author
  mutate(Synonym_ID = paste0("reference_flora_name", row_number())) %>%  #Create a unique identifier for each synonym
  ungroup() %>%
  pivot_wider(
    names_from = Synonym_ID,     #Use the Synonym_ID to create new column names
    values_from = Synonym        #Use the Synonym values as the data in the new columns
  )

#Make sure matching done without duplicate matches to an Accepted_Name_with_Author
duplicates <- Reference_List_Long_Storage_Names %>%
  group_by(Accepted_Name_with_Author) %>%
  filter(n() > 1) %>%
  pull(Accepted_Name_with_Author) %>%
  unique()

#Print message based on whether duplicates were found
if(length(duplicates) > 0) {
  cat("Multiple matches of reference flora to POWO Accepted Name with Author. Please check the reference flora: \n")
  cat(paste(duplicates, collapse = ", "), "\n")
} else {
  cat("No duplicate matches found, please proceed.\n")
}
}
#Reference_List_Long_Storage_Names now contains the Accepted_Name_with_Author according to the main species list and POWO, 
#and all reference list Accepted names and their synonyms, which are themselves a synonym to the Accepted_Name_with_Author.
#Now they can be added into the main species list. 
#But also mark them, so we can distinguish that these species are found in both data sources. 
{
Reference_List_Long_Storage_Names$Data_Source<-"GBIF_and_Reference_Flora"
#Now left join the Confirmed_Master_List_Tidy and the Reference_List_Long_Storage_Names
Confirmed_Master_List_Tidy <- left_join(Confirmed_Master_List_Tidy, Reference_List_Long_Storage_Names, by = "Accepted_Name_with_Author")
#Change Data_Source NA to GBIF for remaining species. 
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(Data_Source = ifelse(is.na(Data_Source), "GBIF", Data_Source))


}
#Reference_List_Long_Storage_Names_Umnatched contains all taxa names not found in the Confirmed_Master_List_Tidy

save.image(file = "Taxa_List_Save_20.RData") #Save everything 
#load("Taxa_List_Save_20.RData")

#Prepare for data integration 

#Remove columns if they match "RANK", "rank", or "Rank"
Reference_List_Long_Storage_Names_Umnatched <- Reference_List_Long_Storage_Names_Umnatched %>%
  select(-matches("^RANK$|^rank$|^Rank$"))

Reference_List_Long_Storage_Names_Umnatched <- Reference_List_Long_Storage_Names_Umnatched %>%
  select(where(~ !all(is.na(.))))

Reference_List_Long_Storage_Names_Umnatched<-Reference_List_Long_Storage_Names_Umnatched%>%
  rename(Accepted_Name_with_Author=Reference_List_Accepted_Name)

Reference_List_Long_Storage_Names_Umnatched <- Reference_List_Long_Storage_Names_Umnatched %>%
  mutate(rank = case_when(
    str_detect(Accepted_Name_with_Author, " subsp\\.") ~ "Subspecies",
    str_detect(Accepted_Name_with_Author, " var\\.") ~ "Variety",
    str_detect(Accepted_Name_with_Author, " subvar\\.") ~ "Subvariety",
    str_detect(Accepted_Name_with_Author, " morph") ~ "Morph",
    str_detect(Accepted_Name_with_Author, " f\\.") ~ "Form",
    str_detect(Accepted_Name_with_Author, " subf\\.") ~ "Subform",    
    str_detect(Accepted_Name_with_Author, " agg\\.") ~ "Aggregate",
    str_detect(Accepted_Name_with_Author, " aggregate") ~ "Aggregate",
    str_detect(Accepted_Name_with_Author, " sect\\.") ~ "Section",
    str_detect(Accepted_Name_with_Author, " subsect\\.") ~ "Subsection",
    str_detect(Accepted_Name_with_Author, " ser\\.") ~ "Series",
    str_detect(Accepted_Name_with_Author, " subser\\.") ~ "Subseries",
    str_detect(Accepted_Name_with_Author, " tr\\.") ~ "Tribe",
    str_detect(Accepted_Name_with_Author, " subtrib\\.") ~ "Subtribe",
    str_detect(Accepted_Name_with_Author, " gen\\.") ~ "Genus",
    str_detect(Accepted_Name_with_Author, " subg\\.") ~ "Subgenus",
    str_detect(Accepted_Name_with_Author, " fam\\.") ~ "Family",
    str_detect(Accepted_Name_with_Author, " subfam\\.") ~ "Subfamily",
    str_detect(Accepted_Name_with_Author, " ord\\.") ~ "Order",
    str_detect(Accepted_Name_with_Author, " subord\\.") ~ "Suborder",
    str_detect(Accepted_Name_with_Author, " cl\\.") ~ "Class",
    str_detect(Accepted_Name_with_Author, " subcl\\.") ~ "Subclass",
    str_detect(Accepted_Name_with_Author, "× ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, "^X ") ~ "Hybrid",
    TRUE ~ NA_character_  #Replace with NA for cases where none of the above conditions are met
  )) %>%
  #Assign rank for cases where it is still NA
  mutate(rank = case_when(
    is.na(rank) & str_count(Accepted_Name_with_Author, "\\s") >= 1 & 
      str_detect(Accepted_Name_with_Author, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species", #Detect cases when first word is capitalized and second word is not, label as species, also accounting for accented names 
    is.na(rank) & str_count(Accepted_Name_with_Author, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ rank
  ))
                          
Reference_List_Long_Storage_Names_Umnatched$Data_Source<-"Reference_Flora"               
Reference_List_Long_Storage_Names_Umnatched <- Reference_List_Long_Storage_Names_Umnatched %>%
  select(Accepted_Name_with_Author,rank,Data_Source,starts_with("Synonym_"), starts_with("original_input_name_"))

suppressPackageStartupMessages(library(plyr))
Confirmed_Master_List_Tidy <-rbind.fill(Confirmed_Master_List_Tidy,Reference_List_Long_Storage_Names_Umnatched)
detach(package:plyr)
library("dplyr")
{
#Now tidy up Confirmed_Master_List_Tidy
#Select dataframe only including names and no other data descriptors 
Confirmed_Master_List_Tidy_Names<-Confirmed_Master_List_Tidy %>%
  select(Accepted_Name_with_Author,starts_with("Additional_Name"), starts_with("reference_flora_name"), starts_with("Synonym_"), starts_with("original_input_name_"))

#Now shift all names to the left to fill in NA cells (tidy it up)
Confirmed_Master_List_Tidy_Names <- as.data.frame(t(apply(Confirmed_Master_List_Tidy_Names, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
colnames(Confirmed_Master_List_Tidy_Names)[1] <- "Accepted_Name_with_Author"
colnames(Confirmed_Master_List_Tidy_Names) <- sub("V", "", colnames(Confirmed_Master_List_Tidy_Names))
Confirmed_Master_List_Tidy_Names<-Confirmed_Master_List_Tidy_Names %>% 
  rename_at(vars(2:ncol(Confirmed_Master_List_Tidy_Names)), ~paste0('Additional_Name_',.)) %>% #Rename Input Name Columns 
  select(where(~!all(is.na(.x)))) #Remove for example any columns where all rows are NA.

#Now over the many times the data has been amalgamated, there are cases where some of the additional names are duplicates. 
#Clean this up too, by pivoting the data to long format and removing duplicate cases, then pivoting back. 

#Pivot all columns except "Accepted_Name_with_Author" into long format
Confirmed_Master_List_Tidy_Names <- Confirmed_Master_List_Tidy_Names %>%
  pivot_longer(
    cols = -Accepted_Name_with_Author, #Exclude the column "Accepted_Name_with_Author"
    names_to = "Additional_Name_Number",               #New column for the names of the original columns
    values_to = "Additional_Name"                  #New column for the values from those columns
  ) %>%
  select(Accepted_Name_with_Author, Additional_Name) %>%
  filter(!is.na(Additional_Name)) %>%
  distinct(Accepted_Name_with_Author, Additional_Name)

missing_additional_name <- Confirmed_Master_List_Tidy %>%
  filter(!Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy_Names$Accepted_Name_with_Author) %>%
  select(Accepted_Name_with_Author)

new_rows <- missing_additional_name %>%
  mutate(Additional_Name = Accepted_Name_with_Author)

Confirmed_Master_List_Tidy_Names<-rbind(Confirmed_Master_List_Tidy_Names,new_rows) 

Confirmed_Master_List_Tidy_Names<-Confirmed_Master_List_Tidy_Names%>%distinct()

#Add indet. into Genus level names - we know these are names confirmed to the Genus level
genus_matches_forIndet <- Confirmed_Master_List_Tidy %>%
  filter(rank %in% c("GENUS", "Genus", "genus") & !hybrid)

#Create indet rows to bind in
genus_matches_forIndet_Addition <- Confirmed_Master_List_Tidy_Names %>%
  left_join(genus_matches_forIndet, by = "Accepted_Name_with_Author") %>%
  filter(!is.na(genus)) %>%
  mutate(Additional_Name = paste(genus, "indet.")) %>%
  bind_rows(mutate(., Additional_Name = paste(genus, "indet"))) %>%
  select(Accepted_Name_with_Author, Additional_Name) %>%
  distinct() %>%
  filter(!Additional_Name %in% Final_Leftovers$original_input_name) %>%
  mutate(Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author),  
    Additional_Name = str_trim(Additional_Name))
  
Confirmed_Master_List_Tidy_Names<-rbind(Confirmed_Master_List_Tidy_Names,genus_matches_forIndet_Addition)

#Add in copy of Accepted Name as Synonym for future use

Accepted_Name_Rows <- Confirmed_Master_List_Tidy_Names %>%
  mutate(Additional_Name = Accepted_Name_with_Author) %>%  
  select(Accepted_Name_with_Author, Additional_Name) %>% distinct()

Confirmed_Master_List_Tidy_Names<-rbind(Confirmed_Master_List_Tidy_Names,Accepted_Name_Rows)

Confirmed_Master_List_Tidy_Names<-Confirmed_Master_List_Tidy_Names%>% distinct()

Confirmed_Master_List_Tidy_Names <- Confirmed_Master_List_Tidy_Names %>%
  mutate(Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author),  
    Additional_Name = str_trim(Additional_Name))

Confirmed_Master_List_Tidy_Names<-Confirmed_Master_List_Tidy_Names%>%distinct()

#Sometimes through joining processes, some names can be incorrectly joined to multiple other names.
#But sometimes, these are also correct. 
#For example, Pedicularis flammea Oeder joining to both Pedicularis flammea L. and Pedicularis oederi Vahl is wrong.
#But Cerastium vulgatum L. joining to two Accepted Names is correct. 

#As a fail safe, here we check for these cases and remove any we know to be wrong. 
Dup_Check_POWO<-df_total2 %>% #Where these cases are most often found
  drop_na(author) %>%
  mutate(Name_with_Author = paste(name, author, sep = " ")) %>%
  mutate(Accepted_Name_with_Author = paste(synonymOf_name, synonymOf_author, sep = " ")) %>%
  group_by(Name_with_Author) %>% #Remove duplicate cases, which are legitimate 
  filter(n() == 1) %>%
  ungroup()

Dup_Check_POWO <- Dup_Check_POWO %>%
  mutate(Accepted_Name_with_Author = if_else(is.na(synonymOf_author), 
                                             synonymOf_name, 
                                             Accepted_Name_with_Author))

duplicate_names <- Confirmed_Master_List_Tidy_Names %>%
  group_by(Additional_Name) %>%
  filter(n() > 1) %>%
  select(Additional_Name, Accepted_Name_with_Author) %>%
  ungroup() %>%
  distinct()

duplicate_names$Accepted_Name_with_Author <- trimws(duplicate_names$Accepted_Name_with_Author)

filtered_duplicate_names <- duplicate_names %>%
  inner_join(Dup_Check_POWO, by = c("Additional_Name" = "Name_with_Author", 
                                    "Accepted_Name_with_Author" = "Accepted_Name_with_Author"))

duplicate_names<-duplicate_names%>%filter(Additional_Name %in% filtered_duplicate_names$Additional_Name) %>% #Only keep cases which are not on POWO and due to our process
  filter(!Accepted_Name_with_Author %in% filtered_duplicate_names$Accepted_Name_with_Author)

Confirmed_Master_List_Tidy_Names <- Confirmed_Master_List_Tidy_Names %>%
  anti_join(duplicate_names, 
            by = c("Additional_Name" = "Additional_Name", 
                   "Accepted_Name_with_Author" = "Accepted_Name_with_Author"))

Confirmed_Master_List_Tidy_Names <- Confirmed_Master_List_Tidy_Names %>%
  mutate(Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author),Additional_Name = str_trim(Additional_Name)) %>%
  distinct(Accepted_Name_with_Author, Additional_Name, .keep_all = TRUE)

Confirmed_Master_List_Tidy_Names<-Confirmed_Master_List_Tidy_Names%>%distinct()

Confirmed_Master_List_Tidy_Names_Storage<-Confirmed_Master_List_Tidy_Names

#Calculate the total number of unique names
total_names <- nrow(Confirmed_Master_List_Tidy_Names)
#Print the message 
cat(sprintf("Fun Fact! Your taxa list has a total of %d synonyms in it! This includes all uniquely spelled Synonyms!\n", total_names))
}

{
#Now pivot the names back to long format, and append them back into the species list with the descriptor data
Confirmed_Master_List_Tidy_Names <- Confirmed_Master_List_Tidy_Names %>%
  group_by(Accepted_Name_with_Author) %>%
  mutate(name_id = row_number()) %>%  #Unique ID for each Additional_Name within the same Accepted_Name_with_Author
  pivot_wider(
    names_from = name_id,
    values_from = Additional_Name,
    names_prefix = "Additional_Name_"
  ) %>%
  ungroup()  #Remove the grouping

#Select dataframe only data descriptors and accepted name and also drop some other columns not needed 
Confirmed_Master_List_Tidy_Descriptors <- Confirmed_Master_List_Tidy %>%
  select(-c(starts_with("Additional_Name"), starts_with("reference_flora_name"), starts_with("Synonym_"), starts_with("original_input_name_"), response, queryId))

#Small edit to hybrids 
Confirmed_Master_List_Tidy_Descriptors <- Confirmed_Master_List_Tidy_Descriptors %>%
  mutate(rank = if_else(hybrid == 1, "Hybrid", rank))

Confirmed_Master_List_Tidy_Descriptors <- Confirmed_Master_List_Tidy_Descriptors %>%
  mutate(across(where(is.character), ~ str_trim(., side = "both")))

Confirmed_Master_List_Tidy_Names <- Confirmed_Master_List_Tidy_Names %>%
  mutate_all(~ str_trim(., side = "both"))

#Left join back together
Confirmed_Master_List_Tidy<-NA 
Confirmed_Master_List_Tidy<-left_join(Confirmed_Master_List_Tidy_Descriptors, Confirmed_Master_List_Tidy_Names, by="Accepted_Name_with_Author" )

#Repair any missing rank data:
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(rank = case_when(
    is.na(rank) & str_detect(Accepted_Name_with_Author, " subsp\\.") ~ "Subspecies",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " var\\.") ~ "Variety",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " subvar\\.") ~ "Subvariety",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " morph") ~ "Morph",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " f\\.") ~ "Form",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " subf\\.") ~ "Subform",    
    is.na(rank) & str_detect(Accepted_Name_with_Author, " agg\\.") ~ "Aggregate",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " aggregate") ~ "Aggregate",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " sect\\.") ~ "Section",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " subsect\\.") ~ "Subsection",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " ser\\.") ~ "Series",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " subser\\.") ~ "Subseries",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " tr\\.") ~ "Tribe",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " subtrib\\.") ~ "Subtribe",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " gen\\.") ~ "Genus",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " subg\\.") ~ "Subgenus",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " fam\\.") ~ "Family",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " subfam\\.") ~ "Subfamily",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " ord\\.") ~ "Order",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " subord\\.") ~ "Suborder",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " cl\\.") ~ "Class",
    is.na(rank) & str_detect(Accepted_Name_with_Author, " subcl\\.") ~ "Subclass",
    is.na(rank) & str_detect(Accepted_Name_with_Author, "× ") ~ "Hybrid",
    is.na(rank) & str_detect(Accepted_Name_with_Author, "^X ") ~ "Hybrid",
    TRUE ~ rank  #Leave as is if the rank is not NA
  )) %>%
  #Assign rank for cases where it is still NA
  mutate(rank = case_when(
    is.na(rank) & str_count(Accepted_Name_with_Author, "\\s") >= 1 & 
      str_detect(Accepted_Name_with_Author, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species", #Detect cases when first word is capitalized and second word is not, label as species, also accounting for accented names 
    is.na(rank) & str_count(Accepted_Name_with_Author, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ rank
  ))

#Tidy up the rank information
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(rank = str_to_title(rank))

#Tidy up the antonym information 
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(Autonym = sapply(Accepted_Name_with_Author, search_autonym))  #Apply detect_autonym to Accepted Name in the confirmed master list 
}

save.image(file = "Taxa_List_Save_21.RData") #Save everything 
#load("Taxa_List_Save_21.RData")

###################################################################################
###################################################################################
###################################################################################
###################################################################################
############# Internal Validation and Linking back to GBIF Input Data #############
###################################################################################
###################################################################################
###################################################################################
###################################################################################
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Internal Validation: Step 1 - Linking Accepted Names back to GBIF data 

#Note that there are cases, albeit rare, when using the POWO backbone that there is a taxa name
#which even with the same author name, it belongs to multiple taxa accepted names. An example is 
#Cerastium vulgatum L., which is a synonym of both Cerastium glomeratum subsp. glomeratum and Cerastium holosteoides Fr.
{
#For any GBIF records which use this name, there will therefore be multiple hits when trying to match. 
#To deal with this, first identify duplicate synonym names:
Duplicate_POWO_Synonyms <- Confirmed_Master_List_Tidy_Names_Storage %>%
  group_by(Additional_Name) %>%
  filter(n() > 1) %>%
  select(Additional_Name) %>%
  distinct()
#Now here need to relabel with both names when there is a duplicate 
#Make a combined version of the Accepted_Name_with_Author for each duplicate Additional_Name, 
#so that when the GBIF data is append in, it is clear that there are multiple options for these unique cases
Combined_Names <- Confirmed_Master_List_Tidy_Names_Storage %>%
  filter(Additional_Name %in% Duplicate_POWO_Synonyms$Additional_Name) %>%  #Filter for duplicates
  group_by(Additional_Name) %>%  #Group by the Additional_Name
  mutate(Combined_Accepted_Names = paste(unique(Accepted_Name_with_Author), collapse = " OR ")) %>% #Combine unique Accepted_Name_with_Author
  distinct(Additional_Name, Combined_Accepted_Names)  #Combine unique Accepted_Name_with_Author and separate by OR

#Update Confirmed_Master_List_Tidy_Names_Storage with the combined Accepted_Name_with_Author values, and remove duplicate cases
Confirmed_Master_List_Tidy_Names_Storage <- Confirmed_Master_List_Tidy_Names_Storage %>%
  left_join(Combined_Names, by = "Additional_Name") %>%  #Join the combined names back to the original dataframe
  mutate(Accepted_Name_with_Author = ifelse(!is.na(Combined_Accepted_Names), Combined_Accepted_Names, Accepted_Name_with_Author)) %>%  #Replace Accepted_Name_with_Author with the combined version when needed
  select(-Combined_Accepted_Names) %>%  #Remove the temporary column
  distinct()
}

{
#Use original GBIF data from Combi_List_Storage
#Confirmed_Master_List_Tidy_Names_Storage contains the long pivoted data from the master list. 
#Match to the Combi_List_Storage, but normalize names, so that matches like Carex ×helvola Blytt to Carex × helvola Blytt
#will still work:
Combi_List_Storage <- Combi_List_Storage %>%
    mutate(Accepted_Name_with_Author_Match = Confirmed_Master_List_Tidy_Names_Storage$Accepted_Name_with_Author[match(
        normalize_names(scientificName), normalize_names(Confirmed_Master_List_Tidy_Names_Storage$Additional_Name))]) %>%
    relocate(c(Accepted_Name_with_Author_Match, scientificName), .before = 1) %>%  #Reorder columns
    rename(Accepted_Name_with_Author = Accepted_Name_with_Author_Match)

#Tidy 
Combi_List_Storage$Accepted_Name_with_Author <- gsub("  OR ", " OR ", Combi_List_Storage$Accepted_Name_with_Author)  

Combi_List_Storage_Accepted <- Combi_List_Storage %>%  #Keep all cases where there is a matched Accepted_Name_with_Author
  filter(!is.na(Accepted_Name_with_Author)) 

names(Combi_List_Storage_Accepted)[names(Combi_List_Storage_Accepted) == 'scientificName_backup'] <- 'scientificName'

Combi_List_Storage_Leftover <- Combi_List_Storage %>%  #Keep all cases where there is a matched Accepted_Name_with_Author
  filter(is.na(Accepted_Name_with_Author)) 

#Remove cases of occurrences for Leftover_autonyms_Publish
Combi_List_Storage_Leftover_autonyms_Publish <- Combi_List_Storage_Leftover %>%
  filter(scientificName %in% Leftover_autonyms_Publish$original_input_name) %>%
  mutate(
    Accepted_Name_with_Author = ifelse(
      is.na(Accepted_Name_with_Author) & scientificName %in% Leftover_autonyms_Publish$original_input_name,
      scientificName,
      Accepted_Name_with_Author))

Final_Leftovers<-Final_Leftovers%>% #If also found in Final Leftovers, Autonym takes precedence 
  filter(!original_input_name %in% Leftover_autonyms_Publish$original_input_name)

Combi_List_Storage_Leftover<-Combi_List_Storage_Leftover%>%
  filter(!scientificName %in% Leftover_autonyms_Publish$original_input_name)

#Clean ranks of added in data, where the rank is GENUS or FAMILY but now its is known to a lower taxonomic level:
Combi_List_Storage_Accepted <- Combi_List_Storage_Accepted %>%
  mutate(taxonRank = if_else(taxonRank %in% c("GENUS", "FAMILY"), NA_character_, taxonRank)) %>%
  mutate(taxonRank = case_when(
    is.na(taxonRank) ~ case_when(
      str_detect(Accepted_Name_with_Author, "× ") ~ "Hybrid",
      str_detect(Accepted_Name_with_Author, "^X ") ~ "Hybrid",
      str_detect(Accepted_Name_with_Author, " [×xX] ") ~ "Hybrid",
      str_detect(Accepted_Name_with_Author, " subsp\\. ") ~ "Subspecies",
      str_detect(Accepted_Name_with_Author, " var\\. ") ~ "Variety",
      str_detect(Accepted_Name_with_Author, " subvar\\. ") ~ "Subvariety",
      str_detect(Accepted_Name_with_Author, " morph") ~ "Morph",
      str_detect(Accepted_Name_with_Author, " f\\. ") ~ "Form",
      str_detect(Accepted_Name_with_Author, " subf\\. ") ~ "Subform",
      str_detect(Accepted_Name_with_Author, " agg\\. ") ~ "Aggregate",
      str_detect(Accepted_Name_with_Author, " aggregate") ~ "Aggregate",
      str_detect(Accepted_Name_with_Author, " sect\\. ") ~ "Section",
      str_detect(Accepted_Name_with_Author, " subsect\\. ") ~ "Subsection",
      str_detect(Accepted_Name_with_Author, " ser\\. ") ~ "Series",
      str_detect(Accepted_Name_with_Author, " subser\\. ") ~ "Subseries",
      str_detect(Accepted_Name_with_Author, " tr\\. ") ~ "Tribe",
      str_detect(Accepted_Name_with_Author, " subtrib\\. ") ~ "Subtribe",
      str_detect(Accepted_Name_with_Author, " gen\\. ") ~ "Genus",
      str_detect(Accepted_Name_with_Author, " subg\\. ") ~ "Subgenus",
      str_detect(Accepted_Name_with_Author, " fam\\. ") ~ "Family",
      str_detect(Accepted_Name_with_Author, " subfam\\. ") ~ "Subfamily",
      str_detect(Accepted_Name_with_Author, " ord\\. ") ~ "Order",
      str_detect(Accepted_Name_with_Author, " subord\\. ") ~ "Suborder",
      str_detect(Accepted_Name_with_Author, " cl\\. ") ~ "Class",
      str_detect(Accepted_Name_with_Author, " subcl\\. ") ~ "Subclass",
      TRUE ~ NA_character_  #Keep remaining entries as NA in taxonRank
    ),
    TRUE ~ taxonRank #Retain taxonRank for other cases
  )) %>%
  
  #Assign taxonRank for cases where it is still NA (was originally GENUS or FAMILY)
  mutate(taxonRank = case_when(
    is.na(taxonRank) & str_count(Accepted_Name_with_Author, "\\s") >= 1 & 
      str_detect(Accepted_Name_with_Author, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species", #Detect cases when first word is capitalized and second word is not
    is.na(taxonRank) & str_count(Accepted_Name_with_Author, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ taxonRank #Keep original taxonRank for non-matching cases
  ))

#Also take the Genus or Higher level names from Combi_List_Storage_Leftover, and add into Combi_List_Storage_Accepted
#with appropriate labels, when possible:
#df_total4, df_total4_backup, and df_total6 contain originally queried information regarding Genera and higher ranks. 
#For the Taxa list, only Genera and Families not represented by species or lower ranks were maintained in the list. 
#However, there will be matches still to the original GBIF data for specimens which are of these ranks, but which 
#do have a lower rank of species level or lower in the data. 

df_total4_backup <- df_total4_backup %>%
  mutate(Accepted_Name_with_Author = paste(name, author, sep = " ")) #Now I also have the Accepted Names with their authority names on them. 

suppressPackageStartupMessages(library(plyr))
HigherRanks <-rbind.fill(df_total4,df_total6,df_total4_backup)
detach(package:plyr)
library("dplyr")

HigherRanks <- HigherRanks %>%
  mutate(Accepted_Name_with_Author = ifelse(rank == "Family" & is.na(Accepted_Name_with_Author), 
                                            paste(family, author, sep = " "), 
                                            Accepted_Name_with_Author))

#Take into account any changes in Leftovers Data for Higher Rank Names, such as Downingia,
#where Downingia is not actually a Genus level specimen, it is actually a species 

HigherRanks<-HigherRanks%>%filter(taxonomicStatus != "Unplaced" | is.na(taxonomicStatus)) %>% select(Accepted_Name_with_Author,rank) %>% distinct()

HigherRanks<-HigherRanks %>%
  mutate(Normalized_Name = normalize_names(Accepted_Name_with_Author))

#Now the higher ranks data is also available to match with GBIF specimens. 
Combi_List_Storage_reAdd <- Combi_List_Storage_Leftover %>%
  mutate(Normalized_Name = normalize_names(scientificName)) %>%  #Normalize names for joining
  left_join(HigherRanks, by = "Normalized_Name") %>% #Join by the normalized names
  select(-c(Normalized_Name, taxonRank, Accepted_Name_with_Author.x)) %>%
  filter(!is.na(Accepted_Name_with_Author.y)) %>%
  rename(Accepted_Name_with_Author = Accepted_Name_with_Author.y,
         taxonRank = rank) %>%
  select(Accepted_Name_with_Author, scientificName, everything())

#Now remove from Combi_List_Storage_Leftover
Combi_List_Storage_Leftover<-Combi_List_Storage_Leftover %>%
  filter(!gbifID %in% Combi_List_Storage_reAdd$gbifID)
#Now add back into Combi_List_Storage_Accepted

Combi_List_Storage_Accepted<-rbind(Combi_List_Storage_Accepted,Combi_List_Storage_reAdd)

names(Combi_List_Storage_Leftover)[names(Combi_List_Storage_Leftover) == 'scientificName_backup'] <- 'scientificName'

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Internal Validation: Step 2 - Linking Names without Authors back into Species List - Trying to recover removed specimens with confident matches 

#Confirmed_Master_List_Tidy_Names_Storage contains the long pivoted data 
#Combi_List_TaxonIssue_NO contains the GBIF data where a species name was not registered with an authority, and 
#where there are multiple options of species name with authority, therefore meaning it could not be automated at that time. 

#Now however, with a species list in place, the Combi_List_TaxonIssue_NO names can be checked against names and synonyms which 
#are confidently known to to exist for this given flora already. 

Combi_List_TaxonIssue_NO_CheckNames <- Combi_List_TaxonIssue_NO %>%
  select(verbatimScientificName) %>% distinct() %>%
  filter(verbatimScientificName != "")

#Now we want to join matching cases to Combi_List_TaxonIssue_NO_CheckNames
#Need to account for improper matches, such as Arnica alpina subsp. arnica to Arnica alpina because it is a partial match. 

#First, define additional names at species rank:
Confirmed_Master_List_Tidy_Names_Storage_species <- Confirmed_Master_List_Tidy_Names_Storage %>%
  filter(!str_detect(Additional_Name, " f\\.| var\\.|subsp\\.|subvar\\.|morph|agg\\.|sect\\.|subsect\\.|ser\\.|subser\\.| tr\\.|subtrib\\.|gen\\.|subg\\.|fam\\.|subfam\\.|ord\\.|subord\\.|cl\\.|subcl\\.")) 
#Non-species rank 
Confirmed_Master_List_Tidy_Names_Storage_non_species <- Confirmed_Master_List_Tidy_Names_Storage %>%
  filter(str_detect(Additional_Name, " f\\.| var\\.|subsp\\.|subvar\\.|morph|agg\\.|sect\\.|subsect\\.|ser\\.|subser\\.| tr\\.|subtrib\\.|gen\\.|subg\\.|fam\\.|subfam\\.|ord\\.|subord\\.|cl\\.|subcl\\.")) 

#Do the same to Combi_List_TaxonIssue_NO_CheckNames  
Combi_List_TaxonIssue_NO_CheckNames_species<-Combi_List_TaxonIssue_NO_CheckNames %>%
  filter(!str_detect(verbatimScientificName, " f\\.| var\\.|subsp\\.|subvar\\.|morph|agg\\.|sect\\.|subsect\\.|ser\\.|subser\\.| tr\\.|subtrib\\.|gen\\.|subg\\.|fam\\.|subfam\\.|ord\\.|subord\\.|cl\\.|subcl\\.")) %>%
  filter(str_count(verbatimScientificName, "\\S+") > 1) #Remove cases of one word only, these will be Genera or higher ranks and too unreliable to try and automate 
#Non-species rank 
Combi_List_TaxonIssue_NO_CheckNames_non_species<-Combi_List_TaxonIssue_NO_CheckNames %>%
  filter(str_detect(verbatimScientificName, " f\\.| var\\.|subsp\\.|subvar\\.|morph|agg\\.|sect\\.|subsect\\.|ser\\.|subser\\.| tr\\.|subtrib\\.|gen\\.|subg\\.|fam\\.|subfam\\.|ord\\.|subord\\.|cl\\.|subcl\\.")) %>%
  filter(str_count(verbatimScientificName, "\\S+") > 1) #Remove cases of one word only, these will be Genera or higher ranks and too unreliable to try and automate 

#Now check the species rank to species rank, and the non-species rank to the non-species rank:

#Create empty dataframe 
Matched_results_species <- data.frame(verbatimScientificName = character(),
                                    Additional_Name = character(),
                                    Accepted_Name_with_Author= character(),
                                    stringsAsFactors = FALSE)

#Go through each of the species level names and find any and all matches 
for (name in Combi_List_TaxonIssue_NO_CheckNames_species$verbatimScientificName) {
  #Filter the Additional_Name column to find matches
  matches <- Confirmed_Master_List_Tidy_Names_Storage_species %>%
    filter(str_detect(Additional_Name, fixed(name))) %>%
    select(Additional_Name, Accepted_Name_with_Author) %>%
    distinct()
    #Add the matches to the Matched_results_species
  if (nrow(matches) > 0) {
    matched_rows <- data.frame(verbatimScientificName = name,
                               Additional_Name = matches$Additional_Name,
                               Accepted_Name_with_Author = matches$Accepted_Name_with_Author,
                               stringsAsFactors = FALSE)
    Matched_results_species <- rbind(Matched_results_species, matched_rows)
  }
}

#Create empty dataframe 
Matched_results_nonspecies <- data.frame(verbatimScientificName = character(),
                                      Additional_Name = character(),
                                      Accepted_Name_with_Author= character(),
                                      stringsAsFactors = FALSE)

#Go through each of the species level names and find any and all matches 
for (name in Combi_List_TaxonIssue_NO_CheckNames_non_species$verbatimScientificName) {
  #Filter the Additional_Name column to find matches
  matches <- Confirmed_Master_List_Tidy_Names_Storage_non_species %>%
    filter(str_detect(Additional_Name, fixed(name))) %>%
    select(Additional_Name, Accepted_Name_with_Author) %>%
    distinct()
  #Add the matches to the Matched_results_nonspecies
  if (nrow(matches) > 0) {
    matched_rows <- data.frame(verbatimScientificName = name,
                               Additional_Name = matches$Additional_Name,
                               Accepted_Name_with_Author = matches$Accepted_Name_with_Author,
                               stringsAsFactors = FALSE)
    Matched_results_nonspecies <- rbind(Matched_results_nonspecies, matched_rows)
  }
}

Matched_results<-rbind(Matched_results_species, Matched_results_nonspecies)

#Now we can remove the additional name, and drop cases where there is more than one 
#verbatimScientificName match to Accepted_Name_with_Author, meaning we do not confidently know 
#to which one it belongs.
Matched_results<-Matched_results%>%
  select(-c(Additional_Name)) %>%
  group_by(verbatimScientificName) %>%
  filter(n() == 1) %>%
  ungroup()

#The resulting Matched_results are the only names which can be confidently appended back into the larger GBIF data, for
#they have a single match to an Accepted Name or one of its synonyms, but not to other Accepted Names or their synonyms. 

Combi_List_TaxonIssue_NO_reAdd2<- Combi_List_TaxonIssue_NO %>%
  filter(verbatimScientificName %in% Matched_results$verbatimScientificName)

#Final Combi_List_TaxonIssue_NO of valid names but without authority, and which cannot be confidently matched to only one accepted name in the given flora at hand 
Combi_List_TaxonIssue_NO<- Combi_List_TaxonIssue_NO %>%
  filter(!verbatimScientificName %in% Matched_results$verbatimScientificName)

Combi_List_TaxonIssue_NO_reAdd2 <- Combi_List_TaxonIssue_NO_reAdd2 %>%
  left_join(Matched_results, by = "verbatimScientificName")

#Now repair taxonRank
Combi_List_TaxonIssue_NO_reAdd2$taxonRank<-NA
Combi_List_TaxonIssue_NO_reAdd2$scientificName<-Combi_List_TaxonIssue_NO_reAdd2$verbatimScientificName

Combi_List_TaxonIssue_NO_reAdd2 <- Combi_List_TaxonIssue_NO_reAdd2 %>%
  mutate(taxonRank = case_when(
    str_detect(Accepted_Name_with_Author, "× ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, "^X ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, " [×xX] ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, " subsp\\. ") ~ "Subspecies",
    str_detect(Accepted_Name_with_Author, " var\\. ") ~ "Variety",
    str_detect(Accepted_Name_with_Author, " subvar\\. ") ~ "Subvariety",
    str_detect(Accepted_Name_with_Author, " morph") ~ "Morph",
    str_detect(Accepted_Name_with_Author, " f\\. ") ~ "Form",
    str_detect(Accepted_Name_with_Author, " subf\\. ") ~ "Subform",
    str_detect(Accepted_Name_with_Author, " agg\\. ") ~ "Aggregate",
    str_detect(Accepted_Name_with_Author, " aggregate") ~ "Aggregate",
    str_detect(Accepted_Name_with_Author, " sect\\. ") ~ "Section",
    str_detect(Accepted_Name_with_Author, " subsect\\. ") ~ "Subsection",
    str_detect(Accepted_Name_with_Author, " ser\\. ") ~ "Series",
    str_detect(Accepted_Name_with_Author, " subser\\. ") ~ "Subseries",
    str_detect(Accepted_Name_with_Author, " tr\\. ") ~ "Tribe",
    str_detect(Accepted_Name_with_Author, " subtrib\\. ") ~ "Subtribe",
    str_detect(Accepted_Name_with_Author, " gen\\. ") ~ "Genus",
    str_detect(Accepted_Name_with_Author, " subg\\. ") ~ "Subgenus",
    str_detect(Accepted_Name_with_Author, " fam\\. ") ~ "Family",
    str_detect(Accepted_Name_with_Author, " subfam\\. ") ~ "Subfamily",
    str_detect(Accepted_Name_with_Author, " ord\\. ") ~ "Order",
    str_detect(Accepted_Name_with_Author, " subord\\. ") ~ "Suborder",
    str_detect(Accepted_Name_with_Author, " cl\\. ") ~ "Class",
    str_detect(Accepted_Name_with_Author, " subcl\\. ") ~ "Subclass",
    TRUE ~ NA_character_  #Keep any remaining entries as NA in taxonRank
  )) %>%
  #Assign taxonRank for cases where it is still NA
  mutate(taxonRank = case_when(
    is.na(taxonRank) & str_count(Accepted_Name_with_Author, "\\s") >= 1 & 
      str_detect(Accepted_Name_with_Author, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species", #Detect cases when first word is capitalized and second word is not, label as species, also accounting for accented names 
    is.na(taxonRank) & str_count(Accepted_Name_with_Author, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ taxonRank
  ))

#Document Results
Documentation_Results$Twenty<-nrow(Combi_List_TaxonIssue_NO_reAdd2)
#Document Results
Documentation_Results$TwentyOne<-nrow(Combi_List_TaxonIssue_NO)

#Now bind back into GBIF data
Combi_List_Storage_Accepted<-rbind(Combi_List_Storage_Accepted, Combi_List_TaxonIssue_NO_reAdd2)

#Now get names and GBIF catalogue numbers of names without authority which are now leftover:
Combi_List_TaxonIssue_NO_Catalogs<- Combi_List_TaxonIssue_NO %>%
  select(verbatimScientificName, catalogNumber)

#Now pivot the names back to long format
Combi_List_TaxonIssue_NO_Catalogs <- Combi_List_TaxonIssue_NO_Catalogs %>%
  group_by(verbatimScientificName) %>%
  mutate(name_id = row_number()) %>%  #Unique ID for each catalogNumber within the same verbatimScientificName
  pivot_wider(
    names_from = name_id,
    values_from = catalogNumber,
    names_prefix = "Catalogue_Number_"
  ) %>%
  ungroup()  #Remove the grouping

#Repair any ranks now that data is finalized for Combi_List_Storage_Accepted:
#Find the cases where the rank is capital letters (this means it is from GBIF),
#turn it to NA, and then repair using the Accepted Name now. 

Combi_List_Storage_Accepted <- Combi_List_Storage_Accepted %>%
  mutate(taxonRank = case_when(
    str_detect(taxonRank, "^[A-Z]+$") ~ NA_character_,  #Replace all-uppercase taxonRanks with NA
    TRUE ~ taxonRank
  )) %>%
  mutate(taxonRank = case_when(
    str_detect(Accepted_Name_with_Author, "× ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, "^X ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, " [×xX] ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, " subsp\\. ") ~ "Subspecies",
    str_detect(Accepted_Name_with_Author, " var\\. ") ~ "Variety",
    str_detect(Accepted_Name_with_Author, " subvar\\. ") ~ "Subvariety",
    str_detect(Accepted_Name_with_Author, " morph") ~ "Morph",
    str_detect(Accepted_Name_with_Author, " f\\. ") ~ "Form",
    str_detect(Accepted_Name_with_Author, " subf\\. ") ~ "Subform",
    str_detect(Accepted_Name_with_Author, " agg\\. ") ~ "Aggregate",
    str_detect(Accepted_Name_with_Author, " aggregate") ~ "Aggregate",
    str_detect(Accepted_Name_with_Author, " sect\\. ") ~ "Section",
    str_detect(Accepted_Name_with_Author, " subsect\\. ") ~ "Subsection",
    str_detect(Accepted_Name_with_Author, " ser\\. ") ~ "Series",
    str_detect(Accepted_Name_with_Author, " subser\\. ") ~ "Subseries",
    str_detect(Accepted_Name_with_Author, " tr\\. ") ~ "Tribe",
    str_detect(Accepted_Name_with_Author, " subtrib\\. ") ~ "Subtribe",
    str_detect(Accepted_Name_with_Author, " gen\\. ") ~ "Genus",
    str_detect(Accepted_Name_with_Author, " subg\\. ") ~ "Subgenus",
    str_detect(Accepted_Name_with_Author, " fam\\. ") ~ "Family",
    str_detect(Accepted_Name_with_Author, " subfam\\. ") ~ "Subfamily",
    str_detect(Accepted_Name_with_Author, " ord\\. ") ~ "Order",
    str_detect(Accepted_Name_with_Author, " subord\\. ") ~ "Suborder",
    str_detect(Accepted_Name_with_Author, " cl\\. ") ~ "Class",
    str_detect(Accepted_Name_with_Author, " subcl\\. ") ~ "Subclass",
    TRUE ~ taxonRank  #Keep remaining ranks unchanged
  )) %>%
  #Assign taxonRank for cases where it is still NA
  mutate(taxonRank = case_when(
    is.na(taxonRank) & str_count(Accepted_Name_with_Author, "\\s") >= 1 & 
      str_detect(Accepted_Name_with_Author, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species",  
    is.na(taxonRank) & str_count(Accepted_Name_with_Author, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ taxonRank  
  ))

#Write Excel file with the Greenland specimens from Herbarium C and their corresponding Accepted Name
#Write to Excel File:
#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("GBIF_Records_With_Accepted_Names_", today_date, ".xlsx")
#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)
#Write to Excel file
write_xlsx(Combi_List_Storage_Accepted, file_name)
#Set the working directory back to the original directory
setwd("..")

#Now the leftovers 
#Concatenate the date with the rest of the file name
file_name <- paste0("GBIF_Records_With_Unrecognized_Names_", today_date, ".xlsx")
#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)
#Write to Excel file
write_xlsx(Combi_List_Storage_Leftover, file_name)
#Set the working directory back to the original directory
setwd("..")

#Now the Names_without_Authors
file_name <- paste0("GBIF_Records_Names_Without_Authors_", today_date, ".xlsx")
setwd(folder_name)
write_xlsx(Combi_List_TaxonIssue_NO_Catalogs, file_name)
setwd("..")

#Document Results
Documentation_Results$TwentyTwo<-nrow(Combi_List_TaxonIssue_NO_Catalogs)

#Document Results
Documentation_Results$TwentyThree<-nrow(Combi_List_Storage_Accepted)

#Document Results
Documentation_Results$TwentyFour<-nrow(Combi_List_Storage_Leftover)

Unauthored_Names_Publish <- Combi_List_TaxonIssue_NO_Catalogs %>%
  select(verbatimScientificName) %>%
  distinct() %>%
  filter(!is.na(verbatimScientificName) & verbatimScientificName != "") %>%
  rename(original_input_name = verbatimScientificName)

Unauthored_Names_Publish$Verified_Name<-Unauthored_Names_Publish$original_input_name

Unauthored_Names_Publish$Status<-"No_Author_Multiple_Author_Options"

#Final editing of Rank Data:
Unauthored_Names_Publish <- Unauthored_Names_Publish %>%
  mutate(rank = NA_character_) %>% 
  mutate(rank = case_when(
    str_detect(rank, "^[A-Z]+$") ~ NA_character_,  #Replace all-uppercase taxonRanks with NA
    TRUE ~ rank
  )) %>%
  mutate(rank = case_when(
    str_detect(Verified_Name, "× ") ~ "Hybrid",
    str_detect(Verified_Name, "^X ") ~ "Hybrid",
    str_detect(Verified_Name, " [×xX] ") ~ "Hybrid",
    str_detect(Verified_Name, " subsp\\. ") ~ "Subspecies",
    str_detect(Verified_Name, " var\\. ") ~ "Variety",
    str_detect(Verified_Name, " subvar\\. ") ~ "Subvariety",
    str_detect(Verified_Name, " morph") ~ "Morph",
    str_detect(Verified_Name, " f\\. ") ~ "Form",
    str_detect(Verified_Name, " subf\\. ") ~ "Subform",
    str_detect(Verified_Name, " agg\\. ") ~ "Aggregate",
    str_detect(Verified_Name, " aggregate") ~ "Aggregate",
    str_detect(Verified_Name, " sect\\. ") ~ "Section",
    str_detect(Verified_Name, " subsect\\. ") ~ "Subsection",
    str_detect(Verified_Name, " ser\\. ") ~ "Series",
    str_detect(Verified_Name, " subser\\. ") ~ "Subseries",
    str_detect(Verified_Name, " tr\\. ") ~ "Tribe",
    str_detect(Verified_Name, " subtrib\\. ") ~ "Subtribe",
    str_detect(Verified_Name, " gen\\. ") ~ "Genus",
    str_detect(Verified_Name, " subg\\. ") ~ "Subgenus",
    str_detect(Verified_Name, " fam\\. ") ~ "Family",
    str_detect(Verified_Name, "^[A-Z][a-zà-öø-ÿ]+eae\\b") ~ "Family", #If the first word ends with eae
    str_detect(Verified_Name, " subfam\\. ") ~ "Subfamily",
    str_detect(Verified_Name, " ord\\. ") ~ "Order",
    str_detect(Verified_Name, " subord\\. ") ~ "Suborder",
    str_detect(Verified_Name, " cl\\. ") ~ "Class",
    str_detect(Verified_Name, " subcl\\. ") ~ "Subclass",
    TRUE ~ rank  #Keep remaining ranks unchanged
  )) %>%
  #Assign rank for cases where it is still NA
  mutate(rank = case_when(
    is.na(rank) & str_count(Verified_Name, "\\s") >= 1 & 
      str_detect(Verified_Name, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species",  
    is.na(rank) & str_count(Verified_Name, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ rank  
  ))
save.image(file = "Taxa_List_Save_22.RData") #Save everything 
#load("Taxa_List_Save_22.RData")
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Internal Validation: Step 3 - Making a Simple Data Output 
{
Combi_List_Storage_Accepted_Simple<- Combi_List_Storage_Accepted %>%
  select(Accepted_Name_with_Author,taxonRank, catalogNumber, verbatimScientificName, scientificName, gbifID,datasetKey, occurrenceID, publishingOrgKey, taxonKey, speciesKey)

#Write to Excel File:
#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("GBIF_Records_With_Accepted_Names_Simple_", today_date, ".xlsx")
#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)
#Write to Excel file
write_xlsx(Combi_List_Storage_Accepted_Simple, file_name)
#Set the working directory back to the original directory
setwd("..")

save.image(file = "Taxa_List_Save_22.5.RData") #Save everything 
#load("Taxa_List_Save_22.5.RData")
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Internal Validation: Step 4 - Summary Statistics of Accepted Names and Leftover Names over time 
{
  
#Check how many specimens we have of each species
Specimen_Counts<- Combi_List_Storage_Accepted %>% 
  count(Accepted_Name_with_Author)            #Count number of specimens for each scientific name 
colnames(Specimen_Counts)[2] <- "Number_of_Specimens"

#Check how specimens are distributed through time
Time<- Combi_List_Storage_Accepted %>%          #N.B. This will likely be less than the number of scientificNames (and therefore the Specimen_Counts) as for some scientificNames there are no years recorded (eg for Achillea millefolium f. lanulosa)
  group_by(Accepted_Name_with_Author) %>%
  slice(c(which.min(year), which.max(year))) %>%
  mutate(Oldest_Specimen_with_Date = min(year),
         Youngest_Specimen_with_Date = max(year)) %>%
  select(Accepted_Name_with_Author,Oldest_Specimen_with_Date,Youngest_Specimen_with_Date) %>%
  distinct()
{
#Check for samples younger than a certain age: 
Sixty_or_younger<- Combi_List_Storage_Accepted %>% 
  group_by(Accepted_Name_with_Author) %>%
  tally(year>1963) %>%
  transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
            Number_of_Sixty_Yrs_or_Younger = n)

Fifty_or_younger<- Combi_List_Storage_Accepted %>% 
  group_by(Accepted_Name_with_Author) %>%
  tally(year>1973) %>%
  transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
            Number_of_Fifty_Yrs_or_Younger = n)

Forty_or_younger<- Combi_List_Storage_Accepted %>% 
  group_by(Accepted_Name_with_Author) %>%
  tally(year>1983) %>%
  transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
            Number_of_Forty_Yrs_or_Younger = n)

Thirty_or_younger<- Combi_List_Storage_Accepted %>% 
  group_by(Accepted_Name_with_Author) %>%
  tally(year>1993) %>%
  transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
            Number_of_Thirty_Yrs_or_Younger = n)

Twenty_or_younger<- Combi_List_Storage_Accepted %>% 
  group_by(Accepted_Name_with_Author) %>%
  tally(year>2003) %>%
  transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
            Number_of_Twenty_Yrs_or_Younger = n)

Ten_or_younger<- Combi_List_Storage_Accepted %>% 
  group_by(Accepted_Name_with_Author) %>%
  tally(year>2013) %>%
  transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
            Number_of_Ten_Yrs_or_Younger = n)

Number_of_Samples_With_Unknown_Year<- Combi_List_Storage_Accepted %>% 
  group_by(Accepted_Name_with_Author) %>%
  tally(is.na(year)) %>%
  transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
            Number_of_Samples_With_Unknown_Year = n)

Specimen_Counts<-left_join(Specimen_Counts,Time, by="Accepted_Name_with_Author")
Specimen_Counts<-left_join(Specimen_Counts,Sixty_or_younger, by="Accepted_Name_with_Author")
Specimen_Counts<-left_join(Specimen_Counts,Fifty_or_younger, by="Accepted_Name_with_Author")
Specimen_Counts<-left_join(Specimen_Counts,Forty_or_younger, by="Accepted_Name_with_Author")
Specimen_Counts<-left_join(Specimen_Counts,Thirty_or_younger, by="Accepted_Name_with_Author")
Specimen_Counts<-left_join(Specimen_Counts,Twenty_or_younger, by="Accepted_Name_with_Author")
Specimen_Counts<-left_join(Specimen_Counts,Ten_or_younger, by="Accepted_Name_with_Author")
Specimen_Counts<-left_join(Specimen_Counts,Number_of_Samples_With_Unknown_Year, by="Accepted_Name_with_Author")
  }

Specimen_Counts_1<-Specimen_Counts %>%
  filter(!is.na(Oldest_Specimen_with_Date) & !is.na(Youngest_Specimen_with_Date))

#Change values from 0 to NA when there are no transcribed dates whatsoever anyways. 
Specimen_Counts_2<-Specimen_Counts %>%
  filter(is.na(Oldest_Specimen_with_Date) & is.na(Youngest_Specimen_with_Date))
Specimen_Counts_2$Number_of_Sixty_Yrs_or_Younger<-NA
Specimen_Counts_2$Number_of_Fifty_Yrs_or_Younger<-NA
Specimen_Counts_2$Number_of_Forty_Yrs_or_Younger<-NA
Specimen_Counts_2$Number_of_Thirty_Yrs_or_Younger<-NA
Specimen_Counts_2$Number_of_Twenty_Yrs_or_Younger<-NA
Specimen_Counts_2$Number_of_Ten_Yrs_or_Younger<-NA

Specimen_Counts_Final<-rbind(Specimen_Counts_1,Specimen_Counts_2)
#Now for the Leftovers:
#First, add in the name found for the specimens after manual taxonomic checking:
#We do this because sometimes the scientificName for the leftovers was actually incorrect,
#and therefore if there is a corrected version, it should be used. 
#There could also be cases were two scientificNames are corrected to the same
#name, but that name itself is still a leftover. 

#This involves using Final_Leftovers, and Combi_List_Storage_Leftover
#Recall that Final_Leftovers$name is the manually edited name. 

Combi_List_Storage_Leftover<-Combi_List_Storage_Leftover%>%
  rename(Verified_Unrecognized_Name = Accepted_Name_with_Author) 
}
{
#Join into the Final_Leftovers, but making sure that the Verified_Unrecognized_Name is the one manually checked by the
#Expert Taxonomist (rather than the original input name)

#Ensure all Final Leftover Original Input Names are present (as this differs from the Final_Leftovers dataframe, which only contains
#the final names, and not additional nams which may match to it.)
Final_Leftovers1<-Final_Leftovers1%>%
  select(-c(Manually_Edited_Name))

Final_Leftovers1 <- Final_Leftovers1 %>%
  pivot_longer(cols = -name,  
               names_to = "filler",  
               values_to = "original_input_name") %>%
  drop_na(original_input_name) %>%
  select(-c(filler))

Final_Leftovers<-rbind(Final_Leftovers,Final_Leftovers1)
Final_Leftovers<-Final_Leftovers%>%distinct()

Final_Leftovers<-Final_Leftovers %>%
  rename(Verified_Unrecognized_Name = name)

Final_Leftovers_NameAdd<-Final_Leftovers %>%
  select(Verified_Unrecognized_Name) %>%
  mutate(original_input_name = Verified_Unrecognized_Name)

Final_Leftovers <- rbind(Final_Leftovers,Final_Leftovers_NameAdd) %>% distinct()

#Remove Final Leftover Names which do not belong 
Final_Leftovers<-Final_Leftovers%>%
  filter(!original_input_name %in% Confirmed_Master_List_Tidy_Names_Storage$Additional_Name)

All_Names<-Combi_List_Storage %>% select(scientificName) %>% distinct()
Final_Leftovers<-Final_Leftovers%>%
  filter(normalize_names(original_input_name) %in% normalize_names(All_Names$scientificName)) #593 Final leftovers before normalizing, want one more 

Combi_List_Storage_Leftover <- Combi_List_Storage_Leftover %>%
  mutate(Normalized_ScientificName = normalize_names(scientificName)) %>%
  left_join(
    Final_Leftovers %>%
      mutate(Normalized_OriginalInputName = normalize_names(original_input_name)) %>%
      group_by(Verified_Unrecognized_Name, Normalized_OriginalInputName) %>%
      filter(row_number() == 1) %>%  # Keep only one row per Verified_Unrecognized_Name and normalized name
      ungroup(),
    by = c("Normalized_ScientificName" = "Normalized_OriginalInputName")
  ) %>%
  select(-Normalized_ScientificName)

#Now every Combi_List_Storage_Leftover specimen should have an original input name associated to it. 
Combi_List_Storage_Leftover<-Combi_List_Storage_Leftover%>%
  select(-starts_with("Verified_Unrecognized_")) %>%
  left_join(Final_Leftovers, by = "original_input_name") 

Combi_List_Storage_Leftover<-Combi_List_Storage_Leftover%>%
    select(Verified_Unrecognized_Name, original_input_name, everything())
}

#There can also be cases where a name was a leftover, was matched into the Reference List, but then that 
#Reference list name had multiple POWO matches and was removed. An example is Leontodon autumnalis var. taraxaci (L.) Hartm.
#Which was a leftover name matched to Scorzoneroides autumnalis (L.) Moench, but Scorzoneroides autumnalis (L.) Moench
#matched to multiple POWO names and was removed. Therefore, now there is no Leftover Name record of Leontodon autumnalis var. taraxaci (L.) Hartm.
{
Leftovers_Reference_Back_to_Leftovers <- Combi_List_Storage_Leftover %>%
  filter(is.na(Verified_Unrecognized_Name)) %>%  
  select(scientificName) %>% 
  distinct() 

#Make sure not in Unplaced_Removed_Publish
Leftovers_Reference_Back_to_Leftovers<-Leftovers_Reference_Back_to_Leftovers%>%
  filter(!scientificName %in% Unplaced_Removed_Publish$Verified_Name)
  
Combi_List_Storage_Leftover <- Combi_List_Storage_Leftover %>%
  mutate(Verified_Unrecognized_Name = ifelse(
    is.na(Verified_Unrecognized_Name), 
    #If Verified_Unrecognized_Name is NA, check if scientificName matches any original_input_name from the Reference List
    ifelse(
      scientificName %in% Reference_List_CompLeftovers1$original_input_name, 
      scientificName,  #Assign scientificName to Verified_Unrecognized_Name
      Verified_Unrecognized_Name),  #Leave it as is if no match is found
    Verified_Unrecognized_Name))

#If this is the case, add back in to Final Leftovers
Final_Leftovers <- Final_Leftovers %>%
  bind_rows(Leftovers_Reference_Back_to_Leftovers %>%
      filter(scientificName %in% Reference_List_CompLeftovers1$original_input_name) %>%
      mutate(Verified_Unrecognized_Name = scientificName, 
             original_input_name = scientificName) %>%
      select(Verified_Unrecognized_Name, original_input_name))

#Remove Cases Belonging to Unplaced_Removed_Publish
Combi_List_Unplaced_Removed_Publish<-Combi_List_Storage_Leftover%>%
  filter(scientificName %in% Unplaced_Removed_Publish$Verified_Name)

Combi_List_Storage_Leftover<-Combi_List_Storage_Leftover%>%
  filter(!scientificName %in% Unplaced_Removed_Publish$Verified_Name)

#Remove Cases Belonging to Taxonomic_Autonyms_Publish
Combi_List_Taxonomic_Autonyms_Publish<-Combi_List_Storage_Leftover%>%
  filter(scientificName %in% Taxonomic_Autonyms_Publish$Verified_Name)

Combi_List_Storage_Leftover<-Combi_List_Storage_Leftover%>%
  filter(!scientificName %in% Taxonomic_Autonyms_Publish$Verified_Name)
}

if (all(!is.na(Combi_List_Storage_Leftover$Verified_Unrecognized_Name))) {
  print("All original input names from GBIF are confirmed to have been processed and matched! Woohoo!")
} else {
  print("There are names which have not been successfully processed and matched. Something has gone wrong, please check the NA values in Verified_Unrecognized_Name to see what happened.")
}

{
Specimen_Counts_Leftovers<- Combi_List_Storage_Leftover %>% 
  count(Verified_Unrecognized_Name)            #Count number of specimens for each scientific name 
colnames(Specimen_Counts_Leftovers)[2] <- "Number_of_Specimens"


Time_Leftovers<- Combi_List_Storage_Leftover %>%          #N.B. This will likely be less than the number of scientificNames (and therefore the Specimen_Counts) as for some scientificNames there are no years recorded (eg for Achillea millefolium f. lanulosa)
  group_by(Verified_Unrecognized_Name) %>%
  slice(c(which.min(year), which.max(year))) %>%
  mutate(Oldest_Specimen_with_Date = min(year),
         Youngest_Specimen_with_Date = max(year)) %>%
  select(Verified_Unrecognized_Name,Oldest_Specimen_with_Date,Youngest_Specimen_with_Date, Verified_Unrecognized_Name) %>%
  distinct()

Specimen_Leftovers_Counts<-left_join(Specimen_Counts_Leftovers,Time_Leftovers, by="Verified_Unrecognized_Name")
colnames(Specimen_Leftovers_Counts)[1] <- "Unaccepted_Name"

#Using Combi_List_Storage_Accepted to get information about the data source for each Accepted Name.
#The datasetKey indicates the data source. 

#Count how many institutions contribute to a given accepted name
Specimen_Counts_Institutions <- Combi_List_Storage_Accepted %>%
  distinct(Accepted_Name_with_Author, datasetKey) %>%  
  count(Accepted_Name_with_Author, name = "Unique_Institution_Count")  #Count distinct data sources

#Add to Specimen_Counts_Final and to Specimen_Counts_Leftovers
Specimen_Counts_Final<-left_join(Specimen_Counts_Final, Specimen_Counts_Institutions, by="Accepted_Name_with_Author")

#Count how many institutions contribute to a given leftover name, not counting Unknown_Institution_Code
Specimen_Counts_Institutions_Leftover <- Combi_List_Storage_Leftover %>%
  distinct(Verified_Unrecognized_Name, datasetKey) %>% 
  count(Verified_Unrecognized_Name, name = "Unique_Institution_Count")

Specimen_Counts_Leftovers<-left_join(Specimen_Counts_Leftovers, Specimen_Counts_Institutions_Leftover, by = c("Verified_Unrecognized_Name"))

#Write to Excel File:
#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("GBIF_Recognized_Taxa_Specimen_Counts_", today_date, ".xlsx")
#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)
#Write to Excel file
write_xlsx(Specimen_Counts_Final, file_name)
#Set the working directory back to the original directory
setwd("..")

#Write to Excel File:
#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("GBIF_Unrecognized_Taxa_Specimen_Counts_", today_date, ".xlsx")
#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)
#Write to Excel file
write_xlsx(Specimen_Counts_Leftovers, file_name)
#Set the working directory back to the original directory
setwd("..")

save.image(file = "Taxa_List_Save_23.RData") #Save everything 
#load("Taxa_List_Save_23.RData")
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Internal Validation: Step 5 - Flagging Names which are likely not belonging to the given flora 

#To run an internal validation, we can use the number of specimens for a given species
#as an indication of whether we think it is a reliable species name. In other words, if there
#are more records of that species in the data, it increases our confidence that this species
#belongs. When there is a small amount of species however, we want to further investigate it, as many
#species names can be added when the specimen's data was not properly transcribed during mass-digitization, or
#when the species was incorrectly identified. Moreover, the number of sources for that given name can also be factored in. 

#Autonyms - autonyms are auto-generated due to Article 26 of The International Code of Nomenclature for algae, fungi, and plants. Some may want to not run checks on them - we advise to run checks based on our Greenland case study, as many were erraneous specimen labels.

#Manually determine if you want these considered for flagging:
Autonym_Decision <- tolower(readline(prompt = "Autonyms are removed from flagged specimens by default. Press Enter to keep default, or type NO to remove them instead: "))

#Conditionally remove the autonyms from the specimen count list
if (Autonym_Decision != "no") {
  #Remove the autonyms from the specimen count list if the user did not type "NO"
  Specimen_Counts_Final_NoAutonyms <- Specimen_Counts_Final %>%
    filter(!sapply(Accepted_Name_with_Author, search_autonym))
} else {
  #If the user chose to keep autonyms, keep the original dataframe 
  Specimen_Counts_Final_NoAutonyms <- Specimen_Counts_Final
}

#Set a value for the species counts threshold - this can be dictated by the user depending on how the data is looking. 
Count_Value <- as.numeric(readline(prompt = "What is the minimum number of GBIF records needed in this flora for an Accepted Name to be considered reliable (default is 6): "))

#Convert input to numeric
Count_Value <- as.numeric(Count_Value)
#If the input is NA (meaning it's not a number), set the default value to 5
if (is.na(Count_Value)) {
  Count_Value <- 5
}

#For the Accepted Names
Count_Number_Accepted<- sum(Specimen_Counts_Final_NoAutonyms$Number_of_Specimens <= Count_Value)
cat("Number of Accepted Taxa Names which have", Count_Value, "or less specimens:", Count_Number_Accepted, "\n")

#For the Leftover Names
Count_Number_Leftover<- sum(Specimen_Counts_Leftovers$Number_of_Specimens <= Count_Value)
cat("Number of Leftover Taxa Names which have", Count_Value, "or less specimens:", Count_Number_Leftover, "\n")

#Set a value, if wanted, for the data source threshold - meaning how many sources have to have provided this name for it to
#be considered reliable, even if there are not many specimens with that name 
#The default value is set to 3 institutional providers 

#Set a value for the institutional data source count threshold - this can be dictated by the user depending on how the data is looking. 
Institution_Value <- readline(prompt = "What is the minimum number of data sources needed in this flora for an Accepted Name to be considered reliable (default is 0 - press Enter to keep this): ")
#Ignore cases where Specimen_Counts_Final_NoAutonyms$Unique_Institution_Count is NA, as these are due to the Reference Flora being added into the data. 

#Convert input to numeric
Institution_Value <- as.numeric(Institution_Value)
if (is.na(Institution_Value)) {
  Institution_Value <- 0
}

Institution_Number_Accepted <- sum(Specimen_Counts_Final_NoAutonyms$Unique_Institution_Count <= Institution_Value)
cat("Number of Accepted Taxa Names which have", Institution_Value, "or less data sources:", Institution_Number_Accepted, "\n")

Institution_Number_Leftover <- sum(Specimen_Counts_Leftovers$Unique_Institution_Count <= Institution_Value)
cat("Number of Unrecognized Taxa Names which have", Institution_Value, "or less data sources:", Institution_Number_Leftover, "\n")

save.image(file = "Taxa_List_Save_23.5.RData") #Save everything 
#load("Taxa_List_Save_23.5.RData")


#Set a value for the collection timespan of samples - this can be dictated by the user depending on how the data is looking. 
#Here, the user can elect to flag samples if their most recent collection time has not been recent, where they define how recent: 
#Only apply to Accepted Names:
#If using this flagging option, then names without any collection data data are automatically flagged. 

Most_Recent_Collection_Time_Value <- as.numeric(readline(prompt = "What is the maximum number of years ago for which a the collection of an observation with an Accepted Name can be considered reliable (default is 60 years,
                                                         meaning that if it has been more than 60 years since it was last collected, it is flagged): "))

#Convert input to numeric
Most_Recent_Collection_Time_Value <- as.numeric(Most_Recent_Collection_Time_Value)
#If the input is NA (meaning it's not a number), set the default value to 60
if (is.na(Most_Recent_Collection_Time_Value)) {
  Most_Recent_Collection_Time_Value <- 60
}

Current_year <- as.numeric(format(Sys.Date(), "%Y"))
Most_Recent_Collection_Time_Value <- as.numeric(Most_Recent_Collection_Time_Value)

Most_Recent_Collection_Time_Value <- Current_year - Most_Recent_Collection_Time_Value #The year where if no sampling occurred during this year or since, the name is flagged. 

#Flagging Cases:
#Cases will be flagged according to their specimen count and their institution count, and also this combined. 
#This way, investigating can be partitioned as needed. 

#For the Accepted Taxa Names in Specimen_Counts_Final_NoAutonyms:
{
#Create the new column 'Flagged_Specimen_Count'
Specimen_Counts_Final_NoAutonyms$Number_of_Specimens <- as.numeric(Specimen_Counts_Final_NoAutonyms$Number_of_Specimens)
Specimen_Counts_Leftovers$Number_of_Specimens<-as.numeric(Specimen_Counts_Leftovers$Number_of_Specimens)

#For the Accepted Names (The Taxa List)
Specimen_Counts_Final_NoAutonyms <- Specimen_Counts_Final_NoAutonyms %>%
  mutate(Flagged_DataSource_Count = Unique_Institution_Count <= Institution_Value)

#Most_Recent_Collection_Time_Value for the Accepted Names
Specimen_Counts_Final_NoAutonyms <- Specimen_Counts_Final_NoAutonyms %>%
  mutate(Flagged_Most_Recent_Collection_Time = 
           if_else(is.na(Youngest_Specimen_with_Date) | Youngest_Specimen_with_Date < Most_Recent_Collection_Time_Value, 
                   TRUE, FALSE))

Specimen_Counts_Final_NoAutonyms <- Specimen_Counts_Final_NoAutonyms %>%
  mutate(Flagged_Specimen_Count = Number_of_Specimens <= Count_Value) %>%
  mutate(Flagged_Name = Flagged_Specimen_Count | Flagged_DataSource_Count | Flagged_Most_Recent_Collection_Time) %>% #If ANY are TRUE, overall FLAG the name
  select(Accepted_Name_with_Author, Flagged_Name, Flagged_Specimen_Count, Flagged_Most_Recent_Collection_Time, Flagged_DataSource_Count)

#For the Leftover Names (The Unrecognized List)
Specimen_Counts_Leftovers <- Specimen_Counts_Leftovers %>%
  mutate(Flagged_DataSource_Count = Unique_Institution_Count <= Institution_Value)

Specimen_Counts_Leftovers <- Specimen_Counts_Leftovers %>%
  mutate(Flagged_Specimen_Count = Number_of_Specimens <= Count_Value) %>%
  mutate(Flagged_Name = Flagged_Specimen_Count | Flagged_DataSource_Count) %>%
  select(Verified_Unrecognized_Name, Flagged_Name, Flagged_Specimen_Count, Flagged_DataSource_Count)

#Now Add in the GBIF catalogue numbers per Flagged Name for easy searching 
#Go into Combi_List_Storage_Accepted_Simple and find all GBIF IDs of Flagged Taxa Names 
Specimen_Counts_Final_NoAutonyms<-Specimen_Counts_Final_NoAutonyms %>%
  left_join(Combi_List_Storage_Accepted_Simple %>%
              select(Accepted_Name_with_Author, gbifID),
            by = "Accepted_Name_with_Author") %>%
  group_by(Accepted_Name_with_Author) %>%
  summarize(gbifID = toString(gbifID)) %>%
  ungroup() %>%
  left_join(Specimen_Counts_Final_NoAutonyms, by = "Accepted_Name_with_Author")

Specimen_Counts_Leftovers<-Specimen_Counts_Leftovers %>%
  left_join(Combi_List_Storage_Leftover %>%
              select(Verified_Unrecognized_Name, gbifID),
            by = "Verified_Unrecognized_Name") %>%
  group_by(Verified_Unrecognized_Name) %>%
  summarize(gbifID = toString(gbifID)) %>%
  ungroup() %>%
  left_join(Specimen_Counts_Leftovers, by = "Verified_Unrecognized_Name")

#Also specifically flag cases where the only data source is of human observation,
#as we give more trust to occurrences stemming from digitized records. 
#To do this, we filter for Combi_List_Storage$basisOfRecord
human_observation_names <- Combi_List_Storage %>%
  group_by(Accepted_Name_with_Author) %>%
  filter(all(basisOfRecord == "HUMAN_OBSERVATION")) %>%
  distinct(Accepted_Name_with_Author) %>%
  pull(Accepted_Name_with_Author)

Specimen_Counts_Final_NoAutonyms <- Specimen_Counts_Final_NoAutonyms %>%
  mutate(Flagged_DataSource_Count = ifelse(Accepted_Name_with_Author %in% human_observation_names, TRUE, Flagged_DataSource_Count))

Specimen_Counts_Final_NoAutonyms <- Specimen_Counts_Final_NoAutonyms %>%
  mutate(Flagged_Name = ifelse(Flagged_DataSource_Count == TRUE, TRUE, Flagged_Name))

#Now we have Specimen_Counts_Final_NoAutonyms, and Specimen_Counts_Leftovers,
#Which can be saved on their own, and also merged into the data. 

#Write to Excel File:
#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("Initial_GBIF_Recognized_Taxa_Counts_", today_date, ".xlsx")
#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)
#Write to Excel file
write_xlsx(Specimen_Counts_Final_NoAutonyms, file_name)
#Set the working directory back to the original directory
setwd("..")

#Write to Excel File:
#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("Initial_GBIF_Unrecognized_Taxa_Counts_Flagged_Names_", today_date, ".xlsx")
#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)
#Write to Excel file
write_xlsx(Specimen_Counts_Leftovers, file_name)
#Set the working directory back to the original directory
setwd("..")

save.image(file = "Taxa_List_Save_24.RData") #Save everything 
#load("Taxa_List_Save_24.RData")
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Additional Curation Step - Checking Flagged Names, and Unflagging, Removing, or Keeping Flagged

#This is a step requiring a taxonomic expert, who can assess the Flagged Names and determine if 
#1. They should be removed, as in they are an error, like Arthropteris tenella (G.Forst.) J.Sm.
#2. If they should be unflagged, as we are confident it belongs, like Lupinus nootkatensis Donn ex Sims
#3. If we are unsure, and so it should be kept as flagged. 

Confirmed_Master_List_Tidy_Original_Count<-nrow(Confirmed_Master_List_Tidy)
saveRDS(Confirmed_Master_List_Tidy_Original_Count, file = "Confirmed_Master_List_Tidy_Original_Count.rds")

#First automate what we can 
#Count the number of rows where Flagged_Name is TRUE
num_flagged_true <- sum(Specimen_Counts_Final_NoAutonyms$Flagged_Name == TRUE)
#Print the result
cat("Number of rows where Flagged_Name is TRUE:", num_flagged_true, "\n") #554 Flagged Names to start, FEB 23, 2025 - 738

Number_Original_Flagged<-num_flagged_true

#First check if the Data Source is GBIF AND Reference Flora, or just the Reference Flora, as then we are also confident in the name
GBIF_and_Reference_Flora_Flag <- Specimen_Counts_Final_NoAutonyms %>%
  filter(Flagged_Name == TRUE) %>%
  select(Accepted_Name_with_Author) %>%
  inner_join(Confirmed_Master_List_Tidy %>%
               filter(Data_Source == "GBIF_and_Reference_Flora" | Data_Source == "Reference_Flora"),
             by = "Accepted_Name_with_Author")


Specimen_Counts_Final_NoAutonyms <- Specimen_Counts_Final_NoAutonyms %>%
  mutate(Flagged_Name = if_else(Accepted_Name_with_Author %in% GBIF_and_Reference_Flora_Flag$Accepted_Name_with_Author, 
                                FALSE, 
                                Flagged_Name))

#Count the number of rows where Flagged_Name is TRUE
num_flagged_true <- sum(Specimen_Counts_Final_NoAutonyms$Flagged_Name == TRUE)
#Print the result
cat("Number of rows where Flagged_Name is TRUE:", num_flagged_true, "\n") #500 Flagged Names now, FEB 23, 2025 - 647 Now

#Next, make use of KEW POWO database to unflag specimens known to occur in Greenland
#The KEW database is missing many names which are IN Greenland, but we are confident 
#that the ones known to be in Greenland in KEW are in Greenland. 

#We can do this by extracting the Accepted Names of Taxa which are found in the country of interest according to KEW POWO:

#Set the POWO Query based on the country being used, then find the number of species in that country, then query again setting that number as the limit +1 to be safe. 
all_species_results <- list()
#Loop through each country in the Country_List
for (country in Country_List) {
  query <- list(distribution = country)
  POWO_query_species <- search_powo(query, limit = 1)
  POWO_query_species <- search_powo(query, limit = (POWO_query_species$total + 1))
  country_df <- bind_rows(lapply(POWO_query_species$results, as.data.frame), .id = "species_id")
  country_df$country <- country
  all_species_results[[country]] <- country_df
}
POWO_query_species_final <- bind_rows(all_species_results)

POWO_query_species_final <- POWO_query_species_final %>%
  mutate(Accepted_Name_with_Author = ifelse(
      is.na(author),name,paste(name, author))) %>%
  mutate(Accepted_Name_with_Author = gsub("\\s+", " ", trimws(Accepted_Name_with_Author)))

POWO_query_species_final <- POWO_query_species_final %>%
  mutate(Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author))

#Store for later use
External_5_KEW<-POWO_query_species_final

#Deflag Accepted Names which KEW POWO reports to be in the country at hand
Specimen_Counts_Final_NoAutonyms$Flagged_Name <- ifelse(Specimen_Counts_Final_NoAutonyms$Accepted_Name_with_Author %in% POWO_query_species_final$Accepted_Name_with_Author,
  FALSE,Specimen_Counts_Final_NoAutonyms$Flagged_Name)

#Count the number of rows where Flagged_Name is TRUE
num_flagged_true <- sum(Specimen_Counts_Final_NoAutonyms$Flagged_Name == TRUE)
#Print the result
cat("Number of rows where Flagged_Name is TRUE:", num_flagged_true, "\n") #380, FEB 23, 2025 - 458

#Now take cases of Genera and find if they have a species or lower rank which is NOT flagged. 
#If so, then de-flag them as we know their species or lower rank occurs and is not flagged. 

Specimen_Counts_Final_NoAutonyms <- Specimen_Counts_Final_NoAutonyms %>%
  rowwise() %>%
  mutate(
    #Check if the name is a genus in the Confirmed_Master_List_Tidy
    matched_genus = ifelse(Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy$Accepted_Name_with_Author,
                           Confirmed_Master_List_Tidy$genus[Confirmed_Master_List_Tidy$Accepted_Name_with_Author == Accepted_Name_with_Author & 
                                                              Confirmed_Master_List_Tidy$rank == "Genus"],
                           NA_character_),
    #Check for matching species with the same genus in Confirmed_Master_List_Tidy
    flagged_false = any(Specimen_Counts_Final_NoAutonyms$Accepted_Name_with_Author %in% 
                          Confirmed_Master_List_Tidy$Accepted_Name_with_Author[Confirmed_Master_List_Tidy$genus == matched_genus] &
                          Specimen_Counts_Final_NoAutonyms$Flagged_Name == FALSE, na.rm = TRUE)
  ) %>%
  #Update Flagged_Name based on the checks
  mutate(Flagged_Name = ifelse(flagged_false, FALSE, Flagged_Name)) %>%
  #Clean up the intermediate columns
  select(-matched_genus, -flagged_false)

#Same for Family level
Specimen_Counts_Final_NoAutonyms <- Specimen_Counts_Final_NoAutonyms %>%
  rowwise() %>%
  mutate(
    #Check if the name is a family in the Confirmed_Master_List_Tidy
    matched_family = ifelse(Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy$Accepted_Name_with_Author,
                            Confirmed_Master_List_Tidy$family[Confirmed_Master_List_Tidy$Accepted_Name_with_Author == Accepted_Name_with_Author & 
                                                                Confirmed_Master_List_Tidy$rank == "Family"],
                            NA_character_),
    #Check for matching genera with the same family in Confirmed_Master_List_Tidy
    flagged_false = any(Specimen_Counts_Final_NoAutonyms$Accepted_Name_with_Author %in% 
                          Confirmed_Master_List_Tidy$Accepted_Name_with_Author[Confirmed_Master_List_Tidy$family == matched_family] &
                          Specimen_Counts_Final_NoAutonyms$Flagged_Name == FALSE, na.rm = TRUE)
  ) %>%
  #Update Flagged_Name based on the checks
  mutate(Flagged_Name = ifelse(flagged_false, FALSE, Flagged_Name)) %>%
  #Clean up the intermediate columns
  select(-matched_family, -flagged_false)

#Count the number of rows where Flagged_Name is TRUE
num_flagged_true <- sum(Specimen_Counts_Final_NoAutonyms$Flagged_Name == TRUE)
#Print the result
cat("Number of rows where Flagged_Name is TRUE:", num_flagged_true, "\n") #363 Flagged Names now, FEB 23, 2025 - 436

#Remove cases with " OR " in the name - these are cases of specimens which are double matched due to the same Genus and specific epithet and author being all the same (rare)
Specimen_Counts_Final_NoAutonyms <- Specimen_Counts_Final_NoAutonyms %>%
  filter(!str_detect(Accepted_Name_with_Author, " OR "))
#Count the number of rows where Flagged_Name is TRUE
num_flagged_true <- sum(Specimen_Counts_Final_NoAutonyms$Flagged_Name == TRUE)
#Print the result
cat("Number of rows where Flagged_Name is TRUE:", num_flagged_true, "\n") #363 Flagged Names now, so reduced flagged names by another 2, FEB 23, 2025 - 435

Flagged_Check<-Specimen_Counts_Final_NoAutonyms%>%
  filter(Flagged_Name==TRUE)

Flagged_Check<-Flagged_Check%>%
  select(Accepted_Name_with_Author) 

Flagged_Check$Change_To<-NA
Flagged_Check$Comment<-NA
Flagged_Check$Taxa_Name_Without_Author<-NA

#Download 
write_xlsx(Flagged_Check, "Flagged_Check.xlsx")

cat("!!! Manual Check by Taxonomic Expert !!!\n", 
    "!!! Please Download and Use Flagged_Check.xlsx to make Decisions !!!\n",
    "To Edit, use the column \"Change_To\"\n",
    "- If the name does not belong, write \"REMOVE\"\n",
    "- If the name does belong, write \"UNFLAG\"\n",
    "- If the name needs to be corrected, please paste in the corrected full taxa name and in the Comment column write 'Rename' \n",
    "- If the name needs to be corrected, please paste in the corrected taxa name without author into the Taxa_Name_Without_Author column\n",
    "- If the name and its specimens are valid, just a low occurrence count, please write \"Flagged\"\n",
    "\nUse the \"Comment\" column to add in other commentary.\n")

#Re-Upload the data sheet
#We do it like this because this step is extensive, and sometimes needs to be done by multiple people and
#by people less adept in R, so using an Excel sheet allows for more flexibility. 
Flagged_Check <- read_excel("Flagged_Check.xlsx") #Insert custom file name 

save.image(file = "Taxa_List_Save_25.RData") #Save everything 
#load("Taxa_List_Save_25.RData")
{
#First, remove all REMOVE cases from Confirmed_Master_List_Tidy
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  filter(!Accepted_Name_with_Author %in% (Flagged_Check %>%
                                            filter(tolower(Change_To) == "remove") %>%
                                            pull(Accepted_Name_with_Author)))


#Also remove all remaining FLAGGED cases from Confirmed_Master_List_Tidy
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  filter(!Accepted_Name_with_Author %in% (Flagged_Check %>%
                                            filter(tolower(Change_To) == "flagged") %>%
                                            pull(Accepted_Name_with_Author)))

#Drop all UNFLAG cases from Flagged_Check, these are now validated
Flagged_Check <- Flagged_Check %>%
  filter(tolower(Change_To) != "unflag")

#Characterize the names to add to the final list of removed names 
Removed_Flags_Publish <- Flagged_Check %>%
  filter(tolower(Change_To) == "flagged" | tolower(Change_To) == "remove") %>%
  rename(Verified_Name = Accepted_Name_with_Author, status = Change_To) %>%
  mutate(Status = ifelse(tolower(status) == "flagged", "Low_Number_Occurrences", status))

if (nrow(Removed_Flags_Publish) > 0) {
Removed_Flags_Publish <- Removed_Flags_Publish %>%
  left_join(Combi_List_Storage %>%
              select(Accepted_Name_with_Author, scientificName) %>%
              distinct(), by = c("Verified_Name" = "Accepted_Name_with_Author")) %>%
  group_by(Verified_Name) %>%
  mutate(original_input_name = paste(unique(scientificName), collapse = ", ")) %>%
  ungroup() %>%
  select(-scientificName) %>%
  distinct()

Removed_Flags_Publish <- Removed_Flags_Publish %>%
  mutate(rank = NA_character_) %>% 
  mutate(rank = case_when(
    str_detect(rank, "^[A-Z]+$") ~ NA_character_,  #Replace all-uppercase taxonRanks with NA
    TRUE ~ rank
  )) %>%
  mutate(rank = case_when(
    str_detect(Verified_Name, "× ") ~ "Hybrid",
    str_detect(Verified_Name, "^X ") ~ "Hybrid",
    str_detect(Verified_Name, " [×xX] ") ~ "Hybrid",
    str_detect(Verified_Name, " subsp\\. ") ~ "Subspecies",
    str_detect(Verified_Name, " var\\. ") ~ "Variety",
    str_detect(Verified_Name, " subvar\\. ") ~ "Subvariety",
    str_detect(Verified_Name, " morph") ~ "Morph",
    str_detect(Verified_Name, " f\\. ") ~ "Form",
    str_detect(Verified_Name, " subf\\. ") ~ "Subform",
    str_detect(Verified_Name, " agg\\. ") ~ "Aggregate",
    str_detect(Verified_Name, " aggregate") ~ "Aggregate",
    str_detect(Verified_Name, " sect\\. ") ~ "Section",
    str_detect(Verified_Name, " subsect\\. ") ~ "Subsection",
    str_detect(Verified_Name, " ser\\. ") ~ "Series",
    str_detect(Verified_Name, " subser\\. ") ~ "Subseries",
    str_detect(Verified_Name, " tr\\. ") ~ "Tribe",
    str_detect(Verified_Name, " subtrib\\. ") ~ "Subtribe",
    str_detect(Verified_Name, " gen\\. ") ~ "Genus",
    str_detect(Verified_Name, " subg\\. ") ~ "Subgenus",
    str_detect(Verified_Name, " fam\\. ") ~ "Family",
    str_detect(Verified_Name, "^[A-Z][a-zà-öø-ÿ]+eae\\b") ~ "Family", #If the first word ends with eae
    str_detect(Verified_Name, " subfam\\. ") ~ "Subfamily",
    str_detect(Verified_Name, " ord\\. ") ~ "Order",
    str_detect(Verified_Name, " subord\\. ") ~ "Suborder",
    str_detect(Verified_Name, " cl\\. ") ~ "Class",
    str_detect(Verified_Name, " subcl\\. ") ~ "Subclass",
    TRUE ~ rank  #Keep remaining ranks unchanged
  )) %>%
  #Assign rank for cases where it is still NA
  mutate(rank = case_when(
    is.na(rank) & str_count(Verified_Name, "\\s") >= 1 & 
      str_detect(Verified_Name, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species",  
    is.na(rank) & str_count(Verified_Name, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ rank  
  ))

Removed_Flags_Publish <- Removed_Flags_Publish %>%
  mutate(rank = ifelse(is.na(rank) & 
                         grepl("^[A-Z][a-z]*\\s*\\(?[A-Z][a-z]*", Verified_Name), 
                       "Genus_or_Higher", 
                       rank))

}
}

{
Removed_Flags_Publish<-Removed_Flags_Publish%>%
  select(-c(status))


#Now process the renamed ones:

#Drop all FLAGGED and REMOVE cases from Flagged_Check, these are now validated
Flagged_Check <- Flagged_Check %>%
  filter(!(tolower(Change_To) %in% c("flagged", "remove")))

#Now we have only cases of names which have been corrected. 
#But first remove these cases from Confirmed_Master_List_Tidy
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  filter(!(Data_Source == "GBIF" & Accepted_Name_with_Author %in% Flagged_Check$Accepted_Name_with_Author))

#First, repair the names in the GBIF data:
Combi_List_Storage <- Combi_List_Storage %>%
  mutate(scientificName = ifelse(scientificName %in% Flagged_Check$Accepted_Name_with_Author,
                            Flagged_Check$Change_To[match(scientificName, Flagged_Check$Accepted_Name_with_Author)],
                            scientificName))

Combi_List_Storage <- Combi_List_Storage %>%
  mutate(Accepted_Name_with_Author = ifelse(Accepted_Name_with_Author %in% Flagged_Check$Accepted_Name_with_Author,
                                       Flagged_Check$Change_To[match(Accepted_Name_with_Author, Flagged_Check$Accepted_Name_with_Author)],
                                       Accepted_Name_with_Author))


Combi_List_Storage_1 <- Combi_List_Storage_1 %>%
  mutate(scientificName = ifelse(scientificName %in% Flagged_Check$Accepted_Name_with_Author,
                                 Flagged_Check$Change_To[match(scientificName, Flagged_Check$Accepted_Name_with_Author)],
                                 scientificName))

#Find cases where the corrected name already exists in our dataset 
Matched_Flagged_Names<-Flagged_Check%>%
  filter(Change_To %in% Confirmed_Master_List_Tidy_Names_Storage$Additional_Name) %>%
  rename(Additional_Name_For_This_Dataset = Accepted_Name_with_Author) %>%
  select(Additional_Name_For_This_Dataset, Change_To) #This is to check but we don't use it, as we are not appending in the original incorrect Accepted Names

#Now in the Combi_List_Storage dataframes we have replaced the scientificName of these ones which need to be corrected. 
#Now we also know which names already exist in our data. 
#It was important not to add in these names as additional names because they represent entirely other taxa, since they are Accepted Names. 

#Now process the names which do not already exist in our data, if any

#Here it is important to consider that these are already flagged names, meaning that
#unless we correct the number of specimens to above the threshold value, or unless there is some other
#factor like them being an Autonym, they may remain as flagged and removed. 
#However we still have to process them to determine this. 
UnMatched_Flagged_Names<-Flagged_Check%>%
  filter(!Change_To %in% Confirmed_Master_List_Tidy_Names_Storage$Additional_Name)

UnMatched_Flagged_Names<-UnMatched_Flagged_Names%>%
  rename(Accepted_Name=Taxa_Name_Without_Author)


  {
    df_total_flag1 = data.frame() #for accepted species 
    df_total_flag2 = data.frame() #for synonyms, which have 2 or more rows because one row is the accepted name for that given synonym
    df_total_flag3 = data.frame() #for ones that do not work with the R package KEWR 
    Temp2 <- data.frame(accepted=logical(),
                        author=character(),
                        kingdom=character(),
                        family=character(),
                        name=character(),
                        rank=character(),
                        url=character(),
                        fqId=character(),
                        images=numeric(),
                        synonymOf=numeric(),
                        snippet=numeric())
    
    #Progress Bar
    total_iterations <- length(unique(UnMatched_Flagged_Names$Change_To))
    
    #Initialize the progress bar
    Flagged_Progress_Bar <- progress_bar$new(
      format = "  Processing [:bar] :percent :current/:total Taxon IDs :elapsed",
      total = total_iterations,  #Total number of iterations
      width = 60,                #Width of the progress bar
      clear = FALSE              #Keep the bar after completion
    )
  }
}

  
UnMatched_Flagged_Names$Accepted_Name <- str_trim(UnMatched_Flagged_Names$Accepted_Name)


#Now using the search_powo of the Kewr package, I use each species name in my list and search the KEW database. 
  for (sp in unique(UnMatched_Flagged_Names$Accepted_Name)) {                                               
    Temp1 <- search_powo(sp,)                                                                   
    if (Temp1$total==0) {
      df_total_flag3 <- rbind(df_total_flag3,Temp1$query)  
    } else {
      Temp2 <- tidy(Temp1)                                                                      #Tidy information 
    }
    x <- c("accepted","author","kingdom","family","name","rank", "url","fqId","images", "synonymOf", "snippet")          #Create category images etc , which some outputs do contain
    Temp2[x[!(x %in% colnames(Temp2))]] = 0                                                    #If a Temp2 query doesn't have this column, then add it in with a value of 0, to allow rbinding later. 
    
    if (nrow(Temp2)>=2) {                                                                      #If there are 2 or more rows, store in df_total_flag2 as this is a synonym species. 
      df_total_flag2 <- rbind(df_total_flag2,Temp2)  
    } else {
      df_total_flag1 <- rbind(df_total_flag1,Temp2)                                                      #If there is only one row, store in df_total_flag1 as this is already an accepted name
    }
    
    #Update the progress bar
    Flagged_Progress_Bar$tick()
  }
{
    df_total_flag1 <- df_total_flag1 %>% #Remove duplicates 
      distinct()
    
    df_total_flag2 <- df_total_flag2 %>% #Remove duplicates 
      distinct()
    
    df_total_flag3 <- df_total_flag3 %>% #Remove duplicates 
      distinct()
}
{
#There are only two cases where a name would be kept: if it is an autonym, or if the Accepted Names now includes 
#several of the other names in its synonyms, and the specimen count and institution count now exceed the threshold. 

df_total_flag1<-df_total_flag1 %>%
  filter(name %in% UnMatched_Flagged_Names$Accepted_Name)

df_total_flag2<-df_total_flag2 %>%
  filter(name %in% UnMatched_Flagged_Names$Accepted_Name)  

True_Names2<- df_total_flag2 %>%
  filter(accepted == "TRUE")
df_total_flag1 <- rbind(df_total_flag1,True_Names2)

#Keep synonym names only 
df_total_flag2<- df_total_flag2 %>%
  filter(accepted == "FALSE")
#Un-nest the df to extract the synonym name and corresponding actual name of each species 
df_total_flag2<-tidyr::unnest(df_total_flag2, cols=synonymOf, names_sep="_")

df_total_flag1.1 <- df_total_flag1 %>%
  transmute(Accepted_Taxon_ID = fqId)  
df_total_flag2.1 <- df_total_flag2 %>%
  transmute(Accepted_Taxon_ID = synonymOf_fqId)  
df_total<- data.frame()
df_total <- rbind(df_total_flag1.1, df_total_flag2.1) #List of all species present in the given species list, but with the Taxon ID for their Accepted Species Names, not synonyms 
df_total$Accepted_Taxon_ID <- gsub ("urn:lsid:ipni.org:names:","", df_total$Accepted_Taxon_ID) #remove the jargon
df_total <- df_total %>%
  distinct() #Remove duplicate numbers 

List1 = data.frame()

#Progress Bar
total_iterations <- length(unique(df_total$Accepted_Taxon_ID))

#Initialize the progress bar
Progress_Bar_Leftover <- progress_bar$new(
  format = "  Processing [:bar] :percent :current/:total Taxon IDs :elapsed",
  total = total_iterations,  #Total number of iterations
  width = 60,                
  clear = FALSE              
)
}

for (ac in unique(df_total$Accepted_Taxon_ID)) {           #for each ac accepted species Taxon ID in my list
  Temp1 <- lookup_powo(ac)                         #Search KEW and store information for that accepted Taxon ID
  Temp2 <- tidy(Temp1)                             #Tidy information 
  Temp2 <- add_cols(Temp2, c('basionym','basionymOf','modified','bibliographicCitation','genus','taxonomicStatus','kingdom','phylum','family','nomenclaturalCode','source','namePublishedInYear','taxonRemarks',
                             'nomenclaturalStatus','lifeform','climate', 'hybrid','childNameUsages','synonym','locations','plantae','fungi','fqId', 'name', 'authors','species','rank','reference','classification',
                             'synonyms', 'nomenclaturalRemarks','infraspecies', 'hybridFormula', "paftolId")) #If this fails, check the code of the ac that failed and see what extra columns it has, then add in here. 
  List1 <- rbind(List1,Temp2)  
  
  Progress_Bar_Leftover$tick()
}

{
Synonym_Names2<-NA
Synonym_Names<-NA
Basionyms<-NA
Basionyms2<-NA
Synonym_Names_Authors<-NA
Final_Synonym_Names<-NA

Synonym_Names2<- do.call(rbind.data.frame, List1$synonyms) 

Synonym_Names2<-Synonym_Names2%>%mutate(Synonym_with_Author = paste(name, author, sep = " "))     
List1<-List1%>%mutate(Accepted_Name_with_Author = paste(name, authors, sep = " "))               
List1$Accepted_Name_with_Author<-gsub("NA","", List1$Accepted_Name_with_Author)     
List1$Accepted_Name_with_Author <- trimws(List1$Accepted_Name_with_Author)

Synonym_Names<- setDT(List1)[, .(Synonym = c(name, unlist(synonyms,recursive = TRUE))), by = Accepted_Name_with_Author] 
Synonym_Names<-Synonym_Names %>%                                                                  
  filter(grepl("urn:lsid:ipni.org:names",Synonym_Names$Synonym)) %>% #
  rename("fqId" = "Synonym")

Synonym_Names<-left_join(Synonym_Names, Synonym_Names2, by="fqId")                               
Synonym_Names<-Synonym_Names%>%
  transmute(Accepted_Name_with_Author=Accepted_Name_with_Author,
            Synonym_Names_Authors = Synonym_with_Author)
Basionyms<- try(do.call(rbind.data.frame, List1$basionymOf), silent = TRUE)
Basionyms2 <- try(do.call(rbind.data.frame, List1$basionym), silent = TRUE)
Basionyms <- rbind(Basionyms, Basionyms2)

Basionyms[] <- lapply(Basionyms, function(x) {
  ifelse(grepl("Error in do.call\\(rbind.data.frame, List1\\$basionymOf", x) |
           grepl("Error in do.call\\(rbind.data.frame, List1\\$basionym", x), NA, x)
})
if (!all(is.na(Basionyms))) {
Basionyms <- Basionyms %>%
  drop_na(name) 
Basionym_Names1<- setDT(List1)[, .(Basionym = c(name, unlist(basionymOf,recursive = TRUE))), by = Accepted_Name_with_Author]
Basionym_Names2<- setDT(List1)[, .(Basionym = c(name, unlist(basionym,recursive = TRUE))), by = Accepted_Name_with_Author]
Basionym_Names <- rbind(Basionym_Names1, Basionym_Names2)
Basionym_Names <- Basionym_Names %>%
  drop_na(Basionym) %>% #Remove NAs from columns 
  filter(grepl("urn:lsid:ipni.org:names", Basionym)) %>%
  rename("fqId" = "Basionym")
Basionym_Names<-left_join(Basionym_Names, Basionyms, by="fqId")                                    #Now I join the Synonym to the Accepted Name 
Basionym_Names<-Basionym_Names%>%mutate(Synonym_with_Author = paste(name, author, sep = " "))      #Now I have the Synonyms with their authority names on them. 
Basionym_Names<-Basionym_Names%>%
  transmute(Accepted_Name_with_Author=Accepted_Name_with_Author,
            Synonym_Names_Authors = Synonym_with_Author)
#Now add into Synonym list:
Synonym_Names <- rbind(Synonym_Names,Basionym_Names) #List of all synonyms and Basionyms from KEW dataset per accepted species 
}
Synonym_Names <- Synonym_Names %>%
  distinct() #Remove duplicates

#Remove cases where the Accepted Name and the Synonym in a given row are the same, as this is redundant 
Synonym_Names <- Synonym_Names[Synonym_Names$Accepted_Name_with_Author != Synonym_Names$Synonym_Names_Authors,]

Final_Synonym_Names<- dcast(setDT(Synonym_Names), Accepted_Name_with_Author ~ rowid(Accepted_Name_with_Author), value.var = "Synonym_Names_Authors")
colnames(Final_Synonym_Names)[1] <- "Accepted_Name_with_Author"

Final_Synonym_Names<-Final_Synonym_Names %>% rename_at(vars(2:ncol(Final_Synonym_Names)), ~paste0('Synonym_',.)) #Rename Synonym Columns, which start at Column 2
Final_Synonym_Names_Storage<-Final_Synonym_Names
#Final_Synonym_Names now can allow us to determine if we re-add based on the flagging parameters we set 

Final_Synonym_Names <- Final_Synonym_Names %>%
  pivot_longer(
    cols = -Accepted_Name_with_Author,  #All columns except Accepted_Name_with_Author
    names_to = "Synonym_Type",           #Name of the new column for the former column names
    values_to = "Synonym_Name"           #Name of the new column for the values
  ) %>%
  drop_na(Synonym_Name) %>%
  select(-c(Synonym_Type))

Final_Synonym_Names <- Final_Synonym_Names %>%
  bind_rows(
    Final_Synonym_Names %>%
      select(Accepted_Name_with_Author) %>%
      rename(Synonym_Name = Accepted_Name_with_Author) %>%
      mutate(Accepted_Name_with_Author = Synonym_Name)  #Create the new Accepted_Name_with_Author column
  ) %>%
  distinct()  #Remove duplicates if needed

#Prepare data should it be integrated:
Final_Synonym_Names_Wide <- Final_Synonym_Names %>%
  group_by(Accepted_Name_with_Author) %>%
  mutate(Additional_Name_Index = row_number()) %>%  # Create an index for each synonym
  pivot_wider(
    names_from = Additional_Name_Index,
    values_from = Synonym_Name,
    names_prefix = "Additional_Name_"
  ) %>%ungroup()

UnMatched_Flagged_Names<-UnMatched_Flagged_Names%>%
  rename(original_Accepted_Name_with_Author = Accepted_Name_with_Author) %>%
  select(-c(Comment))

UnMatched_Flagged_Names <- left_join(
  UnMatched_Flagged_Names,
  Specimen_Counts_Final,
  by = c("original_Accepted_Name_with_Author" = "Accepted_Name_with_Author")
) 

save.image(file = "Taxa_List_Save_25.5.RData") #Save everything 
#load("Taxa_List_Save_25.5.RData")
}

UnMatched_Flagged_Names<-UnMatched_Flagged_Names%>%
  select(original_Accepted_Name_with_Author, Change_To, Accepted_Name, Number_of_Specimens, Unique_Institution_Count)

UnMatched_Flagged_Names<-left_join(UnMatched_Flagged_Names, Final_Synonym_Names, by = c("Change_To" = "Synonym_Name"))

UnMatched_Flagged_Names<-UnMatched_Flagged_Names%>%drop_na(Accepted_Name_with_Author)

#First check if any names found in the POWO list for the region, as they can be unflagged automatically:
Flagged_To_Add_POWO<-UnMatched_Flagged_Names%>%
  filter(Accepted_Name_with_Author %in% POWO_query_species_final$Accepted_Name_with_Author)

#If Flagged_To_Add_POWO is not empty, 
if (nrow(Flagged_To_Add_POWO) > 0) {
  
  Flagged_To_Add_POWO<-left_join(Flagged_To_Add_POWO,Final_Synonym_Names_Storage, by = "Accepted_Name_with_Author")
  
  UnMatched_Flagged_Names<-UnMatched_Flagged_Names%>%filter(!Accepted_Name_with_Author %in% Flagged_To_Add_POWO$Accepted_Name_with_Author)
  
  Flagged_To_Add_POWO <- Flagged_To_Add_POWO %>%
    rename_with(~ gsub("^Synonym_", "Additional_Name_", .), starts_with("Synonym_"))
  
  Flagged_To_Add_POWO$Data_Source<-"GBIF"
  
  Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
    filter(!Accepted_Name_with_Author %in% Flagged_To_Add_POWO$Accepted_Name_with_Author) #Remove older version if present 
  
  #Remove the Accepted Names now to be replaced 
  Original_Flagged_Name <- Flagged_Check %>%
    filter(Change_To %in% c(Flagged_To_Add_POWO$Accepted_Name_with_Author, 
                            unlist(select(Flagged_To_Add_POWO, starts_with("Additional")))))
  
  Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
    filter(!Accepted_Name_with_Author %in% Original_Flagged_Name$Accepted_Name_with_Author)
  
  suppressPackageStartupMessages(library(plyr))
  Confirmed_Master_List_Tidy<-rbind.fill(Confirmed_Master_List_Tidy,Flagged_To_Add_POWO) #Add back into Confirmed_Master_List_Tidy
  detach(package:plyr)
  library("dplyr")
  
  #Edit the Accepted Name for the Combi_List_Storage of the Flagged_To_Add_POWO
  Combi_Cleaner_0 <- Flagged_Check %>%
    filter(Change_To %in% unlist(Flagged_To_Add_POWO)) %>%
    mutate(Match_Name = Flagged_To_Add_POWO$Accepted_Name_with_Author[apply(Flagged_To_Add_POWO, 1, function(x) any(Change_To %in% x))])
  
  Combi_List_Storage <- Combi_List_Storage %>%
    mutate(Accepted_Name_with_Author = ifelse(Accepted_Name_with_Author %in% Combi_Cleaner_0$Accepted_Name_with_Author,
                                              Combi_Cleaner_0$Match_Name[match(Accepted_Name_with_Author, Combi_Cleaner_0$Accepted_Name_with_Author)],
                                              Accepted_Name_with_Author))
}

#Now per Accepted_Name_with_Author, sum the total specimen counts (cannot flag here for number of institutions)
Flagged_To_Add <- UnMatched_Flagged_Names %>%
  group_by(Accepted_Name_with_Author) %>%
  dplyr::summarise(Total_Specimens = sum(Number_of_Specimens, na.rm = TRUE), .groups = 'drop') %>%
  filter(Total_Specimens > Count_Value) %>% select(Accepted_Name_with_Author)
    
#If Flagged_To_Add is not empty, 
if (nrow(Flagged_To_Add) > 0) {
  Flagged_To_Add<-List1%>%
    filter(Accepted_Name_with_Author %in% Flagged_To_Add$Accepted_Name_with_Author)
    
  Flagged_To_Add<-left_join(Flagged_To_Add,Final_Synonym_Names_Storage, by = "Accepted_Name_with_Author")
    
  UnMatched_Flagged_Names<-UnMatched_Flagged_Names%>%filter(!Accepted_Name_with_Author %in% Flagged_To_Add$Accepted_Name_with_Author)
    
  Flagged_To_Add <- Flagged_To_Add %>%
    rename_with(~ gsub("^Synonym_", "Additional_Name_", .), starts_with("Synonym_"))
    
  Flagged_To_Add$Data_Source<-"GBIF"
  
  Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
    filter(!Accepted_Name_with_Author %in% Flagged_To_Add$Accepted_Name_with_Author) #Remove older version if present 
  
  #Remove the Accepted Names now to be replaced 
  Original_Flagged_Name <- Flagged_Check %>%
    filter(Change_To %in% c(Flagged_To_Add$Accepted_Name_with_Author, 
                            unlist(select(Flagged_To_Add, starts_with("Additional")))))
  
  Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
    filter(!Accepted_Name_with_Author %in% Original_Flagged_Name$Accepted_Name_with_Author)
  
suppressPackageStartupMessages(library(plyr))
Confirmed_Master_List_Tidy<-rbind.fill(Confirmed_Master_List_Tidy,Flagged_To_Add) #Add back into Confirmed_Master_List_Tidy
detach(package:plyr)
library("dplyr")
  
#Edit the Accepted Name for the Combi_List_Storage of the Flagged_To_Add
  Combi_Cleaner <- Flagged_Check %>%
    filter(Change_To %in% unlist(Flagged_To_Add)) %>%
    mutate(Match_Name = Flagged_To_Add$Accepted_Name_with_Author[apply(Flagged_To_Add, 1, function(x) any(Change_To %in% x))])
  
  Combi_List_Storage <- Combi_List_Storage %>%
    mutate(Accepted_Name_with_Author = ifelse(Accepted_Name_with_Author %in% Combi_Cleaner$Accepted_Name_with_Author,
                                              Combi_Cleaner$Match_Name[match(Accepted_Name_with_Author, Combi_Cleaner$Accepted_Name_with_Author)],
                                              Accepted_Name_with_Author))
  
}

#Combine remaining names and search for autonyms 
Remaining_Flags <- UnMatched_Flagged_Names %>%
  select(Accepted_Name_with_Author) %>%
  bind_rows(df_total_flag3 %>% rename(Accepted_Name_with_Author = 1)) 

Flagged_Autonym <- Remaining_Flags %>%
  mutate(Is_Autonym = sapply(Accepted_Name_with_Author, search_autonym)) %>%
  filter(Is_Autonym) %>%  
  select(-Is_Autonym) 
  
if (nrow(Flagged_Autonym) > 0) {  
  
  Flagged_Autonym<-left_join(Flagged_Autonym, List1, by = "Accepted_Name_with_Author")
  Flagged_Autonym<-left_join(Flagged_Autonym, Final_Synonym_Names_Storage, by = "Accepted_Name_with_Author")
  Flagged_Autonym <- Flagged_Autonym %>%
    rename_with(~ gsub("^Synonym_", "Additional_Name_", .), starts_with("Synonym_"))
  Remaining_Flags<-Remaining_Flags%>%filter(!Accepted_Name_with_Author %in% Flagged_Autonym$Accepted_Name_with_Author)   
  Flagged_Autonym$Autonym<-TRUE
  Flagged_Autonym$Data_Source<-"GBIF"
  Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
    filter(!Accepted_Name_with_Author %in% Flagged_Autonym$Accepted_Name_with_Author) #Remove older version of new Accepted Name present, if it is an Accepted Name
  
  #Remove the Accepted Names now to be replaced 
  Original_Autonym_Name <- Flagged_Check %>%
    filter(Change_To %in% c(Flagged_Autonym$Accepted_Name_with_Author, 
                            unlist(select(Flagged_Autonym, starts_with("Additional")))))
  
  Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
    filter(!Accepted_Name_with_Author %in% Original_Autonym_Name$Accepted_Name_with_Author)
  
  suppressPackageStartupMessages(library(plyr))
  Confirmed_Master_List_Tidy<-rbind.fill(Confirmed_Master_List_Tidy,Flagged_Autonym) #Add back into Confirmed_Master_List_Tidy the new replacement Accepted Names
  detach(package:plyr)
  library("dplyr")
  
  #Edit the Accepted Name for the Combi_List_Storage of the Flagged_Autonym
  Combi_Cleaner <- Flagged_Check %>%
    filter(Change_To %in% unlist(Flagged_Autonym)) %>%
    mutate(Match_Name = Flagged_Autonym$Accepted_Name_with_Author[apply(Flagged_Autonym, 1, function(x) any(Change_To %in% x))])
  
  Combi_Cleaner_2<-Combi_Cleaner
  
  Combi_List_Storage <- Combi_List_Storage %>%
    mutate(Accepted_Name_with_Author = ifelse(Accepted_Name_with_Author %in% Combi_Cleaner$Accepted_Name_with_Author,
                                              Combi_Cleaner$Match_Name[match(Accepted_Name_with_Author, Combi_Cleaner$Accepted_Name_with_Author)],
                                              Accepted_Name_with_Author))
}
   
#Now add remaining Remaining_Flags to Removed_Flags_Publish
Remaining_Flags<-Flagged_Check%>%   
  filter(Change_To %in% Remaining_Flags$Accepted_Name_with_Author)

#Remove from Confirmed_Master_List_Tidy$Accepted_Name_with_Author
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  filter(!Accepted_Name_with_Author %in% Remaining_Flags$Accepted_Name_with_Author)

Remaining_Flags$status<-NA

Remaining_Flags <- Remaining_Flags %>%
  mutate(status = ifelse(Change_To %in% unlist(Final_Synonym_Names_Storage), 
                         "Low_Number_Occurrences", 
                         "Unrecognized_Name"))

Remaining_Flags<-Remaining_Flags %>%
  rename(Verified_Name = Change_To,
  original_input_name = Accepted_Name_with_Author)

Remaining_Flags <- Remaining_Flags %>%
  mutate(rank = NA_character_) %>% 
  mutate(rank = case_when(
    str_detect(rank, "^[A-Z]+$") ~ NA_character_,  #Replace all-uppercase taxonRanks with NA
    TRUE ~ rank
  )) %>%
  mutate(rank = case_when(
    str_detect(Verified_Name, "× ") ~ "Hybrid",
    str_detect(Verified_Name, "^X ") ~ "Hybrid",
    str_detect(Verified_Name, " [×xX] ") ~ "Hybrid",
    str_detect(Verified_Name, " subsp\\. ") ~ "Subspecies",
    str_detect(Verified_Name, " var\\. ") ~ "Variety",
    str_detect(Verified_Name, " subvar\\. ") ~ "Subvariety",
    str_detect(Verified_Name, " morph") ~ "Morph",
    str_detect(Verified_Name, " f\\. ") ~ "Form",
    str_detect(Verified_Name, " subf\\. ") ~ "Subform",
    str_detect(Verified_Name, " agg\\. ") ~ "Aggregate",
    str_detect(Verified_Name, " aggregate") ~ "Aggregate",
    str_detect(Verified_Name, " sect\\. ") ~ "Section",
    str_detect(Verified_Name, " subsect\\. ") ~ "Subsection",
    str_detect(Verified_Name, " ser\\. ") ~ "Series",
    str_detect(Verified_Name, " subser\\. ") ~ "Subseries",
    str_detect(Verified_Name, " tr\\. ") ~ "Tribe",
    str_detect(Verified_Name, " subtrib\\. ") ~ "Subtribe",
    str_detect(Verified_Name, " gen\\. ") ~ "Genus",
    str_detect(Verified_Name, " subg\\. ") ~ "Subgenus",
    str_detect(Verified_Name, " fam\\. ") ~ "Family",
    str_detect(Verified_Name, "^[A-Z][a-zà-öø-ÿ]+eae\\b") ~ "Family", #If the first word ends with eae
    str_detect(Verified_Name, " subfam\\. ") ~ "Subfamily",
    str_detect(Verified_Name, " ord\\. ") ~ "Order",
    str_detect(Verified_Name, " subord\\. ") ~ "Suborder",
    str_detect(Verified_Name, " cl\\. ") ~ "Class",
    str_detect(Verified_Name, " subcl\\. ") ~ "Subclass",
    TRUE ~ rank  #Keep remaining ranks unchanged
  )) %>%
  #Assign rank for cases where it is still NA
  mutate(rank = case_when(
    is.na(rank) & str_count(Verified_Name, "\\s") >= 1 & 
      str_detect(Verified_Name, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species",  
    is.na(rank) & str_count(Verified_Name, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ rank  
  ))

Remaining_Flags <- Remaining_Flags %>%
  mutate(rank = ifelse(is.na(rank) & 
                         grepl("^[A-Z][a-z]*\\s*\\(?[A-Z][a-z]*", Verified_Name), 
                       "Genus_or_Higher", 
                       rank))

Remaining_Flags<-Remaining_Flags%>%
  rename(Status = status)

Removed_Flags_Publish<-rbind(Removed_Flags_Publish, Remaining_Flags)

Removed_Flags_Publish<-Removed_Flags_Publish%>%
  select(-Taxa_Name_Without_Author)

Removed_Flags_Publish <- Removed_Flags_Publish %>%
  mutate(Status = ifelse(Status == "REMOVE", "Unclear_Labelling_Geography_Image_or_Other", Status))

#Write to Excel File:
#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("Removed_Flagged_Names_", today_date, ".xlsx")

#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)

#Write to Excel file
write_xlsx(Removed_Flags_Publish, file_name)

#Set the working directory back to the original directory
setwd("..")

save.image(file = "Taxa_List_Save_26.RData") #Save everything 
#load("Taxa_List_Save_26.RData")

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Final Step - Processing and Cleaning the Lists
{
Confirmed_Master_List_Tidy$Accepted_Name_with_Author <- str_trim(Confirmed_Master_List_Tidy$Accepted_Name_with_Author)

#Re-order
Confirmed_Master_List_Tidy<- Confirmed_Master_List_Tidy%>%
  select("Accepted_Name_with_Author", "synonym", "KEW ID", "taxonomicStatus", "Data_Source", "Autonym", "kingdom", "phylum", "family", "genus", "species", "rank", "plantae", "fungi", "hybrid", "namePublishedInYear", "authors", "reference", "nomenclaturalCode", "nomenclaturalStatus", "lifeform", "climate", "taxonRemarks", "locations", everything())

#Now tidy up 
Confirmed_Master_List_Tidy_Names<-Confirmed_Master_List_Tidy %>%
  select(Accepted_Name_with_Author,starts_with("Additional_"), starts_with("reference_flora_name"), starts_with("Synonym_"), starts_with("original_input_name_"))

#Now shift all names to the left to fill in NA cells (tidy it up)
Confirmed_Master_List_Tidy_Names <- as.data.frame(t(apply(Confirmed_Master_List_Tidy_Names, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))))
colnames(Confirmed_Master_List_Tidy_Names)[1] <- "Accepted_Name_with_Author"
colnames(Confirmed_Master_List_Tidy_Names) <- sub("V", "", colnames(Confirmed_Master_List_Tidy_Names))
Confirmed_Master_List_Tidy_Names<-Confirmed_Master_List_Tidy_Names %>% 
  rename_at(vars(2:ncol(Confirmed_Master_List_Tidy_Names)), ~paste0('Additional_Name_',.)) %>% #Rename Input Name Columns 
  select(where(~!all(is.na(.x)))) #Remove for example any columns where all rows are NA.

Confirmed_Master_List_Tidy_Names <- Confirmed_Master_List_Tidy_Names %>% 
  rename_at(vars(2:ncol(Confirmed_Master_List_Tidy_Names)), ~paste0('Additional_Name_', seq_along(.)))

#Now over the many times the data has been amalgamated, there are cases where some of the additional names are duplicates. 
#Clean this up too, by pivoting the data to long format and removing duplicate cases, then pivoting back. 

Confirmed_Master_List_Tidy_Names <- Confirmed_Master_List_Tidy_Names %>%
  mutate(Additional_Name_1 = ifelse(is.na(Additional_Name_1), Accepted_Name_with_Author, Additional_Name_1))

#Pivot all columns except "Accepted_Name_with_Author" into long format
Confirmed_Master_List_Tidy_Names <- Confirmed_Master_List_Tidy_Names %>%
  pivot_longer(
    cols = -Accepted_Name_with_Author, #Exclude the column "Accepted_Name_with_Author"
    names_to = "Additional_Name_Number",               #New column for the names of the original columns
    values_to = "Additional_Name"                  #New column for the values from those columns
  ) %>%
  select(Accepted_Name_with_Author, Additional_Name) %>%
  filter(!is.na(Additional_Name)) %>%
  distinct(Accepted_Name_with_Author, Additional_Name)

#Add in copy of Accepted Name as Synonym for future use as extra precaution
Accepted_Name_Rows <- Confirmed_Master_List_Tidy_Names %>%
  mutate(Additional_Name = Accepted_Name_with_Author) %>%  
  select(Accepted_Name_with_Author, Additional_Name) %>% distinct()

Confirmed_Master_List_Tidy_Names<-rbind(Confirmed_Master_List_Tidy_Names,Accepted_Name_Rows)

Confirmed_Master_List_Tidy_Names<-Confirmed_Master_List_Tidy_Names%>% distinct()

#Now pivot the names back to long format, and append them back into the species list with the descriptor data
Confirmed_Master_List_Tidy_Names <- Confirmed_Master_List_Tidy_Names %>%
  group_by(Accepted_Name_with_Author) %>%
  mutate(name_id = row_number()) %>%  #Unique ID for each Additional_Name within the same Accepted_Name_with_Author
  pivot_wider(
    names_from = name_id,
    values_from = Additional_Name,
    names_prefix = "Additional_Name_"
  ) %>%
  ungroup()  #Remove the grouping

#Select dataframe only data descriptors and accepted name and also drop some other columns not needed 
Confirmed_Master_List_Tidy_Descriptors <- Confirmed_Master_List_Tidy %>%
  select(-c(starts_with("Additional_"), starts_with("reference_flora_name"), starts_with("Synonym_"), starts_with("original_input_name_")))

#Left join back together
Confirmed_Master_List_Tidy<-NA 
Confirmed_Master_List_Tidy<-left_join(Confirmed_Master_List_Tidy_Descriptors, Confirmed_Master_List_Tidy_Names, by="Accepted_Name_with_Author" )
}
{
#Final editing of Rank Data:
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(rank = NA_character_) %>% 
  mutate(rank = case_when(
    str_detect(rank, "^[A-Z]+$") ~ NA_character_,  #Replace all-uppercase taxonRanks with NA
    TRUE ~ rank
  )) %>%
  mutate(rank = case_when(
    str_detect(Accepted_Name_with_Author, "× ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, "^X ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, " [×xX] ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, " subsp\\. ") ~ "Subspecies",
    str_detect(Accepted_Name_with_Author, " var\\. ") ~ "Variety",
    str_detect(Accepted_Name_with_Author, " subvar\\. ") ~ "Subvariety",
    str_detect(Accepted_Name_with_Author, " morph") ~ "Morph",
    str_detect(Accepted_Name_with_Author, " f\\. ") ~ "Form",
    str_detect(Accepted_Name_with_Author, " subf\\. ") ~ "Subform",
    str_detect(Accepted_Name_with_Author, " agg\\. ") ~ "Aggregate",
    str_detect(Accepted_Name_with_Author, " aggregate") ~ "Aggregate",
    str_detect(Accepted_Name_with_Author, " sect\\. ") ~ "Section",
    str_detect(Accepted_Name_with_Author, " subsect\\. ") ~ "Subsection",
    str_detect(Accepted_Name_with_Author, " ser\\. ") ~ "Series",
    str_detect(Accepted_Name_with_Author, " subser\\. ") ~ "Subseries",
    str_detect(Accepted_Name_with_Author, " tr\\. ") ~ "Tribe",
    str_detect(Accepted_Name_with_Author, " subtrib\\. ") ~ "Subtribe",
    str_detect(Accepted_Name_with_Author, " gen\\. ") ~ "Genus",
    str_detect(Accepted_Name_with_Author, " subg\\. ") ~ "Subgenus",
    str_detect(Accepted_Name_with_Author, " fam\\. ") ~ "Family",
    str_detect(Accepted_Name_with_Author, "^[A-Z][a-zà-öø-ÿ]+eae\\b") ~ "Family", #If the first word ends with eae
    str_detect(Accepted_Name_with_Author, " subfam\\. ") ~ "Subfamily",
    str_detect(Accepted_Name_with_Author, " ord\\. ") ~ "Order",
    str_detect(Accepted_Name_with_Author, " subord\\. ") ~ "Suborder",
    str_detect(Accepted_Name_with_Author, " cl\\. ") ~ "Class",
    str_detect(Accepted_Name_with_Author, " subcl\\. ") ~ "Subclass",
    TRUE ~ rank  #Keep remaining ranks unchanged
  )) %>%
  #Assign rank for cases where it is still NA
  mutate(rank = case_when(
    is.na(rank) & str_count(Accepted_Name_with_Author, "\\s") >= 1 & 
      str_detect(Accepted_Name_with_Author, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species",  
    is.na(rank) & str_count(Accepted_Name_with_Author, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ rank  
  ))

Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(rank = case_when(
    is.na(rank) & 
      str_detect(Accepted_Name_with_Author, 
                 "^\\S+\\s+(\\(?[A-ZÀ-ÖØ-Ý][a-zà-öø-ÿ]*\\)?|[A-Z]\\.)") ~ "Genus",
    TRUE ~ rank
  ))

#Additional Cleaning:
ranks_to_check <- c("Subform", "Form", "Subvariety", "Variety", "Subspecies", "Species")

#Update the genus column if conditions are met
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(
    genus = ifelse(
      rank %in% ranks_to_check & is.na(genus),
      word(Accepted_Name_with_Author, 1),  #Extract the first word as the genus name
      genus))

#Update the species column if conditions are met
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(
    species = ifelse(
      rank %in% ranks_to_check & is.na(species),
      word(Accepted_Name_with_Author, 2),  #Extract the first word as the genus name
      species))

#Re-order
Confirmed_Master_List_Tidy<- Confirmed_Master_List_Tidy%>%
  select("Accepted_Name_with_Author", "synonym", "KEW ID", "taxonomicStatus", "Data_Source", "Autonym", "kingdom", "phylum","clazz","subclass", "order", "family", "genus", "species","infraspecies", "rank", "hybrid", "namePublishedInYear", "authors", "reference", "nomenclaturalCode", "nomenclaturalStatus", "lifeform", "climate", "taxonRemarks", "locations", everything()) %>%
  rename(class = clazz)

Confirmed_Master_List_Tidy<- Confirmed_Master_List_Tidy%>%
  select(-c(plantae,fungi,name, fqId))

Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(synonym = ifelse(taxonomicStatus == "Accepted" & is.na(synonym), FALSE, synonym)) %>%
  mutate(taxonomicStatus = ifelse(Data_Source == "Reference_Flora" & is.na(taxonomicStatus), "Accepted_By_Reference_Flora", taxonomicStatus)) %>%
  mutate(hybrid = ifelse(rank == "Hybrid", 1, hybrid))%>%
  mutate(infraspecies = ifelse(
      rank %in% ranks_to_check & is.na(infraspecies),
      word(Accepted_Name_with_Author, 4),  #Take the 4th word representing the infraspecies 
      infraspecies))

Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(infraspecies = ifelse(
      !(rank %in% ranks_to_check) | rank == "Species",  #Check if rank is not in ranks_to_check or is "Species"
      NA,  #Set infraspecies to NA
      infraspecies))

Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(across(c(genus, species, hybrid, namePublishedInYear, authors, reference, nomenclaturalCode, 
                  nomenclaturalStatus, lifeform, climate, taxonRemarks), 
                ~ ifelse(. == 0, NA, .)))

save.image(file = "Taxa_List_Save_27.RData") #Save everything 
#load("Taxa_List_Save_27.RData")
}
#Note that for some reason, the KEWR package does not append in correct information
#for taxa rank information higher than Family level... it adds in valid but incorrect names
#So we will use another source to repair this in the final list
#We employ the taxize package and query the Integrated Taxonomic Information System.
#When information is unavailable, it is left as NA. 

#First, there are cases where the family information is unknown, such as for the reference
#flora and for the autonyms. 
{
#Use existing data to save query time
#Filter out valid genera (where taxonomicStatus is NOT "Unplaced")
valid_genera <- Confirmed_Master_List_Tidy %>%
  filter(taxonomicStatus != "Unplaced") %>%
  select(genus, family) %>% distinct()

#Make a table to match Genus to known families 
lookup_family <- valid_genera %>%
  filter(!is.na(family))

#Update the Confirmed_Master_List_Tidy family names for empty genera when possible 
for (i in 1:nrow(Confirmed_Master_List_Tidy)) {
  genus_name <- Confirmed_Master_List_Tidy$genus[i]
  
  #Check if the family for this genus is available in the lookup table
  if (is.na(Confirmed_Master_List_Tidy$family[i]) && genus_name %in% lookup_family$genus) {
    #Get the corresponding family value
    family_value <- lookup_family$family[lookup_family$genus == genus_name]
    
    #Update the family in Confirmed_Master_List_Tidy
    Confirmed_Master_List_Tidy$family[i] <- family_value
  }
}

#Now for cases where there is not a match between Genus and Family
#Identify unique genera that need family information
unique_genera <- Confirmed_Master_List_Tidy %>%
  filter(is.na(family) & !is.na(genus) & genus != "") %>%
  distinct(genus)

if (nrow(unique_genera) > 0) {
#List to store results
ipni_results <- list()

#Loop through each genus and search IPNI
for (genus in unique_genera$genus) {
  genus <- trimws(genus)  #Clean the genus name
  
  #Print the genus being searched
  message("Searching IPNI for genus: '", genus, "'")
  
  #Use KEWR's search_ipni function and handle errors
  result <- tryCatch({
    search_ipni(genus)
  }, error = function(e) {
    message(paste("Error searching IPNI for genus:", genus, "|", e$message))
    return(NULL)  #Return NULL on error
  })
  
  #Store the result unlisted if it's not NULL
  if (!is.null(result)) {
    ipni_results[[genus]] <- unlist(result)  #Store the result unlisted
  } else {
    message("No valid result found for genus: '", genus, "'")
  }
}

#Empty data frame to store the results
family_results <- data.frame(genus = character(), family = character(), stringsAsFactors = FALSE)

#Loop through the ipni_results list to extract family information
for (genus in names(ipni_results)) {
  #Extract family information
  family_info <- ipni_results[[genus]]$results.family
  #Create a data frame for the current genus and its family
  temp_df <- data.frame(genus = genus, family = family_info, stringsAsFactors = FALSE)
  #Combine with family_results
  family_results <- rbind(family_results, temp_df)
}

for (i in 1:nrow(family_results)) {
  family_name <- family_results$family[i]
  genus_name <- family_results$genus[i]
  
  #Update the family column in Confirmed_Master_List_Tidy based on genus match
  Confirmed_Master_List_Tidy$family[Confirmed_Master_List_Tidy$genus == genus_name] <- family_name
}
}

save.image(file = "Taxa_List_Save_28.RData") #Save everything 
#load("Taxa_List_Save_28.RData")
}
#Now use the correct KEW family information to update the higher taxa information 
{
Confirmed_Master_List_Tidy<-Confirmed_Master_List_Tidy%>%select(-c(kingdom, phylum, class, subclass, order))

#New columns for taxonomic information
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(Kingdom = NA, Subkingdom = NA, Infrakingdom = NA, Superdivision = NA,
         Division = NA, Subdivision = NA, Class = NA, Superorder = NA,
         Order = NA, Family = family) 
  
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  select("Accepted_Name_with_Author", "synonym", "KEW ID", "taxonomicStatus", "Data_Source", "Autonym", "Kingdom","Subkingdom","Infrakingdom","Superdivision", "Division","Subdivision","Class", "Superorder", "Order","Family", "genus", "species","infraspecies", "rank", "hybrid", "namePublishedInYear", "authors", "reference", "nomenclaturalCode", "nomenclaturalStatus", "lifeform", "climate", "taxonRemarks", "locations", everything()) 

#Create a unique list of families to query
unique_families <- unique(Confirmed_Master_List_Tidy$family)

#Loop through each family and retrieve taxonomic information
for (family_name in unique_families) {
  #Get taxonomic classification from the ITIS database for the current family
  tax_info_itis <- tryCatch(
    classification(family_name, db = "itis"),
    error = function(e) {
      cat("Error fetching data from ITIS for family:", family_name, "\n")
      return(NULL)  #Return NULL on error
    }
  )
  
  #Check if tax_info_itis is valid
  if (!is.null(tax_info_itis)) {
    #Extract the relevant information from tax_info_itis
    tax_info_df <- as.data.frame(tax_info_itis[[family_name]])
    #Check and print the extracted data for debugging
    if (nrow(tax_info_df) > 0) {
      #Check if the kingdom is Plantae
      kingdom_value <- tax_info_df$name[tax_info_df$rank == "kingdom"]
      if (length(kingdom_value) > 0 && kingdom_value == "Plantae") {
        print(paste("Populating data for family:", family_name))  #Debugging output
        print(tax_info_df)  #Show what we've extracted
        #Assign taxonomic classifications to the corresponding rows in Confirmed_Master_List_Tidy
        if ("kingdom" %in% tax_info_df$rank) {
          Confirmed_Master_List_Tidy$Kingdom[Confirmed_Master_List_Tidy$family == family_name] <- kingdom_value
        }
        if ("subkingdom" %in% tax_info_df$rank) {
          Confirmed_Master_List_Tidy$Subkingdom[Confirmed_Master_List_Tidy$family == family_name] <- tax_info_df$name[tax_info_df$rank == "subkingdom"]
        }
        if ("infrakingdom" %in% tax_info_df$rank) {
          Confirmed_Master_List_Tidy$Infrakingdom[Confirmed_Master_List_Tidy$family == family_name] <- tax_info_df$name[tax_info_df$rank == "infrakingdom"]
        }
        if ("superdivision" %in% tax_info_df$rank) {
          Confirmed_Master_List_Tidy$Superdivision[Confirmed_Master_List_Tidy$family == family_name] <- tax_info_df$name[tax_info_df$rank == "superdivision"]
        }
        if ("division" %in% tax_info_df$rank) {
          Confirmed_Master_List_Tidy$Division[Confirmed_Master_List_Tidy$family == family_name] <- tax_info_df$name[tax_info_df$rank == "division"]
        }
        if ("subdivision" %in% tax_info_df$rank) {
          Confirmed_Master_List_Tidy$Subdivision[Confirmed_Master_List_Tidy$family == family_name] <- tax_info_df$name[tax_info_df$rank == "subdivision"]
        }
        if ("class" %in% tax_info_df$rank) {
          Confirmed_Master_List_Tidy$Class[Confirmed_Master_List_Tidy$family == family_name] <- tax_info_df$name[tax_info_df$rank == "class"]
        }
        if ("superorder" %in% tax_info_df$rank) {
          Confirmed_Master_List_Tidy$Superorder[Confirmed_Master_List_Tidy$family == family_name] <- tax_info_df$name[tax_info_df$rank == "superorder"]
        }
        if ("order" %in% tax_info_df$rank) {
          Confirmed_Master_List_Tidy$Order[Confirmed_Master_List_Tidy$family == family_name] <- tax_info_df$name[tax_info_df$rank == "order"]
        }
        if ("family" %in% tax_info_df$rank) {
          Confirmed_Master_List_Tidy$Family[Confirmed_Master_List_Tidy$family == family_name] <- tax_info_df$name[tax_info_df$rank == "family"]
        }
      } else {
        cat("Family", family_name, "does not belong to Plantae. Skipping...\n")
      }
    }
  } else {
    cat("No valid data returned for family:", family_name, "\n")
  }
}

}
#Remove final cases of complete duplication 
Confirmed_Master_List_Tidy<-Confirmed_Master_List_Tidy%>%distinct()

#Also do one last tidy of repeated names within each Accepted Name
long_format_data <- Confirmed_Master_List_Tidy %>%
  mutate(Accepted_Name_with_Author = trimws(Accepted_Name_with_Author)) %>% #Trim white spaces 
  select(Accepted_Name_with_Author, starts_with("Additional")) %>%
  pivot_longer(
    cols = starts_with("Additional"),     #Only pivot the Additional columns
    names_to = "source",                  #Column for the original column names
    values_to = "name"                    #Column for the names
  ) %>%
  filter(!is.na(name)) %>%
  filter(Accepted_Name_with_Author != name) %>%
  group_by(Accepted_Name_with_Author) %>%
  mutate(source = paste0("Additional_Name_", row_number())) %>% #Renaming sources
  ungroup()

Confirmed_Master_List_Tidy_Descriptors <- Confirmed_Master_List_Tidy %>%
  select(-starts_with("Additional"))

#Note that there can be some rare cases where a single white space in autonyms can make them treated as separate names
#It only happens when one is from GBIF and the other is from GBIF_and_Reference_Flora
#We can find these cases and repair them. 

duplicate_trimmed_names <- Confirmed_Master_List_Tidy %>%
  mutate(Trimmed_Name = trimws(Accepted_Name_with_Author)) %>%
  group_by(Trimmed_Name) %>%
  filter(n() > 1) %>%                   #Keep only those with duplicates
  ungroup() %>%
  select(Accepted_Name_with_Author, Trimmed_Name) %>%  #Select original and trimmed names
  distinct()  

Confirmed_Master_List_Tidy_Descriptors <- Confirmed_Master_List_Tidy_Descriptors %>%
  mutate(Accepted_Name_with_Author = trimws(Accepted_Name_with_Author))

Confirmed_Master_List_Tidy_Descriptors <- Confirmed_Master_List_Tidy_Descriptors %>%
  filter(Accepted_Name_with_Author %in% duplicate_trimmed_names$Trimmed_Name) %>%  
  group_by(Accepted_Name_with_Author) %>%
  arrange(desc(taxonomicStatus == "Accepted"), desc(Data_Source == "GBIF_and_Reference_Flora")) %>%
  slice(1) %>%  #Keep the prioritized row for each duplicate
  ungroup() %>%
  bind_rows(Confirmed_Master_List_Tidy_Descriptors %>% 
              filter(!(Accepted_Name_with_Author %in% duplicate_trimmed_names$Trimmed_Name)))  

Confirmed_Master_List_Tidy_Names <- long_format_data %>%
  pivot_wider(
    names_from = source,                   #Use the source as new column names
    values_from = name,                    #The values to fill in the new columns
    values_fill = list(name = "")          #Fill missing values with empty strings
  ) %>%
  mutate(across(everything(), ~ na_if(., ""))) %>%  #Convert empty strings back to NA for further manipulation
  select(Accepted_Name_with_Author, everything()) %>%  #Ensure Accepted_Name_with_Author is first
  filter(rowSums(is.na(.)) < ncol(.)) 

Confirmed_Master_List_Tidy<-NA

Confirmed_Master_List_Tidy<-left_join(Confirmed_Master_List_Tidy_Descriptors,Confirmed_Master_List_Tidy_Names, by="Accepted_Name_with_Author")

#Remove old family column
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  select(-family)

#Repair Hybrid genera 
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(rank = if_else(
    rank == "Hybrid" & grepl("^×", Accepted_Name_with_Author) & 
      #Split by spaces and check if the third word (after "×") is capitalized
      sapply(strsplit(Accepted_Name_with_Author, " "), function(x) {
        if (length(x) >= 3) {
          return(toupper(substr(x[3], 1, 1)) == substr(x[3], 1, 1))  #Check if 3rd word starts with capital letter
        } else {return(FALSE)}}), "Genus",rank))

#Mutate a column dictating the species level name, Species_Level_Name
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author),
         Species_Level_Name = case_when(
           rank == "Hybrid" ~ str_extract(Accepted_Name_with_Author, "^[^×]*×\\s*\\S+"),  #Keep Hybrid logic as is
           rank == "Species" ~ word(Accepted_Name_with_Author, 1, 2),  #Apply for rank "Species"
           rank %in% c("Subspecies", "Variety", "Subvariety", "Form", "Subform") & str_detect(Accepted_Name_with_Author, "^×") ~ word(Accepted_Name_with_Author, 1, 2),  #Apply only for specified ranks and names starting with "×"
           rank %in% c("Subspecies", "Variety", "Subvariety", "Form", "Subform") ~ word(Accepted_Name_with_Author, 1, 2),  #Apply for other ranks too
           TRUE ~ NA_character_  #Default action for other ranks: set to NA
         ))
#Reposition Species_Level_Name
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  select(-Species_Level_Name, everything()) %>%
  select(c(1:(which(grepl("^Additional", names(.)))[1] - 1), "Species_Level_Name", 
           (which(grepl("^Additional", names(.)))[1]:ncol(.))))

#Repair any Aggregates 
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(Accepted_Name_with_Author = if_else(
    rank == "Aggregate" & str_starts(Accepted_Name_with_Author, "The "), str_sub(Accepted_Name_with_Author, 5),Accepted_Name_with_Author))

cat("Your Taxa Lists will now be saved. However, we recommend to check this Taxa List against External Sources and with Experts, and determine if any names need to be removed still. If YES, there will be an opportunity to do this and then override the saved files with the final list.\n")

save.image(file = "Taxa_List_Save_29.5.RData") #Save everything 
#load("Taxa_List_Save_29.5.RData")

#Quick modification to Removed_Flags_Publish
Removed_Flags_Publish <- Removed_Flags_Publish %>%
  mutate(Status = ifelse(Status == "Low_Number_Occurrences", "Low_Number_Occurrences_or_Non_Recent_Collection_Time", Status))

#Add in Annotations regarding any discrepancies between Reference Flora and POWO:
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(Taxa_Annotation = NA_character_)

#Find matches from Multiple_Match_Annotation and generate annotation text
Annotations <- Multiple_Match_Annotation %>%
  group_by(Accepted_Name_with_Author) %>%
  summarise(Reference_Names = paste(unique(Reference_Accepted_Name_with_Author), collapse = ", "), .groups = "drop")

#Join with Confirmed_Master_List_Tidy to update Taxa_Annotation
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  left_join(Annotations, by = "Accepted_Name_with_Author") %>%
  mutate(Taxa_Annotation = ifelse(!is.na(Reference_Names),
                                  paste0(reference_flora_name, " and POWO taxonomic discrepancy. This taxon is considered a synonym to ", Reference_Names, " in the ", reference_flora_name),
                                  Taxa_Annotation)) %>%
  select(-Reference_Names)  

Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  select(Accepted_Name_with_Author, Taxa_Annotation, everything())

#Change any data source if there is an annotation of the reference flora. 
Confirmed_Master_List_Tidy <- Confirmed_Master_List_Tidy %>%
  mutate(Data_Source = ifelse(grepl(reference_flora_name, Taxa_Annotation) & Data_Source == "GBIF", 
                              "GBIF_and_Reference_Flora", 
                              Data_Source))

#Write to Excel File:
#Get today's date (e.g., Feb28_2024)
today_date <- format(Sys.Date(), "%b%d_%Y")

#Create a folder name with the date
folder_name <- paste0("Final_Results_", today_date)

#Create the new folder
dir.create(folder_name)

#Concatenate the date with the rest of the file name
file_name <- paste0("Master_Taxa_List_GBIF_and_ReferenceFlora_", today_date, ".xlsx")

#Set the working directory to the new folder
setwd(folder_name)

#Write to Excel file
write_xlsx(Confirmed_Master_List_Tidy, file_name)

#Set the working directory back to the original directory
setwd("..")

{
#Determine if any Genera or Family level names should be removed if there are representatives of a 
#lower taxonomic resolution in the Master Taxa List. Remove them as another output, depending on the needs for the Taxa List. 

Confirmed_Master_List_Tidy_RemovedGenera<-Confirmed_Master_List_Tidy %>%
  filter(rank %in% c("Genus", "GENUS"))

#Check if the Genus occurs in Confirmed_Master_List_Tidy$genus
#Only keep Genera if there is a lower taxon rank already in the data of that same Genera in the Confirmed_Master_List_Tidy:
Confirmed_Master_List_Tidy_RemovedGenera <- Confirmed_Master_List_Tidy_RemovedGenera[
  Confirmed_Master_List_Tidy_RemovedGenera$genus %in% Confirmed_Master_List_Tidy$genus[!Confirmed_Master_List_Tidy$rank %in% c("Genus", "GENUS", "Family", "FAMILY")], 
]

Confirmed_Master_List_Tidy_RemovedFamily<-Confirmed_Master_List_Tidy %>%
  filter(rank %in% c("Family", "FAMILY"))

Confirmed_Master_List_Tidy_RemovedFamily <- Confirmed_Master_List_Tidy_RemovedFamily[
  Confirmed_Master_List_Tidy_RemovedFamily$Family %in% Confirmed_Master_List_Tidy$Family[!Confirmed_Master_List_Tidy$rank %in% c("Family", "FAMILY")], 
]
Confirmed_Master_List_Tidy_Removed<-rbind(Confirmed_Master_List_Tidy_RemovedGenera,Confirmed_Master_List_Tidy_RemovedFamily)


Confirmed_Master_List_Tidy_NoRepeatedHigherTaxon<-Confirmed_Master_List_Tidy%>%
  filter(!Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy_Removed$Accepted_Name_with_Author)

#Get today's date (e.g., Feb28_2024)
today_date <- format(Sys.Date(), "%b%d_%Y")

#Concatenate the date with the rest of the file name
file_name <- paste0("Master_Taxa_List_OnlyUniqueHigherTaxa_GBIF_and_ReferenceFlora_", today_date, ".xlsx")

#Set the working directory to the existing folder
setwd(paste0("Final_Results_", today_date))

#Write to Excel file
write_xlsx(Confirmed_Master_List_Tidy_NoRepeatedHigherTaxon, file_name)

#Set the working directory back to the original directory (if needed)
setwd("..")

#Document Results
Documentation_Results$Eighteen<-nrow(Confirmed_Master_List_Tidy)

#Summarize counts of Accepted_Name_with_Author by rank and Data_Source
rank_summary <- Confirmed_Master_List_Tidy %>%
  group_by(rank, Data_Source) %>%
  summarise(count = n(), .groups = 'drop')

rank_summary %>%
  group_by(rank, Data_Source) %>%
  summarise(
    total = sum(count),  #Total count for each rank and data source
    .groups = 'drop'
  ) %>%
  mutate(
    message = paste("There are", total, "of rank", rank, "from", Data_Source)
  ) %>%
  pull(message) %>%
  walk(print) 
}

{
#Document Results
Documentation_Results$Nineteen<-rank_summary

unique_genus_sum <- length(unique(na.omit(Confirmed_Master_List_Tidy$genus)))

#Document Results
Documentation_Results$Number_Genera_Final<-unique_genus_sum

unique_species_count <- Confirmed_Master_List_Tidy %>%
  filter(!is.na(genus) & !is.na(species) & genus != 0 & species != 0) %>%
  distinct(genus, species) %>%
  nrow()

#Document Results
Documentation_Results$Number_Species_Final<-unique_species_count

save.image(file = "Taxa_List_Save_29.RData") #Save everything 
#load("Taxa_List_Save_29.RData")
}

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Final Curation Step - Manually Checking Species Names Unique to the List Produced

#The POWO_query_species_final can be used to determine any species in the list produced which are unique to this list. 
#An option is also provided to upload in external lists and compound this with the POWO_query_species_final. 

cat("The External List should have Accepted Names according to POWO, or if the name is not found in POWO, then that Name as provided in the External List. It should contain one column called 'Accepted_Name_with_Author'\n")

cat("The External Lists, if more than one, should be appended into one file, called 'External_List_Compare.xlsx'\n")

response <- readline(prompt = "Do you have an External List to compare? (Yes/No): ")

# Check the response
if(tolower(response) == "yes") {
  # Read the Excel file if the answer is Yes
  library(readxl)
  External_List_Compare <- read_excel("External_List_Compare.xlsx")
  External_List_Compare$Accepted_Name_with_Author <- str_trim(External_List_Compare$Accepted_Name_with_Author)
  External_List_Compare<-External_List_Compare%>%distinct()
  POWO_query_species_final_Comp<-POWO_query_species_final%>%select(Accepted_Name_with_Author)
  POWO_query_species_final_Comp$Accepted_Name_with_Author<-str_trim(POWO_query_species_final_Comp$Accepted_Name_with_Author)
  POWO_query_species_final_Comp<-POWO_query_species_final_Comp%>%distinct()
  POWO_query_species_final_Comp<-rbind(POWO_query_species_final_Comp,External_List_Compare)
  POWO_query_species_final_Comp<-POWO_query_species_final_Comp%>%distinct()
  print("External List has been loaded.")
} else {
  print("No External List to load.")
  POWO_query_species_final_Comp<-POWO_query_species_final%>%select(Accepted_Name_with_Author)
  POWO_query_species_final_Comp$Accepted_Name_with_Author<-str_trim(POWO_query_species_final_Comp$Accepted_Name_with_Author)
  }

#Ensure that no Accepted Name in POWO_query_species_final_Comp has become a synonym using our workflow and through integrating in the reference flora 

for (i in 1:nrow(POWO_query_species_final_Comp)) {
  match_index <- which(long_format_data$name == POWO_query_species_final_Comp$Accepted_Name_with_Author[i])
  if (length(match_index) > 0) {
    new_value <- long_format_data$Accepted_Name_with_Author[match_index]
    POWO_query_species_final_Comp$Accepted_Name_with_Author[i] <- new_value
    cat("Changed:", POWO_query_species_final_Comp$Accepted_Name_with_Author[i], "to", new_value, "\n")}}

POWO_query_species_final_Comp$Accepted_Name_with_Author<-str_trim(POWO_query_species_final_Comp$Accepted_Name_with_Author)
POWO_query_species_final_Comp<-POWO_query_species_final_Comp%>%distinct()


#Now determine taxa found exclusive in the Taxa List and none of the external sources - skip this part if not using external sources
Confirmed_Master_List_Tidy_ExclusiveTaxaList <- Confirmed_Master_List_Tidy %>%
  filter(!Accepted_Name_with_Author %in% External_ALL$Accepted_Name_with_Author) %>%
  filter(!(rank %in% c("Genus", "Family", "Aggregate")))

Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow<-Confirmed_Master_List_Tidy_ExclusiveTaxaList %>%
  select(Accepted_Name_with_Author) #Contains ALL taxa names at species level or below, but this is not really reflecting the reality
#as there are many infraspecifics which are known in Greenland at species level. 
  
#Now need to clean this more, as we have infraspecifics which may be found on other lists at species level,
#but we need to check. Species and hybrids we know that they don't appear then. 
#For example, autonyms.

#First, determine rank 
Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  mutate(rank = NA_character_) %>% 
  mutate(rank = case_when(
    str_detect(rank, "^[A-Z]+$") ~ NA_character_,  #Replace all-uppercase taxonRanks with NA
    TRUE ~ rank
  )) %>%
  mutate(rank = case_when(
    str_detect(Accepted_Name_with_Author, "× ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, "^X ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, " [×xX] ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, " subsp\\. ") ~ "Subspecies",
    str_detect(Accepted_Name_with_Author, " var\\. ") ~ "Variety",
    str_detect(Accepted_Name_with_Author, " subvar\\. ") ~ "Subvariety",
    str_detect(Accepted_Name_with_Author, " morph") ~ "Morph",
    str_detect(Accepted_Name_with_Author, " f\\. ") ~ "Form",
    str_detect(Accepted_Name_with_Author, " subf\\. ") ~ "Subform",
    str_detect(Accepted_Name_with_Author, " agg\\. ") ~ "Aggregate",
    str_detect(Accepted_Name_with_Author, " aggregate") ~ "Aggregate",
    str_detect(Accepted_Name_with_Author, " sect\\. ") ~ "Section",
    str_detect(Accepted_Name_with_Author, " subsect\\. ") ~ "Subsection",
    str_detect(Accepted_Name_with_Author, " ser\\. ") ~ "Series",
    str_detect(Accepted_Name_with_Author, " subser\\. ") ~ "Subseries",
    str_detect(Accepted_Name_with_Author, " tr\\. ") ~ "Tribe",
    str_detect(Accepted_Name_with_Author, " subtrib\\. ") ~ "Subtribe",
    str_detect(Accepted_Name_with_Author, " gen\\. ") ~ "Genus",
    str_detect(Accepted_Name_with_Author, " subg\\. ") ~ "Subgenus",
    str_detect(Accepted_Name_with_Author, " fam\\. ") ~ "Family",
    str_detect(Accepted_Name_with_Author, "^[A-Z][a-zà-öø-ÿ]+eae\\b") ~ "Family", #If the first word ends with eae
    str_detect(Accepted_Name_with_Author, " subfam\\. ") ~ "Subfamily",
    str_detect(Accepted_Name_with_Author, " ord\\. ") ~ "Order",
    str_detect(Accepted_Name_with_Author, " subord\\. ") ~ "Suborder",
    str_detect(Accepted_Name_with_Author, " cl\\. ") ~ "Class",
    str_detect(Accepted_Name_with_Author, " subcl\\. ") ~ "Subclass",
    TRUE ~ rank  #Keep remaining ranks unchanged
  )) %>%
  #Assign rank for cases where it is still NA
  mutate(rank = case_when(
    is.na(rank) & str_count(Accepted_Name_with_Author, "\\s") >= 1 & 
      str_detect(Accepted_Name_with_Author, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species",  
    is.na(rank) & str_count(Accepted_Name_with_Author, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ rank  
  ))

Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  mutate(
    Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author),
    species = case_when(
      rank == "Hybrid" ~ str_extract(Accepted_Name_with_Author, "^[^×]*×\\s*\\S+"),
      str_detect(Accepted_Name_with_Author, "^×") ~ word(Accepted_Name_with_Author, 1, 2),
      TRUE ~ word(Accepted_Name_with_Author, 1, 2)))

#First handle cases of races:
Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  group_by(species) %>%
  mutate(rank = if_else(grepl(" race", Accepted_Name_with_Author) & 
        n() > 1 & 
        any(rank == "Species", na.rm = TRUE),
      "Race", #if there is a race (could only be from the reference flora), and if the species also occurs, change rank to Race 
      rank)) %>%
  ungroup()

Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  mutate(Unique_Check_Species_Level = NA) %>%
  group_by(species) %>%
  mutate(
    Unique_Check_Species_Level = case_when(
      rank == "Species" ~ TRUE,  #Set TRUE if rank is Species
      species %in% species[rank == "Species"] ~ TRUE,  #If the species name matches a species with rank "Species"
      TRUE ~ Unique_Check_Species_Level)) %>%
  ungroup()

#Remove hybrid genera
Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  filter(!grepl("^×\\s+\\w+$", species))

#Now will need to compare to POWO_query_species_final_Comp, but taking into account Rank information
POWO_query_species_final_Comp <- POWO_query_species_final_Comp %>%
  mutate(rank = NA_character_) %>% 
  mutate(rank = case_when(
    str_detect(rank, "^[A-Z]+$") ~ NA_character_,  #Replace all-uppercase taxonRanks with NA
    TRUE ~ rank
  )) %>%
  mutate(rank = case_when(
    str_detect(Accepted_Name_with_Author, "× ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, "^X ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, " [×xX] ") ~ "Hybrid",
    str_detect(Accepted_Name_with_Author, " subsp\\. ") ~ "Subspecies",
    str_detect(Accepted_Name_with_Author, " var\\. ") ~ "Variety",
    str_detect(Accepted_Name_with_Author, " subvar\\. ") ~ "Subvariety",
    str_detect(Accepted_Name_with_Author, " morph") ~ "Morph",
    str_detect(Accepted_Name_with_Author, " f\\. ") ~ "Form",
    str_detect(Accepted_Name_with_Author, " subf\\. ") ~ "Subform",
    str_detect(Accepted_Name_with_Author, " agg\\. ") ~ "Aggregate",
    str_detect(Accepted_Name_with_Author, " aggregate") ~ "Aggregate",
    str_detect(Accepted_Name_with_Author, " sect\\. ") ~ "Section",
    str_detect(Accepted_Name_with_Author, " subsect\\. ") ~ "Subsection",
    str_detect(Accepted_Name_with_Author, " ser\\. ") ~ "Series",
    str_detect(Accepted_Name_with_Author, " subser\\. ") ~ "Subseries",
    str_detect(Accepted_Name_with_Author, " tr\\. ") ~ "Tribe",
    str_detect(Accepted_Name_with_Author, " subtrib\\. ") ~ "Subtribe",
    str_detect(Accepted_Name_with_Author, " gen\\. ") ~ "Genus",
    str_detect(Accepted_Name_with_Author, " subg\\. ") ~ "Subgenus",
    str_detect(Accepted_Name_with_Author, " fam\\. ") ~ "Family",
    str_detect(Accepted_Name_with_Author, "^[A-Z][a-zà-öø-ÿ]+eae\\b") ~ "Family", #If the first word ends with eae
    str_detect(Accepted_Name_with_Author, " subfam\\. ") ~ "Subfamily",
    str_detect(Accepted_Name_with_Author, " ord\\. ") ~ "Order",
    str_detect(Accepted_Name_with_Author, " subord\\. ") ~ "Suborder",
    str_detect(Accepted_Name_with_Author, " cl\\. ") ~ "Class",
    str_detect(Accepted_Name_with_Author, " subcl\\. ") ~ "Subclass",
    TRUE ~ rank  #Keep remaining ranks unchanged
  )) %>%
  #Assign rank for cases where it is still NA
  mutate(rank = case_when(
    is.na(rank) & str_count(Accepted_Name_with_Author, "\\s") >= 1 & 
      str_detect(Accepted_Name_with_Author, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species",  
    is.na(rank) & str_count(Accepted_Name_with_Author, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ rank  
  ))

POWO_query_species_final_Comp$rank[is.na(POWO_query_species_final_Comp$rank)] <- "Genus_Or_Higher"

POWO_query_species_final_Comp <- POWO_query_species_final_Comp %>%
  mutate(
    Accepted_Name_with_Author = str_trim(Accepted_Name_with_Author),
    species = case_when(
      rank == "Hybrid" ~ str_extract(Accepted_Name_with_Author, "^[^×]*×\\s*\\S+"),
      str_detect(Accepted_Name_with_Author, "^×") ~ word(Accepted_Name_with_Author, 1, 2),
      TRUE ~ word(Accepted_Name_with_Author, 1, 2)))

#Now find cases where the name is still found externally at the Species Level, but just not at our specific taxon rank level:
Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  mutate(Found_Externally_at_Species_Level = NA) %>%
  #If Unique_Check_Species_Level is TRUE, set Found_Externally_at_Species_Level to FALSE
  mutate(Found_Externally_at_Species_Level = if_else(
    !is.na(Unique_Check_Species_Level) & Unique_Check_Species_Level == TRUE,
    FALSE,
    Found_Externally_at_Species_Level
  )) %>%
  #Check if species is in POWO_query_species_final_Comp$species and rank is "Species"
  mutate(Found_Externally_at_Species_Level = if_else(
    is.na(Found_Externally_at_Species_Level) & species %in% POWO_query_species_final_Comp$species[POWO_query_species_final_Comp$rank == "Species"],
    TRUE,
    Found_Externally_at_Species_Level
  )) %>%
  #If Found_Externally_at_Species_Level is TRUE, set Unique_Check_Species_Level to FALSE
  mutate(Unique_Check_Species_Level = if_else(
    Found_Externally_at_Species_Level == TRUE, 
    FALSE, 
    Unique_Check_Species_Level
  ))

#Now Found_Externally_at_Species_Level is TRUE when the name is still found at species level, but not below species level.
#and Unique_Check_Species_Level is TRUE when the species itself is not found externally. 

#Now for Parent_Species_Found_Externally_below_Species_Level
Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  mutate(Parent_Species_Found_Externally_below_Species_Level = FALSE) %>%  #Start with FALSE for all
  mutate(Parent_Species_Found_Externally_below_Species_Level = if_else(
    species %in% POWO_query_species_final_Comp$species[!(POWO_query_species_final_Comp$rank %in% c("Hybrid", "Species"))],
    TRUE, 
    Parent_Species_Found_Externally_below_Species_Level
  ))

#Hybrid Match Check
Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  mutate(Hybrid_Match = FALSE) %>%  #Start with FALSE for all rows
  mutate(Hybrid_Match = if_else(
    rank == "Hybrid" & species %in% POWO_query_species_final_Comp$species,
    TRUE, 
    Hybrid_Match
  ))

#Now classify if the names are truly unique or not 
Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  mutate(Truly_Unmatched = case_when(
    #Case 1: If Unique_Check_Species_Level is FALSE and the other three columns are FALSE
    Unique_Check_Species_Level == FALSE &
      !Found_Externally_at_Species_Level &
      !Parent_Species_Found_Externally_below_Species_Level &
      !Hybrid_Match ~ TRUE,
    
    #Case 2: If Unique_Check_Species_Level and Found_Externally_at_Species_Level are NA, and the other two columns are FALSE
    is.na(Unique_Check_Species_Level) &
      is.na(Found_Externally_at_Species_Level) &
      !Parent_Species_Found_Externally_below_Species_Level &
      !Hybrid_Match ~ TRUE,
    
    #Case 3: If Unique_Check_Species_Level is TRUE and the other three columns are FALSE
    Unique_Check_Species_Level == TRUE &
      !Found_Externally_at_Species_Level &
      !Parent_Species_Found_Externally_below_Species_Level &
      !Hybrid_Match ~ TRUE,
    
    #All other cases, set Truly_Unmatched to FALSE
    TRUE ~ FALSE
  ))

Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  select(Accepted_Name_with_Author, Truly_Unmatched, rank, species, everything())

#Now add back in appropriate author information.
Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  #Step 1: Replace species with Accepted_Name_with_Author when rank is "Species"
  mutate(species = case_when(
    rank == "Species" ~ Accepted_Name_with_Author,  #Replace species with Accepted_Name_with_Author if rank is Species
    TRUE ~ species
  )) %>%
  #Step 2: For non-species ranks, check if species matches any species in rank "Species", then replace
  mutate(species = case_when(
    rank != "Species" & species %in% 
      filter(Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow, rank == "Species")$species ~
      filter(Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow, rank == "Species")$Accepted_Name_with_Author[
        match(species, filter(Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow, rank == "Species")$species)
      ],
    TRUE ~ species  
  ))

Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  mutate(species = case_when(rank == "Hybrid" | rank == "Race" ~ Accepted_Name_with_Author,
    TRUE ~ species))

Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow$Confirmed<-NA
Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  select(Accepted_Name_with_Author, Confirmed,Truly_Unmatched, rank, species, everything())

#We can automatically confirm cases from our Reference Flora:
Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  mutate(Confirmed = ifelse(Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy$Accepted_Name_with_Author &
        Confirmed_Master_List_Tidy$Data_Source[match(Accepted_Name_with_Author, Confirmed_Master_List_Tidy$Accepted_Name_with_Author)] %in% 
        c("GBIF_and_Reference_Flora", "Reference_Flora"),"YES_Reference_Flora",Confirmed))

#Now use Confirmed_Master_List_Tidy to find author information
Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow %>%
  mutate(species = case_when(
    #Case where species has two words and is in Species_Level_Name with rank "Species"
    str_count(species, "\\w+") == 2 & species %in% Confirmed_Master_List_Tidy$Species_Level_Name & 
      Confirmed_Master_List_Tidy$rank[match(species, Confirmed_Master_List_Tidy$Species_Level_Name)] == "Species" ~
      Confirmed_Master_List_Tidy$Accepted_Name_with_Author[match(species, Confirmed_Master_List_Tidy$Species_Level_Name)],
    #Otherwise keep the original species name
    TRUE ~ species
  ))

Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow_Truly_Unmatched<-Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow%>%
  filter(Truly_Unmatched==TRUE)

#Now an Excel will created to check these cases:
Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow_Truly_Unmatched<-Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow_Truly_Unmatched%>%
  select(Accepted_Name_with_Author, Confirmed, rank, species) %>%
  mutate(Comment=NA)

#But also give opportunity to check the entire taxon list too. 
Entire_List_Manual_Check<-Confirmed_Master_List_Tidy%>%
  select(Accepted_Name_with_Author, Taxa_Annotation, rank, Species_Level_Name) %>%
  mutate(Confirmed=NA, Comment=NA) %>%
  rename(species = Species_Level_Name)

Entire_List_Manual_Check <- Entire_List_Manual_Check %>%
  mutate(Confirmed = if_else(
    is.na(Confirmed),
    Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow_Truly_Unmatched$Confirmed[
      match(Accepted_Name_with_Author, Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow_Truly_Unmatched$Accepted_Name_with_Author)],Confirmed))

#Mark the unique species for extra checking 
names_to_update <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow_Truly_Unmatched %>%
  filter(is.na(Confirmed) | Confirmed == "") %>%
  pull(Accepted_Name_with_Author)

#Update Confirmed in Entire_List_Manual_Check
Entire_List_Manual_Check <- Entire_List_Manual_Check %>%
  mutate(Confirmed = if_else(Accepted_Name_with_Author %in% names_to_update,
    "Extra_Check_Needed_New_Species",
    Confirmed))

#Entire_List_Manual_Check now contains all the species for someone to look over, but especially marks down the ones which 
#need to be checked further (the ones unique to this dataset)

cat("Please open Entire_List_Manual_Check and do the following:\n\n",
    "In the column Confirmed, type REMOVE if the taxa name should be removed upon checking it, or YES/leave blank if it should remain.\n\n",
    "Species unique to this dataset and the reference flora are marked, as they especially need to be checked. Ones validated by the Reference Flora are already pre-confirmed.\n\n",
    "In the Comment column, note down any important notes leading to the decision.")

write_xlsx(Entire_List_Manual_Check, "Entire_List_Manual_Check.xlsx")

save.image(file = "Taxa_List_Save_30.RData") #Save everything 
#load("Taxa_List_Save_30.RData")





Entire_List_Manual_Check <- read_excel("Entire_List_Manual_Check.xlsx")


{
#Remove REMOVE species from the Confirmed_Master_List_Tidy.
Removed_Curation_Publish<-Entire_List_Manual_Check%>%
    filter(str_to_upper(Confirmed) == "REMOVE") %>%
  rename(Verified_Name = Accepted_Name_with_Author) %>%
  mutate(original_input_name=NA,
         status="Manually_Removed") %>%
  select(-species,-Confirmed, -Taxa_Annotation)

Removed_Curation_Publish$Comment <- paste(
  "Manually removed during final check of taxon list.",
  Removed_Curation_Publish$Comment,sep = " ")

Removed_Curation_Publish$Verified_Name

new_rows <- Combi_List_Storage %>%
  filter(Accepted_Name_with_Author %in% Removed_Curation_Publish$Verified_Name) %>%
  select(Verified_Name = Accepted_Name_with_Author, original_input_name = scientificName) %>%
  distinct()

#Append unique new rows to Removed_Curation_Publish
Removed_Curation_Publish <- bind_rows(Removed_Curation_Publish, new_rows)

Removed_Curation_Publish <- Removed_Curation_Publish %>%
  group_by(Verified_Name) %>%
  mutate(
    Comment = ifelse(is.na(Comment), first(na.omit(Comment)), Comment),
    rank = ifelse(is.na(rank), first(na.omit(rank)), rank),
    status = ifelse(is.na(status), first(na.omit(status)), status)) %>%
  ungroup() %>% drop_na(original_input_name) #Now we have the scientificName for all these newly removed ones

#Now remove these names from Confirmed_Master_List_Tidy
Confirmed_Master_List_Tidy_ManualRemove<-Confirmed_Master_List_Tidy%>%
  filter(Accepted_Name_with_Author %in% Removed_Curation_Publish$Verified_Name)

Confirmed_Master_List_Tidy<-Confirmed_Master_List_Tidy%>%
  filter(!Accepted_Name_with_Author %in% Removed_Curation_Publish$Verified_Name)

Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow_Truly_Unmatched <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow_Truly_Unmatched %>%
  semi_join(
    Entire_List_Manual_Check %>%
      filter(str_to_upper(Confirmed) != "REMOVE"),
    by = "Accepted_Name_with_Author"
  )

unique_species <- Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow_Truly_Unmatched %>%
  mutate(
    first_two_words = ifelse(
      str_detect(species, " × "),
      str_extract(species, "\\S+ × \\S+"), # Extract "word × word"
      word(species, 1, 2) # Default to first two words
    )
  ) %>%
  pull(first_two_words) %>%
  unique()
}
#Count the number of unique species
unique_species_count <- length(unique_species)

cat("There are", unique_species_count, "species unique to this Taxa list\n\n")
cat("They are:\n", paste(unique_species, collapse = "\n"), "\n")
{
New_Species_of_Region<-Confirmed_Master_List_Tidy_ExclusiveTaxaList_SpeciesAndBelow_Truly_Unmatched%>%
  select(Accepted_Name_with_Author,Confirmed,rank)
  
Confirmed_Master_List_Tidy_New_Species_of_Region<-Confirmed_Master_List_Tidy%>%
  filter(Accepted_Name_with_Author %in% New_Species_of_Region$Accepted_Name_with_Author)

#When possible, extract parent species information:
Filtered_Classification <- Confirmed_Master_List_Tidy_New_Species_of_Region %>%
  filter(
    rank != "Species" & rank != "Hybrid",                   
    Data_Source != "Reference_Flora",                        
    taxonomicStatus == "Accepted") %>%
  unnest(classification, names_sep = "_classification") %>%  
  select(Accepted_Name_with_Author, starts_with("classification")) %>%
  filter(classification_classificationrank == "SPECIES") %>%
  mutate(Accepted_Name_with_Author_Species = paste(classification_classificationname, classification_classificationauthor) %>%
           gsub(" NA", "", .) %>%
           str_trim())

New_Species_of_Region <- New_Species_of_Region %>%
  left_join(Filtered_Classification %>%
      select(Accepted_Name_with_Author, Accepted_Name_with_Author_Species), 
    by = "Accepted_Name_with_Author") %>%
  mutate(Accepted_Name_with_Author = coalesce(Accepted_Name_with_Author_Species, Accepted_Name_with_Author),
    rank = if_else(!is.na(Accepted_Name_with_Author_Species), "Species", rank)) %>%
  select(-Accepted_Name_with_Author_Species)

New_Species_of_Region<-New_Species_of_Region%>%distinct()%>%
  filter(!rank=="Race")

New_Species_of_Region <- New_Species_of_Region %>%
  mutate(First_Two_Words = word(Accepted_Name_with_Author, 1, 2)) %>%
  group_by(First_Two_Words) %>%
  mutate(Confirmed = if_else(
      rank == "Species" & 
        any(rank != "Species" & rank != "Hybrid" & Confirmed == "YES_Reference_Flora"),
      "YES_Reference_Flora",
      Confirmed)) %>%
  ungroup() %>%
  select(-First_Two_Words)

New_Species_of_Region <- New_Species_of_Region %>%
  mutate(First_Two_Words = word(Accepted_Name_with_Author, 1, 2)) %>%
  group_by(First_Two_Words) %>%
  mutate(Contains_Species = any(rank == "Species"),
    Species_Row_Confirmed = if_else(rank == "Species", Confirmed, NA_character_)) %>%
  ungroup() %>%
  filter(!(rank != "Species" & rank != "Hybrid" & Contains_Species)) %>%
  select(-First_Two_Words, -Contains_Species, -Species_Row_Confirmed)
}

write_xlsx(New_Species_of_Region, "New_Species.xlsx")

save.image(file = "Taxa_List_Save_31.RData") #Save everything 
#load("Taxa_List_Save_31.RData")

#Redefine Confirmed_Master_List_Tidy_Names_Storage
{
Confirmed_Master_List_Tidy_Names<-Confirmed_Master_List_Tidy %>%
  select(Accepted_Name_with_Author,starts_with("Additional_Name"))

#Pivot all columns except "Accepted_Name_with_Author" into long format
Confirmed_Master_List_Tidy_Names <- Confirmed_Master_List_Tidy_Names %>%
  pivot_longer(
    cols = -Accepted_Name_with_Author, #Exclude the column "Accepted_Name_with_Author"
    names_to = "Additional_Name_Number",               #New column for the names of the original columns
    values_to = "Additional_Name"                  #New column for the values from those columns
  ) %>%
  select(Accepted_Name_with_Author, Additional_Name) %>%
  filter(!is.na(Additional_Name)) %>%
  distinct(Accepted_Name_with_Author, Additional_Name)

missing_additional_name <- Confirmed_Master_List_Tidy %>%
  filter(!Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy_Names$Accepted_Name_with_Author) %>%
  select(Accepted_Name_with_Author)

new_rows <- missing_additional_name %>%
  mutate(Additional_Name = Accepted_Name_with_Author)

Confirmed_Master_List_Tidy_Names<-rbind(Confirmed_Master_List_Tidy_Names,new_rows) 

Confirmed_Master_List_Tidy_Names<-Confirmed_Master_List_Tidy_Names%>%distinct()

#Add back in Accepted_Name_with_Author for downstream use:
new_rows <- Confirmed_Master_List_Tidy_Names %>%
  select(-Additional_Name) %>% 
  mutate(Additional_Name = Accepted_Name_with_Author) %>% distinct()

#Bind the new rows to Confirmed_Master_List_Tidy_Names
Confirmed_Master_List_Tidy_Names <- bind_rows(Confirmed_Master_List_Tidy_Names, new_rows)

Confirmed_Master_List_Tidy_Names <- Confirmed_Master_List_Tidy_Names %>%
  mutate(across(everything(), str_trim))

Confirmed_Master_List_Tidy_Names<-Confirmed_Master_List_Tidy_Names%>%distinct()

Confirmed_Master_List_Tidy_Names_Storage<-Confirmed_Master_List_Tidy_Names
long_format_data<-Confirmed_Master_List_Tidy_Names
}
#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Final Step - Leftovers
{
#Now process the Leftovers
  if ("gbifID" %in% colnames(Specimen_Counts_Leftovers)) {
    Specimen_Counts_Leftovers <- Specimen_Counts_Leftovers %>%
      select(-c(gbifID))
  }

#Remove Final Leftover Names which do not belong
Final_Leftovers<-Final_Leftovers%>%
  filter(!original_input_name %in% Confirmed_Master_List_Tidy_Names_Storage$Additional_Name)

#Final_Leftovers is now in long format. Change to wide:
Final_Leftovers <- Final_Leftovers %>%
  group_by(Verified_Unrecognized_Name) %>% 
  mutate(original_input_name_id = paste0("original_input_name", row_number())) %>% #Create unique column identifiers
  pivot_wider(names_from = original_input_name_id, values_from = original_input_name) %>% 
  ungroup()

Final_Leftovers<-left_join(Specimen_Counts_Leftovers, Final_Leftovers, by = "Verified_Unrecognized_Name" ) 

Final_Leftovers<-Final_Leftovers%>%
  mutate( #if NA change to false - for names from Reference Flora not corresponding to specimens 
    Flagged_Name = ifelse(is.na(Flagged_Name), FALSE, Flagged_Name),
    Flagged_DataSource_Count = ifelse(is.na(Flagged_DataSource_Count), FALSE, Flagged_DataSource_Count),
    Flagged_Specimen_Count = ifelse(is.na(Flagged_Specimen_Count), FALSE, Flagged_Specimen_Count)) 

#Finally, process these leftover names to determine their ranks, and if any need to be removed.

Final_Leftovers_Publish<-Final_Leftovers

#Final editing of Rank Data:
Final_Leftovers_Publish <- Final_Leftovers_Publish %>%
  mutate(rank = NA_character_) %>% 
  mutate(rank = case_when(
    str_detect(rank, "^[A-Z]+$") ~ NA_character_,  #Replace all-uppercase taxonRanks with NA
    TRUE ~ rank
  )) %>%
  mutate(rank = case_when(
    str_detect(Verified_Unrecognized_Name, "× ") ~ "Hybrid",
    str_detect(Verified_Unrecognized_Name, "^X ") ~ "Hybrid",
    str_detect(Verified_Unrecognized_Name, " [×xX] ") ~ "Hybrid",
    str_detect(Verified_Unrecognized_Name, " subsp\\. ") ~ "Subspecies",
    str_detect(Verified_Unrecognized_Name, " var\\. ") ~ "Variety",
    str_detect(Verified_Unrecognized_Name, " subvar\\. ") ~ "Subvariety",
    str_detect(Verified_Unrecognized_Name, " morph") ~ "Morph",
    str_detect(Verified_Unrecognized_Name, " f\\. ") ~ "Form",
    str_detect(Verified_Unrecognized_Name, " subf\\. ") ~ "Subform",
    str_detect(Verified_Unrecognized_Name, " agg\\. ") ~ "Aggregate",
    str_detect(Verified_Unrecognized_Name, " aggregate") ~ "Aggregate",
    str_detect(Verified_Unrecognized_Name, " sect\\. ") ~ "Section",
    str_detect(Verified_Unrecognized_Name, " subsect\\. ") ~ "Subsection",
    str_detect(Verified_Unrecognized_Name, " ser\\. ") ~ "Series",
    str_detect(Verified_Unrecognized_Name, " subser\\. ") ~ "Subseries",
    str_detect(Verified_Unrecognized_Name, " tr\\. ") ~ "Tribe",
    str_detect(Verified_Unrecognized_Name, " subtrib\\. ") ~ "Subtribe",
    str_detect(Verified_Unrecognized_Name, " gen\\. ") ~ "Genus",
    str_detect(Verified_Unrecognized_Name, " subg\\. ") ~ "Subgenus",
    str_detect(Verified_Unrecognized_Name, " fam\\. ") ~ "Family",
    str_detect(Verified_Unrecognized_Name, "^[A-Z][a-zà-öø-ÿ]+eae\\b") ~ "Family", #If the first word ends with eae
    str_detect(Verified_Unrecognized_Name, " subfam\\. ") ~ "Subfamily",
    str_detect(Verified_Unrecognized_Name, " ord\\. ") ~ "Order",
    str_detect(Verified_Unrecognized_Name, " subord\\. ") ~ "Suborder",
    str_detect(Verified_Unrecognized_Name, " cl\\. ") ~ "Class",
    str_detect(Verified_Unrecognized_Name, " subcl\\. ") ~ "Subclass",
    TRUE ~ rank  #Keep remaining ranks unchanged
  )) %>%
  #Assign rank for cases where it is still NA
  mutate(rank = case_when(
    is.na(rank) & str_count(Verified_Unrecognized_Name, "\\s") >= 1 & 
      str_detect(Verified_Unrecognized_Name, "^[A-Z][a-zà-öø-ÿ]+\\s[a-zà-öø-ÿ]+") ~ "Species",  
    is.na(rank) & str_count(Verified_Unrecognized_Name, "\\s") == 0 ~ "Genus_Or_Higher",
    TRUE ~ rank  
  ))

Final_Leftovers_Publish <- Final_Leftovers_Publish %>%
  mutate(Status = ifelse(grepl("Unknown|Covered|\\sad\\s|\\?|\\sad\\.\\s*|\\*|unknown:covered|phyta|[0-9]| indet|/|^[a-z].*\\.$| aff\\.| cfr\\.| cf\\.| and |( x .* x )|var\\. NA |f\\. NA |subsp\\. NA | ad$", 
                               Verified_Unrecognized_Name, ignore.case = TRUE),
                         "REMOVE", "Unrecognized_Name"))
}

Final_Leftovers_Publish <- Final_Leftovers_Publish %>%
  rename(Verified_Name = Verified_Unrecognized_Name)

#Write to Excel File:
#Get today's date (e.g., Feb28_2024)
today_date<- format(Sys.Date(), "%b%d_%Y")
#Concatenate the date with the rest of the file name
file_name <- paste0("Unrecognized_Names_Post_ExtValidation", today_date, ".xlsx")

#Now, set the working directory to the folder created in the start of the script
setwd(folder_name)

#Write to Excel file
write_xlsx(Final_Leftovers_Publish, file_name)

#Set the working directory back to the original directory
setwd("..")

#Unrecognized_Names_Post_ExtValidation.xlsx is now the final list of names which have not been verified by POWO or the external source. 

#Document Results
Documentation_Results$Seventeen<-nrow(Final_Leftovers_Publish)

save.image(file = "Taxa_List_Save_32.RData") #Save everything 
#load("Taxa_List_Save_32.RData")
{
#For specific case where Remaining_Flags contains a newly unrecognized name:
Final_Leftovers_Publish_Add <- Combi_List_Storage_Accepted %>%
  filter(Accepted_Name_with_Author %in% Remaining_Flags$original_input_name & 
      Remaining_Flags$Status[match(Accepted_Name_with_Author, Remaining_Flags$original_input_name)] == "Unrecognized_Name") %>%
  select(Accepted_Name_with_Author,scientificName) %>%
  filter(Accepted_Name_with_Author != scientificName) %>%
  mutate(Verified_Name = Remaining_Flags$Verified_Name[match(Accepted_Name_with_Author, Remaining_Flags$original_input_name)],
    Status = Remaining_Flags$Status[match(Accepted_Name_with_Author, Remaining_Flags$original_input_name)],
    rank = Remaining_Flags$rank[match(Accepted_Name_with_Author, Remaining_Flags$original_input_name)]) %>%
  select(-Accepted_Name_with_Author) %>%
  rename(original_input_name1 = scientificName)

if (nrow(Final_Leftovers_Publish_Add) > 0) {  
suppressPackageStartupMessages(library(plyr))
Final_Leftovers_Publish<-rbind.fill(Final_Leftovers_Publish,Final_Leftovers_Publish_Add)
detach(package:plyr)
library("dplyr")
}
}

#For other specific case where UnMatched_Flagged_Names contains a name which was removed but not ending up in Final_Removed_Names
#Nor any other list:
Removed_Flags_Publish_Add <- UnMatched_Flagged_Names %>%
  filter(!Accepted_Name_with_Author %in% Remaining_Flags$Verified_Name &
           !Accepted_Name_with_Author %in% Removed_Flags_Publish$Verified_Name &
           !Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy_Names_Storage$Additional_Name) %>%
  select(Accepted_Name_with_Author, original_Accepted_Name_with_Author) %>%
  inner_join(Combi_List_Storage_Accepted,
    by = c("original_Accepted_Name_with_Author" = "Accepted_Name_with_Author")) %>%
  transmute(Accepted_Name_with_Author, scientificName, rank = taxonRank) %>%
  rename(Verified_Name = Accepted_Name_with_Author, original_input_name = scientificName) %>%
  mutate(Status ="Low_Number_Occurrences",
         Comment = NA) 

if (nrow(Removed_Flags_Publish_Add) > 0) {
Removed_Flags_Publish<-rbind(Removed_Flags_Publish,Removed_Flags_Publish_Add)
}

#Combine ALL 7 Removed Names into one list
#Leftover_autonyms_Publish
#Ref_Names_Multiple_POWO_Publish - No longer including this as annotations have been added 
#Unauthored_Names_Publish
#Removed_Flags_Publish
#Final_Leftovers_Publish
#Removed_Curation_Publish
#Unplaced_Removed_Publish
#Taxonomic_Autonyms_Publish

{
Leftover_autonyms_Publish$Comment<-"Autonym name which has multiple Species options based on Authority"
Ref_Names_Multiple_POWO_Publish$Comment<-"Names from the Reference Flora which match to multiple POWO Accepted Names in our dataset"
Unauthored_Names_Publish$Comment<-"Names which do not come with author information, and which match to multiple authors and therefore cannot be confidently matched to one Accepted Name"
Final_Leftovers_Publish$Comment<-"Names which were not recognized using the POWO taxonomic backbone, nor that of the Reference Flora" 
Removed_Flags_Publish <- Removed_Flags_Publish %>%
  mutate(Comment = paste("Names which were flagged due to a low number of occurrences and/or low number of data sources and/or for having only Human Observation occurrences. ", Comment))
Removed_Curation_Publish<-Removed_Curation_Publish%>%rename(Status=status)
#Unplaced_Removed_Publish is ready

Final_Leftovers_Publish<-Final_Leftovers_Publish%>%
  select(Verified_Name, Status, rank, Comment, starts_with("original")) %>%
  rename(original_input_name = original_input_name1) #May have multiple original_input_names depending on the data, so will r.bind fill 

Final_Leftovers_Publish <- Final_Leftovers_Publish %>%
  pivot_longer(
    cols = starts_with("original_input_name"), 
    names_to = "original_input_type",          
    values_to = "original_input_name") %>%
  select(Verified_Name, original_input_name, Status, rank, Comment) %>% #Keep desired columns
  drop_na(original_input_name)

Taxonomic_Autonyms_Publish$Comment<-"Autonym name which has a parent name that is a synonym, and the Accepted Name of that parent has multiple accepted infraspecifics of the same rank"

}

Final_Removed_Names <- bind_rows(
  list(Leftover_autonyms_Publish, Unauthored_Names_Publish, 
       Final_Leftovers_Publish, Removed_Flags_Publish, Removed_Curation_Publish, 
       Unplaced_Removed_Publish, Taxonomic_Autonyms_Publish) %>% #Ref_Names_Multiple_POWO_Publish removed 
    keep(~ nrow(.) > 0)) #Keep only non-empty dataframes

#Final_Removed_Names<-rbind(Leftover_autonyms_Publish,Ref_Names_Multiple_POWO_Publish,Unauthored_Names_Publish,Final_Leftovers_Publish,Removed_Flags_Publish,Removed_Curation_Publish,Unplaced_Removed_Publish,Taxonomic_Autonyms_Publish)

Final_Removed_Names <- Final_Removed_Names %>%
  mutate(Status = ifelse(Status == "REMOVE", "Invalid_Name_Integrity", Status))

#Fix cases of more than one original input name per verified name
Final_Removed_Names <- Final_Removed_Names %>%
  rowwise() %>%
  mutate(split_needed = str_detect(original_input_name, ",") && 
      !(original_input_name %in% All_Names$scientificName),
    split_names = if (split_needed) str_split(original_input_name, ",\\s*") else list(original_input_name)) %>%
  ungroup() %>%
  unnest(split_names) %>%
  filter(split_names %in% All_Names$scientificName | !split_needed) %>%
  mutate(original_input_name = split_names) %>%
  select(-split_names, -split_needed) %>%
  distinct()

#Store the original input names in Final_Removed_Names which have been removed
Final_Removed_Original_Input_Names<-Final_Removed_Names%>%
  filter(!Status == "Reference_List_Multiple_POWO_Matches") %>%
  select(original_input_name)

#Get today's date (e.g., Feb28_2024)
today_date <- format(Sys.Date(), "%b%d_%Y")

#Concatenate the date with the rest of the file name
file_name <- paste0("Final_List_All_Removed_Names_", today_date, ".xlsx")

#Set the working directory to the existing folder
setwd(paste0("Final_Results_", today_date))

#Write to Excel file
write_xlsx(Final_Removed_Names, file_name)

#Set the working directory back to the original directory (if needed)
setwd("..")

save.image(file = "Taxa_List_Save_33.RData") #Save everything 
#load("Taxa_List_Save_33.RData")

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Final Step - Re-Counting GBIF Data according to Final List and Final Removed Names

#Now using Combi_List_Storage_1
{
#For any GBIF records which use this name, there will therefore be multiple hits when trying to match. 
#To deal with this, first identify duplicate synonym names:
duplicates <- Confirmed_Master_List_Tidy %>%
  select(Accepted_Name_with_Author, starts_with("Additional")) %>%
  pivot_longer(
    cols = starts_with("Additional"),     #Only pivot the Additional columns
    names_to = "source",                  #Column for the original column names
    values_to = "name") %>%
  filter(!is.na(name)) %>%                 #Remove NA values
  group_by(name) %>%                       #Group by Additional names
  filter(n() > 1) %>%                      #Filter for duplicates
  select(Accepted_Name_with_Author, name) %>% #Keep only relevant columns
  distinct()  

Duplicate_POWO_Synonyms<-duplicates%>%
  rename(Additional_Name = name) %>% group_by(Additional_Name) %>% filter(n() > 1) %>% ungroup()

#Now here need to relabel with both names when there is a duplicate 
#Make a combined version of the Accepted_Name_with_Author for each duplicate Additional_Name, 
#so that when the GBIF data is append in, it is clear that there are multiple options for these unique cases
Combined_Names <- Confirmed_Master_List_Tidy_Names_Storage %>%
  filter(Additional_Name %in% Duplicate_POWO_Synonyms$Additional_Name) %>%  #Filter for duplicates
  group_by(Additional_Name) %>%  #Group by the Additional_Name
  mutate(Combined_Accepted_Names = paste(unique(Accepted_Name_with_Author), collapse = " OR ")) %>% #Combine unique Accepted_Name_with_Author
  distinct(Additional_Name, Combined_Accepted_Names)  #Combine unique Accepted_Name_with_Author and separate by OR

#Update Confirmed_Master_List_Tidy_Names_Storage with the combined Accepted_Name_with_Author values, and remove duplicate cases
Confirmed_Master_List_Tidy_Names_Storage <- Confirmed_Master_List_Tidy_Names_Storage %>%
  left_join(Combined_Names, by = "Additional_Name") %>%  #Join the combined names back to the original dataframe
  mutate(Accepted_Name_with_Author = ifelse(!is.na(Combined_Accepted_Names), Combined_Accepted_Names, Accepted_Name_with_Author)) %>%  #Replace Accepted_Name_with_Author with the combined version when needed
  select(-Combined_Accepted_Names) %>%  #Remove the temporary column
  distinct()

Confirmed_Master_List_Tidy_Names_Storage <- Confirmed_Master_List_Tidy_Names_Storage %>%
  bind_rows(Confirmed_Master_List_Tidy_Names_Storage %>%
      distinct(Accepted_Name_with_Author) %>%
      transmute(Accepted_Name_with_Author,Additional_Name = Accepted_Name_with_Author)) %>%
  distinct() 

#Note that some names do not have synonyms, like Carex rufina Drejer. They need to be added back into Confirmed_Master_List_Tidy_Names_Storage
Confirmed_Master_List_Tidy_Names_Storage <- Confirmed_Master_List_Tidy_Names_Storage %>%
  bind_rows(
    Confirmed_Master_List_Tidy %>%
      filter(!Accepted_Name_with_Author %in% Confirmed_Master_List_Tidy_Names_Storage$Accepted_Name_with_Author) %>%
      mutate(Additional_Name = Accepted_Name_with_Author) %>%  #Create same value in Additional_Name
      select(Accepted_Name_with_Author, Additional_Name))

Confirmed_Master_List_Tidy_Names_Storage <- Confirmed_Master_List_Tidy_Names_Storage %>%
  mutate(across(everything(), ~ str_trim(.)))

#Use original GBIF data from Combi_List_Storage_1
#Confirmed_Master_List_Tidy_Names_Storage contains the long pivoted data from the final Confirmed Master List
#Match to the Combi_List_Storage_1, but normalize names, so that matches like Carex ×helvola Blytt to Carex × helvola Blytt
#will still work:

Combi_List_Storage_1 <- Combi_List_Storage_1 %>%
  mutate(Accepted_Name_with_Author_Match = Confirmed_Master_List_Tidy_Names_Storage$Accepted_Name_with_Author[match(
    normalize_names(scientificName), normalize_names(Confirmed_Master_List_Tidy_Names_Storage$Additional_Name))]) %>%
  relocate(c(Accepted_Name_with_Author_Match, scientificName), .before = 1) %>%  #Reorder columns
  rename(Accepted_Name_with_Author = Accepted_Name_with_Author_Match)

#Tidy 
Combi_List_Storage_1$Accepted_Name_with_Author <- gsub("  OR ", " OR ", Combi_List_Storage_1$Accepted_Name_with_Author)  

#Now use Combi_Cleaner_2 to edit some data which was corrected earlier 
Combi_List_Storage_1 <- Combi_List_Storage_1 %>%
  mutate(Accepted_Name_with_Author = ifelse(scientificName %in% Combi_Cleaner_2$Accepted_Name_with_Author,
                                            Combi_Cleaner_2$Match_Name[match(scientificName, Combi_Cleaner_2$Accepted_Name_with_Author)],
                                            Accepted_Name_with_Author))


#rbind in Combi_List_TaxonIssue_NO_reAdd2, which contains additional specimens (but not any new names)
Combi_List_Storage_1<-rbind(Combi_List_Storage_1,Combi_List_TaxonIssue_NO_reAdd2)
Combi_List_Storage_1$Accepted_Name_with_Author <- trimws(Combi_List_Storage_1$Accepted_Name_with_Author)
}
{
Combi_List_TaxonIssue_NO$Accepted_Name_with_Author<-NA
Combi_List_Storage_1<-rbind(Combi_List_Storage_1,Combi_List_TaxonIssue_NO)

Combi_List_Storage_1 <- Combi_List_Storage_1 %>%
  mutate(scientificName = ifelse(is.na(scientificName), verbatimScientificName, scientificName))

#Handling final cases - Cases where the scientificName is empty but it shouldnt be
Combi_List_Storage_1 <- Combi_List_Storage_1 %>%
  mutate(scientificName = if_else(scientificName == "",
      Combi_List_All_Media_Downloads$scientificName[match(gbifID, Combi_List_All_Media_Downloads$gbifID)],
      scientificName),
    Accepted_Name_with_Author = if_else(scientificName == Combi_List_All_Media_Downloads$scientificName[match(gbifID, Combi_List_All_Media_Downloads$gbifID)] & 
        (is.na(Accepted_Name_with_Author) | Accepted_Name_with_Author == ""),
      scientificName,
      Accepted_Name_with_Author))

#Filter Initial Accepted Names
Combi_List_Accepted_Final<-Combi_List_Storage_1%>%
  filter(normalize_names(Accepted_Name_with_Author) %in% normalize_names(Confirmed_Master_List_Tidy_Names_Storage$Additional_Name))

#Remove Filtered Initial Accepted Names
Combi_List_Storage_1<-Combi_List_Storage_1%>%
  filter(!gbifID %in% Combi_List_Accepted_Final$gbifID)

#Filter initial removed names
Combi_List_Removed_Final<-Combi_List_Storage_1%>%
  filter(normalize_names(scientificName) %in% normalize_names(Final_Removed_Original_Input_Names$original_input_name))

#Remove Filtered Initial Removed Names
Combi_List_Storage_1<-Combi_List_Storage_1%>%
  filter(!gbifID %in% Combi_List_Removed_Final$gbifID)

#Remaining cases where changed during flagging. Repair now:
Combi_List_Storage_1 <- Combi_List_Storage_1 %>%
  left_join(Combi_List_Storage_Accepted %>%
      select(gbifID, Accepted_Name_with_Author, scientificName),
    by = "gbifID",
    suffix = c("", ".new")) %>%
  mutate(Accepted_Name_with_Author = ifelse(!is.na(Accepted_Name_with_Author.new), Accepted_Name_with_Author.new, Accepted_Name_with_Author),
    scientificName = ifelse(!is.na(scientificName.new), scientificName.new, scientificName)) %>%
  select(-Accepted_Name_with_Author.new, -scientificName.new)

Combi_List_Storage_1 <- Combi_List_Storage_1 %>%
  left_join(
    Flagged_Check %>%
      select(Accepted_Name_with_Author, Change_To),
    by = "Accepted_Name_with_Author") %>%
  mutate(Accepted_Name_with_Author = ifelse(!is.na(Change_To), Change_To, Accepted_Name_with_Author)) %>%
  select(-Change_To)
}
{
#Compare again against the Final Confirmed Master List
Combi_List_Accepted_Final1<-Combi_List_Storage_1%>%
  filter(normalize_names(Accepted_Name_with_Author) %in% normalize_names(Confirmed_Master_List_Tidy_Names_Storage$Additional_Name))

if (nrow(Combi_List_Accepted_Final1) > 0) {
Combi_List_Storage_1<-Combi_List_Storage_1%>%
  filter(!gbifID %in% Combi_List_Accepted_Final1$gbifID)
}

Combi_List_Removed_Final1 <- Combi_List_Storage_1 %>%
    filter(normalize_names(scientificName) %in% normalize_names(Final_Removed_Original_Input_Names$original_input_name) |
        normalize_names(Accepted_Name_with_Author) %in% normalize_names(Final_Removed_Original_Input_Names$original_input_name))

if (nrow(Combi_List_Removed_Final1) > 0) {
Combi_List_Storage_1<-Combi_List_Storage_1%>%
  filter(!gbifID %in% Combi_List_Removed_Final1$gbifID)
}

if (nrow(Combi_List_Accepted_Final1) > 0) {
Combi_List_Accepted_Final<-rbind(Combi_List_Accepted_Final,Combi_List_Accepted_Final1)
}

if (nrow(Combi_List_Removed_Final1) > 0) {
Combi_List_Removed_Final<-rbind(Combi_List_Removed_Final,Combi_List_Removed_Final1)
}
}
#More cleaning of names which were flagged and altered
#Handle names which were changed to a new name and that name was not accepted by POWO and also removed
#For example, names found in df_total_flag3 which are not autonyms. 
#An example is "Cystopteris fragilis subsp. diaphana (Bory) Litard." which during the processing of Flagging,
#was changed to "Cystopteris diaphana x fragilis" but it is known in Final_Removed_Names as Cystopteris diaphana (Bory) Blasdell
Combi_List_Storage_1 <- Combi_List_Storage_1 %>%
  mutate(Accepted_Name_with_Author = if_else(
      scientificName %in% Combi_List_Storage_Accepted$scientificName, 
      if_else(Combi_List_Storage_Accepted$Accepted_Name_with_Author[
          match(scientificName, Combi_List_Storage_Accepted$scientificName)
        ] %in% Final_Removed_Original_Input_Names$original_input_name,
        Combi_List_Storage_Accepted$Accepted_Name_with_Author[
          match(scientificName, Combi_List_Storage_Accepted$scientificName)], 
        Accepted_Name_with_Author), 
      Accepted_Name_with_Author))

#Handle any cases of "or" whereby there are more than one option for an Accepted Name according to POWO (rare cases, an example is "Cerastium holosteoides Fr. OR Cerastium glomeratum subsp. glomeratum" because POWO has overlapping synonyms for them)
#These cases go into the Combi_List_Removed_Final
rows_to_move <- Combi_List_Storage_1 %>%
  filter(str_detect(Accepted_Name_with_Author, " OR ")) 

#Append to Combi_List_Removed_Final
Combi_List_Removed_Final <- bind_rows(Combi_List_Removed_Final, rows_to_move)

#Remove those rows from Combi_List_Storage_1
Combi_List_Storage_1 <- Combi_List_Storage_1 %>%
  filter(!str_detect(Accepted_Name_with_Author, " OR "))

if (nrow(Combi_List_Storage_1) == 0) {
  cat("Congratulations! All original input names have been accounted for!\n")
} else {
  cat("There appears to be a problem. Not all original input names have been accounted for. Please check Combi_List_Storage_1 to see what remains - these are the names not matched to either the Confirmed Taxon List, nor the Removed Names List.\n")
}

save.image(file = "Taxa_List_Save_34.RData") #Save everything 
#load("Taxa_List_Save_34.RData")

#Now ALL the occurrences are accounted for in either Combi_List_Accepted_Final or Combi_List_Removed_Final
{
#Combi_List_Accepted_Final already has the Accepted_Name_with_Author, but we need to add the Verified_Name to Combi_List_Removed_Final
Combi_List_Accepted_Final<-Combi_List_Accepted_Final%>%
  rename(Accepted_Name_with_Author_Store = Accepted_Name_with_Author) %>%
  left_join(Confirmed_Master_List_Tidy_Names_Storage, 
            by = c("Accepted_Name_with_Author_Store" = "Additional_Name")) %>%
  select(-Accepted_Name_with_Author_Store) %>%
  select(Accepted_Name_with_Author, scientificName,verbatimScientificName, verbatimScientificNameAuthorship, gbifID,institutionCode, everything())

#Processing the Removed Names
Final_Removed_Names_Tidy<-Final_Removed_Names %>%
  filter(Status != "Reference_List_Multiple_POWO_Matches") %>%
  select(Verified_Name, original_input_name) %>% distinct()

Combi_List_Removed_Final <- Combi_List_Removed_Final %>%
  select(-Accepted_Name_with_Author) %>%
  mutate(scientificName_merge = scientificName) %>%
  left_join(Final_Removed_Names_Tidy, by = c("scientificName_merge" = "original_input_name")) %>%
  select(Verified_Name, scientificName, everything())

#Handle some hybrid cases that occur 
Combi_List_Removed_Final <- Combi_List_Removed_Final %>%
  mutate(Verified_Name = if_else(is.na(Verified_Name),
    if_else(gsub(" ", "", scientificName) %in% gsub(" ", "", Final_Removed_Names_Tidy$original_input_name),
      Final_Removed_Names_Tidy$Verified_Name[match(gsub(" ", "", scientificName), gsub(" ", "", Final_Removed_Names_Tidy$original_input_name))],
      Verified_Name),Verified_Name))

#Handle very specific cases of an unauthored name become an authored one only to have that name later removed during flagging
Combi_List_Removed_Final <- Combi_List_Removed_Final %>%
  mutate(Verified_Name = if_else(is.na(Verified_Name), 
    if_else(gbifID %in% Combi_List_TaxonIssue_NO_reAdd2$gbifID & 
        Combi_List_TaxonIssue_NO_reAdd2$Accepted_Name_with_Author[match(gbifID, Combi_List_TaxonIssue_NO_reAdd2$gbifID)] %in% Final_Removed_Names_Tidy$original_input_name,
      Combi_List_TaxonIssue_NO_reAdd2$Accepted_Name_with_Author[match(gbifID, Combi_List_TaxonIssue_NO_reAdd2$gbifID)],
      Verified_Name),Verified_Name))

#Handle cases of leftover " OR " names
Combi_List_Removed_Final<- Combi_List_Removed_Final %>%
  mutate(Verified_Name = if_else(is.na(Verified_Name),
      rows_to_move$Accepted_Name_with_Author[match(scientificName, rows_to_move$scientificName)],Verified_Name))

}

if (all(!is.na(Combi_List_Removed_Final$Verified_Name) & Combi_List_Removed_Final$Verified_Name != "")) {
  cat("All Occurrences associated to Removed Names are accounted for! Yay!\n")
} else {
  cat("There are some occurrences associated to Removed Names which are not accounted for. Please check the Combi_List_Removed_Final to see what is going on\n")
}

if (all(!is.na(Combi_List_Accepted_Final$Accepted_Name_with_Author) & Combi_List_Accepted_Final$Accepted_Name_with_Author != "")) {
  cat("All Occurrences associated to Accepted Names are accounted for! Yay!\n")
} else {
  cat("There are some occurrences associated to Accepted Names which are not accounted for. Please check the Combi_List_Accepted_Final to see what is going on\n")
}

save.image(file = "Taxa_List_Save_35.RData") #Save everything 
#load("Taxa_List_Save_35.RData")

#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#~~~~~~#
#Final Step - Produce Final Occurrence Counts for both Accepted Names and Removed Names

#First to for Combi_List_Accepted_Final

#Now count the number of occurrences
{
  Specimen_Counts_Final<-NA
  Specimen_Counts<-NA
  Time<-NA
  Number_of_Samples_With_Unknown_Year<-NA
  Specimen_Counts_1<-NA
  Specimen_Counts_2<-NA
  
#Check how many specimens we have of each species
Specimen_Counts<- Combi_List_Accepted_Final %>% 
    count(Accepted_Name_with_Author)            #Count number of specimens for each scientific name 
  colnames(Specimen_Counts)[2] <- "Number_of_Specimens"
  
#Check how specimens are distributed through time
Time<- Combi_List_Accepted_Final %>%          #N.B. This will likely be less than the number of scientificNames (and therefore the Specimen_Counts) as for some scientificNames there are no years recorded (eg for Achillea millefolium f. lanulosa)
    group_by(Accepted_Name_with_Author) %>%
    slice(c(which.min(year), which.max(year))) %>%
    mutate(Oldest_Specimen_with_Date = min(year),
           Youngest_Specimen_with_Date = max(year)) %>%
    select(Accepted_Name_with_Author,Oldest_Specimen_with_Date,Youngest_Specimen_with_Date) %>%
    distinct()
  {
#Sort by sample age 
Before_1800 <- Combi_List_Accepted_Final %>%
      group_by(Accepted_Name_with_Author) %>%
      tally(year < 1800) %>%
      transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
                Number_of_Occurrences_Before_1800 = n)
    
Between_1800_and_1825 <- Combi_List_Accepted_Final %>%
      group_by(Accepted_Name_with_Author) %>%
      tally(year >= 1800 & year <= 1825) %>%
      transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
                Number_of_Occurrences_1800_1825 = n)
    
Between_1826_and_1850 <- Combi_List_Accepted_Final %>%
      group_by(Accepted_Name_with_Author) %>%
      tally(year >= 1826 & year <= 1850) %>%
      transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
                Number_of_Occurrences_1826_1850 = n)
    
Between_1851_and_1875 <- Combi_List_Accepted_Final %>%
      group_by(Accepted_Name_with_Author) %>%
      tally(year >= 1851 & year <= 1875) %>%
      transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
                Number_of_Occurrences_1851_1875 = n)
    
Between_1876_and_1900 <- Combi_List_Accepted_Final %>%
      group_by(Accepted_Name_with_Author) %>%
      tally(year >= 1876 & year <= 1900) %>%
      transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
                Number_of_Occurrences_1876_1900 = n)
    
Between_1901_and_1925 <- Combi_List_Accepted_Final %>%
      group_by(Accepted_Name_with_Author) %>%
      tally(year >= 1901 & year <= 1925) %>%
      transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
                Number_of_Occurrences_1901_1925 = n)
    
Between_1926_and_1950 <- Combi_List_Accepted_Final %>%
      group_by(Accepted_Name_with_Author) %>%
      tally(year >= 1926 & year <= 1950) %>%
      transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
                Number_of_Occurrences_1926_1950 = n)
    
Between_1951_and_1975 <- Combi_List_Accepted_Final %>%
      group_by(Accepted_Name_with_Author) %>%
      tally(year >= 1951 & year <= 1975) %>%
      transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
                Number_of_Occurrences_1951_1975 = n)
    
Between_1976_and_2000 <- Combi_List_Accepted_Final %>%
      group_by(Accepted_Name_with_Author) %>%
      tally(year >= 1976 & year <= 2000) %>%
      transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
                Number_of_Occurrences_1976_2000 = n)
    
Between_2001_and_2025 <- Combi_List_Accepted_Final %>%
      group_by(Accepted_Name_with_Author) %>%
      tally(year >= 2001 & year <= 2025) %>%
      transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
                Number_of_Occurrences_2001_2025 = n)
    
Number_of_Samples_With_Unknown_Year<- Combi_List_Accepted_Final %>% 
      group_by(Accepted_Name_with_Author) %>%
      tally(is.na(year)) %>%
      transmute(Accepted_Name_with_Author = Accepted_Name_with_Author,
                Number_of_Samples_With_Unknown_Year = n)
    
Specimen_Counts<-left_join(Specimen_Counts,Time, by="Accepted_Name_with_Author")
Specimen_Counts <- left_join(Specimen_Counts, Before_1800, by = "Accepted_Name_with_Author")
Specimen_Counts <- left_join(Specimen_Counts, Between_1800_and_1825, by = "Accepted_Name_with_Author")
Specimen_Counts <- left_join(Specimen_Counts, Between_1826_and_1850, by = "Accepted_Name_with_Author")
Specimen_Counts <- left_join(Specimen_Counts, Between_1851_and_1875, by = "Accepted_Name_with_Author")
Specimen_Counts <- left_join(Specimen_Counts, Between_1876_and_1900, by = "Accepted_Name_with_Author")
Specimen_Counts <- left_join(Specimen_Counts, Between_1901_and_1925, by = "Accepted_Name_with_Author")
Specimen_Counts <- left_join(Specimen_Counts, Between_1926_and_1950, by = "Accepted_Name_with_Author")
Specimen_Counts <- left_join(Specimen_Counts, Between_1951_and_1975, by = "Accepted_Name_with_Author")
Specimen_Counts <- left_join(Specimen_Counts, Between_1976_and_2000, by = "Accepted_Name_with_Author")
Specimen_Counts <- left_join(Specimen_Counts, Between_2001_and_2025, by = "Accepted_Name_with_Author")
Specimen_Counts<-left_join(Specimen_Counts,Number_of_Samples_With_Unknown_Year, by="Accepted_Name_with_Author")
}
  
Specimen_Counts_1<-Specimen_Counts %>%
    filter(!is.na(Oldest_Specimen_with_Date) & !is.na(Youngest_Specimen_with_Date))
  
#Change values from 0 to NA when there are no transcribed dates whatsoever anyways. 
Specimen_Counts_2<-Specimen_Counts %>%
    filter(is.na(Oldest_Specimen_with_Date) & is.na(Youngest_Specimen_with_Date)) %>%
    mutate(across(.cols = contains("Number"), .fns = ~ na_if(., 0)))

Specimen_Counts_Accepted_Final<-rbind(Specimen_Counts_1,Specimen_Counts_2)
}

Specimen_Counts_Accepted_Final <- Specimen_Counts_Accepted_Final %>%
  left_join(Confirmed_Master_List_Tidy %>%
              select(Accepted_Name_with_Author, Data_Source), 
            by = "Accepted_Name_with_Author")

#Specimen_Counts_Accepted_Final now contains final occurrence counts for the data
#associated to the Accepted Names in our Taxa List. 

#Now onto the Removed Names
{
Specimen_Counts<-NA
Time<-NA
Number_of_Samples_With_Unknown_Year<-NA
Specimen_Counts_1<-NA
Specimen_Counts_2<-NA
  
  #Check how many specimens we have of each species
Specimen_Counts<- Combi_List_Removed_Final %>% 
    count(Verified_Name)            #Count number of specimens for each scientific name 
colnames(Specimen_Counts)[2] <- "Number_of_Specimens"
  
  #Check how specimens are distributed through time
Time<- Combi_List_Removed_Final %>%          #N.B. This will likely be less than the number of scientificNames (and therefore the Specimen_Counts) as for some scientificNames there are no years recorded (eg for Achillea millefolium f. lanulosa)
    group_by(Verified_Name) %>%
    slice(c(which.min(year), which.max(year))) %>%
    mutate(Oldest_Specimen_with_Date = min(year),
           Youngest_Specimen_with_Date = max(year)) %>%
    select(Verified_Name,Oldest_Specimen_with_Date,Youngest_Specimen_with_Date) %>%
    distinct()
  {
    #Sort by sample age 
Before_1800 <- Combi_List_Removed_Final %>%
      group_by(Verified_Name) %>%
      tally(year < 1800) %>%
      transmute(Verified_Name = Verified_Name,
                Number_of_Occurrences_Before_1800 = n)
    
Between_1800_and_1825 <- Combi_List_Removed_Final %>%
      group_by(Verified_Name) %>%
      tally(year >= 1800 & year <= 1825) %>%
      transmute(Verified_Name = Verified_Name,
                Number_of_Occurrences_1800_1825 = n)
    
Between_1826_and_1850 <- Combi_List_Removed_Final %>%
      group_by(Verified_Name) %>%
      tally(year >= 1826 & year <= 1850) %>%
      transmute(Verified_Name = Verified_Name,
                Number_of_Occurrences_1826_1850 = n)
    
Between_1851_and_1875 <- Combi_List_Removed_Final %>%
      group_by(Verified_Name) %>%
      tally(year >= 1851 & year <= 1875) %>%
      transmute(Verified_Name = Verified_Name,
                Number_of_Occurrences_1851_1875 = n)
    
Between_1876_and_1900 <- Combi_List_Removed_Final %>%
      group_by(Verified_Name) %>%
      tally(year >= 1876 & year <= 1900) %>%
      transmute(Verified_Name = Verified_Name,
                Number_of_Occurrences_1876_1900 = n)
    
Between_1901_and_1925 <- Combi_List_Removed_Final %>%
      group_by(Verified_Name) %>%
      tally(year >= 1901 & year <= 1925) %>%
      transmute(Verified_Name = Verified_Name,
                Number_of_Occurrences_1901_1925 = n)
    
Between_1926_and_1950 <- Combi_List_Removed_Final %>%
      group_by(Verified_Name) %>%
      tally(year >= 1926 & year <= 1950) %>%
      transmute(Verified_Name = Verified_Name,
                Number_of_Occurrences_1926_1950 = n)
    
Between_1951_and_1975 <- Combi_List_Removed_Final %>%
      group_by(Verified_Name) %>%
      tally(year >= 1951 & year <= 1975) %>%
      transmute(Verified_Name = Verified_Name,
                Number_of_Occurrences_1951_1975 = n)
    
Between_1976_and_2000 <- Combi_List_Removed_Final %>%
      group_by(Verified_Name) %>%
      tally(year >= 1976 & year <= 2000) %>%
      transmute(Verified_Name = Verified_Name,
                Number_of_Occurrences_1976_2000 = n)
    
Between_2001_and_2025 <- Combi_List_Removed_Final %>%
      group_by(Verified_Name) %>%
      tally(year >= 2001 & year <= 2025) %>%
      transmute(Verified_Name = Verified_Name,
                Number_of_Occurrences_2001_2025 = n)
    
Number_of_Samples_With_Unknown_Year<- Combi_List_Removed_Final %>% 
      group_by(Verified_Name) %>%
      tally(is.na(year)) %>%
      transmute(Verified_Name = Verified_Name,
                Number_of_Samples_With_Unknown_Year = n)
    
Specimen_Counts<-left_join(Specimen_Counts,Time, by="Verified_Name")
Specimen_Counts <- left_join(Specimen_Counts, Before_1800, by = "Verified_Name")
Specimen_Counts <- left_join(Specimen_Counts, Between_1800_and_1825, by = "Verified_Name")
Specimen_Counts <- left_join(Specimen_Counts, Between_1826_and_1850, by = "Verified_Name")
Specimen_Counts <- left_join(Specimen_Counts, Between_1851_and_1875, by = "Verified_Name")
Specimen_Counts <- left_join(Specimen_Counts, Between_1876_and_1900, by = "Verified_Name")
Specimen_Counts <- left_join(Specimen_Counts, Between_1901_and_1925, by = "Verified_Name")
Specimen_Counts <- left_join(Specimen_Counts, Between_1926_and_1950, by = "Verified_Name")
Specimen_Counts <- left_join(Specimen_Counts, Between_1951_and_1975, by = "Verified_Name")
Specimen_Counts <- left_join(Specimen_Counts, Between_1976_and_2000, by = "Verified_Name")
Specimen_Counts <- left_join(Specimen_Counts, Between_2001_and_2025, by = "Verified_Name")
Specimen_Counts<-left_join(Specimen_Counts,Number_of_Samples_With_Unknown_Year, by="Verified_Name")
    }
  
Specimen_Counts_1<-Specimen_Counts %>%
    filter(!is.na(Oldest_Specimen_with_Date) & !is.na(Youngest_Specimen_with_Date))
  
  #Change values from 0 to NA when there are no transcribed dates whatsoever anyways. 
Specimen_Counts_2<-Specimen_Counts %>%
    filter(is.na(Oldest_Specimen_with_Date) & is.na(Youngest_Specimen_with_Date)) %>%
    mutate(across(.cols = contains("Number"), .fns = ~ na_if(., 0)))
  
Specimen_Counts_Removed_Final<-rbind(Specimen_Counts_1,Specimen_Counts_2)
  }

#For Combi_List_Accepted_Final, the GBIF Accepted occurrences 
#Save all the different files, first from our Taxa List:
#Get today's date (e.g., Feb28_2024)
today_date <- format(Sys.Date(), "%b%d_%Y")

#Concatenate the date with the rest of the file name
file_name <- paste0("GBIF_Records_Final_Taxa_List_", today_date, ".xlsx")

#Set the working directory to the existing folder
setwd(paste0("Final_Results_", today_date))

#Write to Excel file
write_xlsx(Combi_List_Accepted_Final, file_name)

#Set the working directory back to the original directory (if needed)
setwd("..")

#For Combi_List_Removed_Final, the GBIF occurrences pertaining to names removed from our list 
#Save all the different files, first from our Taxa List:
#Get today's date (e.g., Feb28_2024)
today_date <- format(Sys.Date(), "%b%d_%Y")

#Concatenate the date with the rest of the file name
file_name <- paste0("GBIF_Records_Removed_From_Taxa_List_", today_date, ".xlsx")

#Set the working directory to the existing folder
setwd(paste0("Final_Results_", today_date))

#Write to Excel file
write_xlsx(Combi_List_Removed_Final, file_name)

#Set the working directory back to the original directory (if needed)
setwd("..")

#For Specimen_Counts_Accepted_Final, the GBIF occurrences counts for the final taxa list  
#Get today's date (e.g., Feb28_2024)
today_date <- format(Sys.Date(), "%b%d_%Y")

#Concatenate the date with the rest of the file name
file_name <- paste0("GBIF_Specimen_Counts_Taxa_List_", today_date, ".xlsx")

#Set the working directory to the existing folder
setwd(paste0("Final_Results_", today_date))

#Write to Excel file
write_xlsx(Specimen_Counts_Accepted_Final, file_name)

#Set the working directory back to the original directory (if needed)
setwd("..")

#For Specimen_Counts_Removed_Final, the GBIF occurrences counts for the removed names
#Get today's date (e.g., Feb28_2024)
today_date <- format(Sys.Date(), "%b%d_%Y")

#Concatenate the date with the rest of the file name
file_name <- paste0("GBIF_Specimen_Counts_Removed_From_Taxa_List_", today_date, ".xlsx")

#Set the working directory to the existing folder
setwd(paste0("Final_Results_", today_date))

#Write to Excel file
write_xlsx(Specimen_Counts_Removed_Final, file_name)

#Set the working directory back to the original directory (if needed)
setwd("..")

#Make one file with all Taxa List Data in it (Taxa List, Removed Names, and Occurrence Counts)
#Set the working directory to the existing folder
setwd(paste0("Final_Results_", today_date))
#Creating combined Excel File
#Create a new workbook
#Set warnings to be ignored temporarily
options(warn = -1)
#Create a new workbook
wb <- createWorkbook()
#Add each dataframe to a separate sheet
addWorksheet(wb, "Taxa_List")
writeData(wb, "Taxa_List", Confirmed_Master_List_Tidy, na.string = "")
addWorksheet(wb, "Taxa_List_Occurrence_Counts")
writeData(wb, "Taxa_List_Occurrence_Counts", Specimen_Counts_Accepted_Final, na.string = "")
addWorksheet(wb, "Final_Removed_Names_List")
writeData(wb, "Final_Removed_Names_List", Final_Removed_Names, na.string = "")
addWorksheet(wb, "Final_Removed_Names_Occ_Counts")
writeData(wb, "Final_Removed_Names_Occ_Counts", Specimen_Counts_Removed_Final, na.string = "")
#Reset warnings back to default
options(warn = 0)
#Save the workbook
saveWorkbook(wb, "All_Taxa_List_Data.xlsx", overwrite = TRUE)
#Set the working directory back to the original directory (if needed)
setwd("..")

#Make one file with all GBIF Occurrence Data in it
#Set the working directory to the existing folder
setwd(paste0("Final_Results_", today_date))
#Create a new workbook
wb1 <- createWorkbook()
#Add each dataframe to a separate sheet
addWorksheet(wb1, "GBIF_Occurrences_Taxa_List")
writeData(wb1, "GBIF_Occurrences_Taxa_List", Combi_List_Accepted_Final, na.string = "")
addWorksheet(wb1, "GBIF_Occurrences_Removed_Names")
writeData(wb1, "GBIF_Occurrences_Removed_Names", Combi_List_Removed_Final, na.string = "")
#Reset warnings back to default
options(warn = 0)
#Save the workbook
saveWorkbook(wb1, "All_GBIF_Data.xlsx", overwrite = TRUE)
#Set the working directory back to the original directory (if needed)
setwd("..")

save.image(file = "Taxa_List_Save_36.RData") #Save everything 
#load("Taxa_List_Save_36.RData")

###########################################################
################### END of Process!!! #####################
###########################################################

