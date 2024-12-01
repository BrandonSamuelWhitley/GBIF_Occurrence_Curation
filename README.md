# GBIF_Occurrence_Curation
Script used for the project "Harmonising digitised herbaria data to enhance biodiversity knowledge: creating an updated checklist for the flora of Greenland."

#Code Author - Brandon Samuel Whitley
#Author Title - University of Copenhagen PhD Fellow in Arctic Plant Diversity and Pollination Networks
#Author Contact Information - brandon.s.whitley@snm.ku.dk and X - @BrandonWhit1ey
#Date of Creation - January 2023 - Last Edited December 01 2024
#Project Affiliated - "Greenland plant diversity patterns and pollination networks in a changing Arctic"

#Code Description -     This script has been designed to download metacollections of biodiversity plant
                        nomenclature data from GBIF, usually for a given geographic scope (up to 3 countries).
                        It then runs the names ascribed to the specimens through some standardization scripts, 
                        and then checks the names against the KEW Plants of the world (POWO) database. For names which
                        are recognized, it notes their Accepted Name. For names which are synonyms, it re-assigns
                        the name to the accepted name. During the process, it stores all known synonym names, and 
                        all names the taxa are listed as in the GBIF database, to allow for the accessing data 
                        across many data sources. For cases where the name is not recognized, it is put through a 
                        manual taxonomic expert review process to correct it, or to further leave it as unrecognized. 
                        Then, an independent reference flora for the region is uploaded into the data, and the names
                        are cross-checked against the reference flora. Any unrecognized names which are now recognized
                        using the reference flora are now re-added, along with any reference flora names which were not
                        already in the list. Remaining unrecognized names are left for manual review. After, names
                        are flagged according to criteria of minimum specimen count and minimum data source count, while
                        also automatically flagging occurrences derived exclusively from human observation. Flagged names
                        undergo review by experts in the flora to determine if they need to be removed or kept. 
                        Thereafter, the Taxon List is compared to an external list (default using the POWO database but this can
                        also be further supplemented). By doing this, species unique to this dataset are filtered, as they contain
                        the most potential for error. These species are then manually checked and verified or removed as needed. 
                        This process produces a curated metacollection of GBIF occurrence data, harmonized by Accepted Name. In doing
                        this, it also results in a curated Taxon List for the region at hand. 
                        Additionally, it also records edits made to data, and produces an output of any removed names and why, along with 
                        the GBIF occurrences affiliated to those names. 
                        This script was written as a part of a PhD project. This script has been used for Greenland as a case study, and thus 
                        contains many examples for Greenland in the code annotation. Note though that these examples can be found in other occurrence
                        data too, and so they were kept in as examples for the user to understand. 
                        This code is affiliated to the paper "Harmonising digitised herbaria data to enhance biodiversity knowledge: creating an 
                        updated checklist for the flora of Greenland."

#Input Information Required - 
 (1) GBIF Login Information for the given user
 (2) An external Reference Flora, where 
#Column 1 labelled as Accepted_Name_with_Author, containing the Accepted Name and the author of the Species, in ICN format
#Column 2 labelled as Accepted_Name, containing just the Accepted Name of the species
#Additional n columns labelled as Synonym_n for n synonyms, containing the Synonym Name with author
#Additional columns may contain other relevent information, such as the taxa rank, distribution, etc.
#If it contains a taxa rank column, please call it rank" )
#Optional (3) - External Floras to compare data to - Single column Excel sheet with Accepted Name with Author (POWO standardized)
