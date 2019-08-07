

#   _____                       _____                            _                                 _                         _ 
#  |  __ \                     |_   _|                          | |           _                   | |                       | |
#  | |__) |_ ___   _____ _ __    | |  _ __ ___  _ __   ___  _ __| |_ ___ _ __(_)   ___ _____   __ | |_ ___   __  ___ __ ___ | |
#  |  ___/ _` \ \ / / _ \ '__|   | | | '_ ` _ \| '_ \ / _ \| '__| __/ _ \ '__|    / __/ __\ \ / / | __/ _ \  \ \/ / '_ ` _ \| |
#  | |  | (_| |\ V /  __/ |     _| |_| | | | | | |_) | (_) | |  | ||  __/ |   _  | (__\__ \\ V /  | || (_) |  >  <| | | | | | |
#  |_|   \__,_| \_/ \___|_|    |_____|_| |_| |_| .__/ \___/|_|   \__\___|_|  (_) \___|___/ \_/     __\___/   /_/\_\_| |_| |_|_|
#                                              | |                                                                             
#                                              |_| 

#                                                       by: Aaron Fehir (2019/07/08) 
#                                                        https://github.com/Fehiroh


                                                              

                                                              # Libraries
#######################################################################################################################################
if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load("tidyverse", "methods", "XML", "xml2", "RCurl", "janitor")


                                                             # Data Import
#######################################################################################################################################
# csv containing the relevant information
og_data <- read_csv("C:/Users/afehir/Documents/Projects/Los Angelos/xml_test_data_multiple_day.csv") #CHANGE_NECESSARY, put the filepath to the csv. 


# paver reference codes imported from a stable github repository
url_to_acp_distress <- "https://raw.githubusercontent.com/Fehiroh/Paver_distress_codes/master/paver_distress_codes.csv"
paver_code_table <- read_csv(url_to_acp_distress)



                                                             # Functions
######################################################################################################################################
# this predicate function is used to eliminate columns that do not actually contain any distress information 
actual_value <- function(x){
  if (is.numeric(x)){
    if(x != 0 & !is.na(x)){
      return(TRUE)
    } else {
      return(FALSE)
    }} else {
      if(!is.na(x)){
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
}


                                                    # Pre-building the node-strings
#######################################################################################################################################
# String
start_str <- "<pavementData>"
end_str <- "</pavementData>"
penultimate_str_end <- "</geospatialInspectionData>"
strt_insp_elem <- "<inspectedElement "
end_insp_elem <- "</inspectedElement>"

#Insp_element attribute strings
insp_elem_attr_1_start <- 'inspectedElementID="'
insp_elem_attr_2_start <- 'size="'
insp_elem_attr_3_start <- 'noDistress="'
insp_elem_attr_end <- '"'



                                                   # Creating the Reference Table 
#######################################################################################################################################
# Creating the reference that will generate the dates, the file names, and provide reference values for itterative filtering

unique_date_table <- og_data %>% 
  distinct(colldate) %>% 
  mutate(row_numbers = row_number()) %>% 
  select(row_numbers, colldate) %>% 
  mutate(units = 'units="English"', 
         level = 'level="SAMPLE"', 
         inspector = 'inspector="Vision Inspector">', 
         month_1 = str_extract(colldate, "(?<=[:punct:])\\d+(?=[:punct:])"), 
         day_1 = str_extract(colldate, "^\\d+(?=[:punct:])"), 
         year = str_extract(colldate, "\\d{4}$")) %>%
  mutate(day_2 = str_pad(month_1, 2, "left", "0"), 
         month_2 = str_pad(day_1, 2, "left", "0")) %>%
  mutate(new_colldate = str_c(month_2, "/", day_2, "/", year), 
         filename_colldate = str_c("RoadwareImport", year, month_2, day_2)) %>% 
  select(-month_1, -day_1) %>% 
  mutate(inspection_date_1 = str_c("inspectionDate=", '"', new_colldate, '"')) %>% 
  mutate(full_opening_statement = str_c("<geospatialInspectionData", inspection_date_1, units, level, inspector, sep = " ")) %>% 
  select(-units, -level, -inspector, inspection_date_1)




                                                       # Creating the initial files
#######################################################################################################################################

# List of filenames, that can be cycled through to change the .txt to .xml after sink(), 
# currently blank, gets populated by create_files()
list_of_files <- c()

# create a file for each i in x, filetype determines filetype
create_files <- function(x, filetype){
  for (i in x){
    file_name <- str_c(as.character(i), filetype)
    file.create(file_name)
    list_of_files <<- append(list_of_files, file_name)
  }
}

setwd("~/act_dir/Testing_xml") #CHANGE_NECESSARY, point this at where you would like the files to be
wd <- as.character(getwd())
# Mkaes the files 
create_files(unique_date_table$filename_colldate, ".txt")





                                               # Populates those files with the xml values 
#######################################################################################################################################
#start to parse the data and write into the files 
for(row_number in unique_date_table$row_numbers) {
  unique_date_ref_row <- unique_date_table %>% 
    filter(row_numbers == row_number)
  
  
  collection_date_for_filter <- unique_date_ref_row$colldate
  geospatial_tag <- unique_date_ref_row$full_opening_statement
  sink_pointer <- str_c(unique_date_ref_row$filename_colldate, ".txt")
  
  
  # grab the original data belonging to that specific day. 
  
  relevant_rows <- og_data %>% 
    filter(colldate == collection_date_for_filter) %>% 
    mutate(row_numbers = row_number(), 
           PID = paste(networkid, branchid, sectionid, sep= "::"), 
           PID_plus = paste(PID, BeginChainage, sep = "::"))
  
  are_there_distress <- relevant_rows %>% 
    select(PID_plus, 14:64) %>%
    column_to_rownames("PID_plus") %>%
    remove_empty("cols")
  
  row_names <- row.names(are_there_distress)
  are_there_distress$r_sum <- rowSums(are_there_distress)
  
  
  are_there_distress <- are_there_distress %>% 
    mutate(no_distresses = if_else(r_sum > 0, "false", "true"), 
           PID_plus = cbind(as.character(row_names)))
           
           
    are_there_distress_column <- are_there_distress %>% 
             select(no_distresses, PID_plus)
    
  are_there_distress_column$PID_plus <- as.character(are_there_distress_column$PID_plus)
           
  relevant_rows <- left_join(relevant_rows, are_there_distress_column, by = "PID_plus")
           
  
  
  #printing the header of the xml
  sink(sink_pointer)
  cat(start_str, geospatial_tag, sep = "\n")


  
  # start extracting specific information from rows 
  for(row_number in relevant_rows$row_numbers){
    
    specific_row <- relevant_rows %>% 
      filter(row_numbers == row_number) 
    
    pid_plus_for_row <- specific_row$PID_plus
    
    size_input <- specific_row$INSP_AREA
    PID_input <- specific_row$PID
    insp_elem_id_input  <- specific_row$sampleid
    no_distress_in_node <- specific_row$no_distresses
    
    #building elem isnp attr strings
    insp_elem_attr_1 <- paste0(insp_elem_attr_1_start, insp_elem_id_input, insp_elem_attr_end)
    insp_elem_attr_2 <- paste0(insp_elem_attr_2_start, size_input, insp_elem_attr_end)
    insp_elem_attr_pid <- paste0('PID="', PID_input, insp_elem_attr_end)
    insp_type <- 'inspectedType="R" '
    insp_elem_attr_3 <- paste0(insp_elem_attr_3_start, no_distress_in_node, insp_elem_attr_end)
    
    middle_of_insp_attr <- paste(insp_elem_attr_1, insp_elem_attr_2, 
                                 insp_elem_attr_pid, insp_type, 
                                 insp_elem_attr_3)
    
    inspect_element_start_node <- paste0(strt_insp_elem , middle_of_insp_attr, '>')
    
    #print entire thing if 
    if (no_distress_in_node == "true"){
      cat("\n")
      cat(inspect_element_start_node, 
          "<inspectionData>",
            "<PCIDistresses/> ",
            "</inspectionData>",
            "</inspectedElement>",
          sep = "\n")
    
    
      } else{
    
        cat("\n")
        cat(inspect_element_start_node, 
            "<inspectionData>",
            "<PCIDistresses>", 
             sep = "\n")
     
      
      # here's where the part about distress goes 
      specific_distresses <- are_there_distress %>% 
        filter(PID_plus == pid_plus_for_row)%>% 
        select_if(actual_value) %>% 
        select(-PID_plus, -r_sum, -no_distresses)
      
      distresses_present_in_node <- names(specific_distresses)
      last_distress <- last(distresses_present_in_node)
      
      for(actual_distress in distresses_present_in_node){
        
        single_distress <- specific_distresses%>% 
          select(actual_distress) 
        
        name_of_distress <- names(single_distress)
        
        names(single_distress)[1] <- "actual_value"
        
        single_distress_w_info <- single_distress %>%
          mutate(distress_name = name_of_distress) %>% 
          mutate(severity = str_extract(distress_name, "(?<=[:punct:])[:alpha:]$")) %>% 
          mutate(distress_type = str_extract(distress_name, ".+(?=[:punct:]{1}[:alpha:]+$)")) %>% 
          left_join(paver_code_table, by = c("distress_type" = "corresp_col_names")) %>% 
          select(Distress_Code, severity, distress_type, actual_value)
        
        
        distress_code <- paste0('distressCode="', single_distress_w_info$Distress_Code, '"')
        severity <- paste0('severity="', single_distress_w_info$severity, '"')
        quantity <- paste0('quantity="', single_distress$actual_value, '"/>')
        
        distress_info <- paste('<levelDistress', distress_code, severity, quantity, sep = " ")
        
        #Prints distress nodes
        if(actual_distress == last(distresses_present_in_node)){
          cat(distress_info)
        } else {
          cat(distress_info)
          cat("\n")
        }
       }
  # prints end of Inspected Element
    cat("\n")
    cat("</PCIDistresses> ",
        "</inspectionData>",
        "</inspectedElement>",
        sep = "\n")

  }
    } 
  # Pirnts end of xml
  cat("\n")
  cat(penultimate_str_end, end_str, sep = "\n")
  sink()
}


                                                                # Convertion to .xml
############################################################################################################################
for (file_name in list_of_files){
  file_name_stripped <- str_extract(file_name, ".+(?=[:punct:][:alpha:]+$)")
  file_name_new <- str_c(file_name_stripped, ".xml")
  filename_new_plus_path <- str_c(wd, "/", file_name_new)
  old_filename_plus_path <- str_c(wd, "/", file_name)
  file.rename(old_filename_plus_path, filename_new_plus_path)
}





