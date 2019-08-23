# 

#     /$$$$$$   /$$$$$$  /$$    /$$         /$$                     /$$   /$$ /$$      /$$ /$$      
#    /$$__  $$ /$$__  $$| $$   | $$        | $$                    | $$  / $$| $$$    /$$$| $$      
  # | $$  \__/| $$  \__/| $$   | $$       /$$$$$$    /$$$$$$       |  $$/ $$/| $$$$  /$$$$| $$      
  # | $$      |  $$$$$$ |  $$ / $$/      |_  $$_/   /$$__  $$       \  $$$$/ | $$ $$/$$ $$| $$      
  # | $$       \____  $$ \  $$ $$/         | $$    | $$  \ $$        >$$  $$ | $$  $$$| $$| $$      
  # | $$    $$ /$$  \ $$  \  $$$/          | $$ /$$| $$  | $$       /$$/\  $$| $$\  $ | $$| $$      
  # |  $$$$$$/|  $$$$$$/   \  $/           |  $$$$/|  $$$$$$/      | $$  \ $$| $$ \/  | $$| $$$$$$$$
  # \______/  \______/     \_/             \___/   \______/       |__/  |__/|__/     |__/|________/
  
#    /$$$$$$$$                        /$$            /$$$$$$     
#   | $$_____/                       | $$           /$$__  $$    
#   | $$     /$$$$$$   /$$$$$$       | $$          | $$  \ $$    
#   | $$$$$ /$$__  $$ /$$__  $$      | $$          | $$$$$$$$    
#   | $$__/| $$  \ $$| $$  \__/      | $$          | $$__  $$    
#   | $$   | $$  | $$| $$            | $$          | $$  | $$    
#   | $$   |  $$$$$$/| $$            | $$$$$$$$ /$$| $$  | $$ /$$
#   |__/    \______/ |__/            |________/|__/|__/  |__/|__/




#                                                       by: Aaron Fehir
#                                                        (2019/07/08)
#                                                  https://github.com/Fehiroh



# About:

# This program is an ETL program which extracts information out of a csv,
# generates a set of xml documents, and then populates these xmls with
# information stored in the csvs. It was created upon request for the City of
# Los Angelos in order to import information into their pavement management
# software.





                                                      # Libraries
######################################################################################################################


if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(shiny, shinyWidgets, tidyverse, shinyFiles, methods, XML, xml2, RCurl, janitor, gtools, jpeg, shinythemes)


# 
# x <- "https://raw.githubusercontent.com/Fehiroh/Paver_distress_codes/master/background.jpg"    
# download.file(x, "x.jpg", mode = "wb")
# jj <- readJPEG("x.jpg",native=TRUE)



                                          # Importing Paver Codes and Column Names
######################################################################################################################

#  These are links to stable repositories that have the distress codes in them. 

url_to_column_names <- "https://raw.githubusercontent.com/Fehiroh/Paver_distress_codes/master/table_with_csv_names.csv"

url_to_acp_distress <- "https://raw.githubusercontent.com/Fehiroh/Paver_distress_codes/master/paver_distress_codes.csv"

url_to_jcp_distress <- "https://raw.githubusercontent.com/Fehiroh/Paver_distress_codes/master/jcp_paver_codes"

thing <- "\\\\"
wd <- as.character(getwd())
input_for_home <- str_replace_all(wd, "\\/", thing[1])



                                              # Pre-building the node-strings
#######################################################################################################################

# These are used to create the nodes in the xml. Placed up here to improve readability of actual conversion function. 


# String
start_str <- "<pavementData>"
end_str <- "</pavementData>"
penultimate_str_end <- "</geospatialInspectionData>"
strt_insp_elem <- "<inspectedElement "
end_insp_elem <- "</inspectedElement>"

#Insp_element attribute strings
insp_elem_attr_1_start <- 'inspectedElementID="'
insp_elem_attr_2_start <- 'size="'
insp_elem_attr_3_start <- 'noDistresses="'
insp_elem_attr_end <- '"'
comment <- 'comment="" ' 


                                                  # List of Distresses 
#######################################################################################################################

# This list is used to select the distress columns regardless of whether the csv contains acp, jcp, or a mixture of both. 

list_of_distresses_and_pid_plus <- c("allig_l", "allig_m", "allig_h", "bleed_l", "bleed_m", "bleed_h", "block_l", 
                        "block_m", "block_h", "block_s", "block_ls", "bump_l", "bump_m", "bump_h", 
                        "corrug_l", "corrug_m", "corrug_h", "depress_l", "depress_m", "depress_h",  
                        "edge_l", "edge_m", "edge_h", "ashould_l", "ashould_m", "ashould_h", "lo_tr_l", 
                        "lo_tr_m", "lo_tr_h", "lo_tr_s", "lo_tr_ls", "acpatch_l", "acpatch_m", 
                        "acpatch_h", "acpolish", "pothole_l", "pothole_m", "pothole_h", "rrx_l",
                        "rrx_m", "rrx_h", "rut_l", "rut_m", "rut_h", "max_rut_depth_lwp", "max_rut_depth_rwp", 
                        "avg_rut_depth_lwp", "avg_rut_depth_rwp", "max_rut_depth", "avg_rut_depth", "shove_l",
                        "shove_m", "shove_h", "slip_l", "slip_m", "slip_h", "swell_l", "swell_m", "swell_h", 
                        "ravel_m", "ravel_h", "buckle_l", "buckle_m", "buckle_h", "co_br_l", "co_br_m", "co_br_h",
                        "div_slab_l", "div_slab_m", "div_slab_h", "dcrack_l", "dcrack_m", "dcrack_h", "fault_l", 
                        "fault_m", "fault_h", "jnt_seal_l", "jnt_seal_m", "jnt_seal_h", "pshould_l", "pshould_m",
                        "pshould_h", "linear_l", "linear_m", "linear_h", "pcpatlg_l", "pcpatlg_m", "pcpatlg_h", 
                        "pcpatsm_l", "pcpatsm_m", "pcpatsm_h", "pcpolish", "popout", "pump", "punch_l", 
                        "punch_m", "punch_h", "scale_l", "scale_m", "scale_h", "shrink", "cospall_l", "cospall_m",
                        "cospall_h", "jntspall_l", "jntspall_m", "jntspall_h", "pid_plus")


                                              # Df_to_determin_ranking_of_joint_seals
#######################################################################################################################

# Since paver only  wants the highest joint seal rating, this is used to filter out the lesser joint_seals
jnt_seal_rank_names <- c("jnt_seal_names","joint_seal_rank")
jnt_seal_names <- as.character(c("jnt_seal_h", "jnt_seal_m", "jnt_seal_l"))
jnt_seal_ranks <- c("1", "2", "3")
jnt_seal_rank_df <- data.frame(matrix(ncol = length(jnt_seal_rank_names ), nrow = 3))
colnames(jnt_seal_rank_df) <- jnt_seal_rank_names 
jnt_seal_rank_df$rank <- jnt_seal_ranks
jnt_seal_rank_df$names <- jnt_seal_names
jnt_seal_rank_df <<- jnt_seal_rank_df[,3:4]





                                                        # Functions
#######################################################################################################################


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


# Heart of the Program
#######################

# This is the actual function that converts the csv, it only runs when the convert button is pressed.

run_csv_to_xml_converter <- function(){
  
    #convert names to prevent formatting issus from crashing the app
    names(og_data) <- tolower(names(og_data)) 

    # this provides the 
    unique_date_table <- og_data %>% 
      distinct(colldate) %>% 
      mutate(row_numbers = row_number()) %>% 
      select(row_numbers, colldate) %>% 
      mutate(units = 'units="English"', 
             level = 'level="SAMPLE"', 
             inspector = 'inspector="Vision Inspector">', 
             month_1 = str_extract(colldate, "(?<=[:punct:])\\d+(?=[:punct:])"), 
             day_1 = str_extract(colldate, "^\\d+(?=[:punct:])"), 
             year_prep = str_extract(colldate, "\\d{2}$")) %>%
      mutate(year = str_c("20", year_prep), 
             day_2 = str_pad(month_1, 2, "left", "0"), 
             month_2 = str_pad(day_1, 2, "left", "0")) %>%
      mutate(new_colldate = str_c(month_2, "/", day_2, "/", year), 
             filename_colldate = str_c("RoadwareImport", year, month_2, day_2)) %>% 
      select(-month_1, -day_1) %>% 
      mutate(inspection_date_1 = str_c("inspectionDate=", '"', new_colldate, '"')) %>% 
      mutate(full_opening_statement = str_c("<geospatialInspectionData", inspection_date_1, units, level, inspector, sep = " ")) %>% 
      select(-units, -level, -inspector, inspection_date_1) 
  

  
  # Creating the initial files
  #############################
  
  # List of filenames, that can be cycled through to change the .txt to .xml after sink(), 
  # currently blank, gets populated by create_files()
    
  list_of_files <- c()
  
  # create a file for each i in x, filetype determines filetype
  create_files <- function(x, filetype){
    og_wd <<- getwd()
    setwd(place_to_put_xmls_3)
    for (i in x){
      file_name <- str_c(as.character(i), filetype)
      file.create(file_name)
      list_of_files <<- append(list_of_files, file_name)
    }
  }
  
  place_to_put_xmls_2 <- str_extract(place_to_put_xmls, "/.+")
  place_to_put_xmls_3 <<- str_c("C:", place_to_put_xmls_2)
  
  # Makes the files 
  create_files(unique_date_table$filename_colldate, ".txt")
  
  
  
  
  
  # Populates those files with the xml values 
  ###########################################
  
  #start to parse the data and write into the files, itteration used heavily due to the branching structure of xmls. 
  # gets collection dates, then sections belonging to collection date, then which distresse are present in each section,
  # and then what the values of each distress are. 
  
  date_index <- 1
  first_date <- as.character(unique_date_table$colldate[date_index])
  withProgress(message = "Overall Progress", value = 0.01, detail = first_date, {
    
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
               pid = paste(networkid, branchid, sectionid, sep= "::"), 
               pid_plus = paste(pid, beginchainage, sep = "::"))
      
      # Determine if there are distresses for each PID
      are_there_distress <- relevant_rows %>% 
        select(one_of(list_of_distresses_and_pid_plus)) %>%
        column_to_rownames("pid_plus") %>%
        remove_empty("cols")
      
      row_names <- row.names(are_there_distress)
      are_there_distress$r_sum <- rowSums(are_there_distress)
      
      
      are_there_distress <- are_there_distress %>% 
        mutate(no_distresses = if_else(r_sum > 0, "false", "true"), 
               pid_plus = cbind(as.character(row_names)))
      
      
      are_there_distress_column <- are_there_distress %>% 
        select(no_distresses, pid_plus)
      
      are_there_distress_column$pid_plus <- as.character(are_there_distress_column$pid_plus)
      
      relevant_rows <- left_join(relevant_rows, are_there_distress_column, by = "pid_plus")
      
      
      
      # Printing the header of the xml
      sink(sink_pointer)
      cat(start_str, geospatial_tag, sep = "\n")
      
      
      # Select one row, and determine if there is distress
      #####################################################
      
      pid_index <- 1
      first_pid <- as.character(relevant_rows$pid[pid_index])
      withProgress(message = first_date, value = 0.1, detail = first_pid, { #tracks with PID is being processed
      
      
      for(row_number in relevant_rows$row_numbers){
        
        specific_row <- relevant_rows %>% 
          filter(row_numbers == row_number) 
        
        pid_plus_for_row <- specific_row$pid_plus
        
        
        # size is slab count for jcp, inspection length for acp
        if(isTRUE(pavement_type == "acp")){
          size_input <- specific_row$insp_area
        } else {
          size_input <- specific_row$slabcnt
        }
       
        
        pid_input <- specific_row$pid
        insp_elem_id_input  <- specific_row$sampleid
        no_distress_in_node <- specific_row$no_distresses
        
        insp_elem_id_input_1 <- str_extract(insp_elem_id_input, "[:digit:][:punct:]")
        insp_elem_id_input_2 <- str_extract(insp_elem_id_input, "[:digit:]$")
        insp_elem_id_input <- str_c(insp_elem_id_input_1, insp_elem_id_input_2)
        insp_elem_attr_1 <- paste0(insp_elem_attr_1_start, insp_elem_id_input, insp_elem_attr_end)
        insp_elem_attr_2 <- paste0(insp_elem_attr_2_start, size_input, insp_elem_attr_end)
        insp_elem_attr_pid <- paste0('pid="', pid_input, insp_elem_attr_end)
        insp_type <- paste0('inspectedType="R" ', comment)
        insp_elem_attr_3 <- paste0(insp_elem_attr_3_start, no_distress_in_node, insp_elem_attr_end)
        
        middle_of_insp_attr <- paste(insp_elem_attr_1, insp_elem_attr_2, 
                                     insp_elem_attr_pid, insp_type, 
                                     insp_elem_attr_3)
        
        inspect_element_start_node <- paste0('   ', strt_insp_elem , middle_of_insp_attr,  '>')
        
        # start and immediately close out node if no distress present
        if (no_distress_in_node == "true"){
          cat(inspect_element_start_node, 
              "    <inspectionData>",
              "     <PCIDistresses>",
              "     </PCIDistresses>",
              "   </inspectionData>",
              "</inspectedElement>",
              sep = "\n")
          
          
        } else{
        # start node 
          cat(inspect_element_start_node, 
              "<inspectionData>",
              "<PCIDistresses>", 
              sep = "\n")
          
          # Figure out what the distresses are
          specific_distresses <- are_there_distress %>% 
            filter(pid_plus == pid_plus_for_row)%>% 
            select_if(actual_value) %>% 
            select(-pid_plus, -r_sum, -no_distresses)
          
          distresses_present_in_node <- names(specific_distresses)
          
          
          # figure out if there are multiple Joint Seal Damages, if so, grab only the highest severity
          dpin <- as.data.frame(distresses_present_in_node)
          names(dpin)[1] <- "distress_present"
          
          dpin <- dpin %>% 
            mutate(joint_seals_present = str_detect(distress_present, "jnt_seal")) %>% 
            mutate(distress_present = as.character(distress_present))
          
          other_dpin <- dpin %>% 
            filter(joint_seals_present == FALSE) %>% 
            select(distress_present) %>% 
            unlist() %>% unname() %>% as.character()
          
          jnt_seals <- dpin %>% 
            filter(joint_seals_present == TRUE) %>% 
            select(distress_present)
          
          number_of_joint_seals_present <- as.numeric(nrow(jnt_seals))
          if (number_of_joint_seals_present != 0){
            if(number_of_joint_seals_present > 1){
              jnt_seals <- jnt_seals %>% 
                left_join(jnt_seal_rank_df, by = c("distress_present" = "names")) %>% 
                filter(rank == min(rank)) 
            }
            highest_jnt_seal <- as.character(jnt_seals[1,1])
            distresses_present_in_node <- append(highest_jnt_seal, other_dpin)
          }
          
          
          
          last_distress <- last(distresses_present_in_node)
          
          
          
            # Individual Distresses in a single Row
            #######################################
          
            for(actual_distress in distresses_present_in_node){
              
              single_distress <- specific_distresses%>% 
                select(actual_distress) 
              
              name_of_distress <- names(single_distress)
              
              names(single_distress)[1] <- "actual_value"
              
              # Getting the severity Table
              single_distress_w_info <- single_distress %>%
                mutate(distress_name = name_of_distress) %>% 
                mutate(severity = str_extract(distress_name, "(?<=[:punct:])[:alpha:]$")) %>% 
                mutate(distress_type = str_extract(distress_name, ".+(?=[:punct:]{1}[:alpha:]+$)"))
              
              if(isTRUE(single_distress_w_info$distress_name %in% c("pcpolish", "popout", "pump", "punch","shrink"))){
                single_distress_w_info$distress_type <- single_distress_w_info$distress_name
              }
              
              single_distress_w_info <- single_distress_w_info %>% 
                left_join(paver_code_table, by = c("distress_type" = "corresp_col_names")) %>% 
                select(Distress_Code, severity, distress_type, actual_value)
              
              
              it_is_acpatch <- str_detect(single_distress_w_info$distress_type, "acpatch")
              
          
              
               # Solves the ACPATCH issue, which level distress code 11s as NA
              if(isTRUE(it_is_acpatch) & isTRUE(pavement_type == "acp")){
                single_distress_w_info$Distress_Code <- 11
              }
              
    
              distress_code <- paste0('distressCode="', single_distress_w_info$Distress_Code, '"')
              severity <- paste0('severity="', toupper(single_distress_w_info$severity), '"')
              quantity <- paste0('quantity="', single_distress$actual_value, '" ', comment, '/>')
              
              distress_info <- paste('<levelDistress', distress_code, severity, quantity, sep = " ")
              
              # Prints distress nodes 
              if(actual_distress == last(distresses_present_in_node)){
                cat(distress_info)
              } else {
                if(!is.na(single_distress_w_info$Distress_Code)){
                  cat(distress_info)
                  cat("\n")
                }
               }
             
          }
        

          # prints end of Inspected Element
          cat("\n")
          cat("</PCIDistresses> ",
              "</inspectionData>",
              "</inspectedElement>",
              sep = "\n")
          
          date_index <<- date_index + 1
          date_to_display <- as.character(unique_date_table$colldate[date_index])
          incProgress(1/nrow(relevant_rows), detail =  as.character(relevant_rows$pid[pid_index]))
        }
      } 
     
      })
      # Pirnts end of xml
      cat("\n")
      cat(penultimate_str_end, end_str, sep = "\n")
      sink()
      
      
      incProgress(1/nrow(unique_date_table), detail = date_to_display)
      } 
  })
  
  
  
  # Convertion to .xml
  ############################################################################################################################
  
  # Since the original files that are created are formatted correctly for xml, but are stored  in  .txts, 
  # all we have to do to change them into xmls is change "*.txt" into "*.xml"
  # this accomplishes that using the list_of_files list generated earlier.
  
  withProgress(message = "converting to xml",{
    for (file_name in list_of_files){
      file_name_stripped <- str_extract(file_name, ".+(?=[:punct:][:alpha:]+$)")
      file_name_new <- str_c(file_name_stripped, ".xml")
      filename_new_plus_path <- str_c(place_to_put_xmls_3 , "/", file_name_new)
      old_filename_plus_path <- str_c(  place_to_put_xmls_3 , "/", file_name)
      file.rename(old_filename_plus_path, filename_new_plus_path)
    }
  })
}




######################################################################################################################
#                                                _    _ _____ 
#                                              | |  | |_   _|
#                                              | |  | | | |  
#                                              | |  | | | |  
#                                              | |__| |_| |_ 
#                                               \____/|_____|                                                  #UI
######################################################################################################################
# Define UI for application that draws a histogram
ui <- fixedPage(theme = shinytheme("darkly"),
                
                tags$head(
                  tags$style(
                    "body{
                    min-height: 450px;
                    max-height: 450px;
                    height: 450px;
                    height: auto;
                    min-width: 400px;
                    max-width: 450px;
                    margin: auto;
                    }"
      )
                  ),
  
      tags$head(
        tags$style(HTML("
                        @import url('https://fonts.googleapis.com/css?family=Anton|Roboto&display=swap');
                        
                        h1 {
                        font-family: 'Anton', sans-serif;
                        font-weight: 1000;
                        line-height: 1.1;
                        color: #ffffff;
                        font-size: 40px;
                        } 

                         h3{
                        font-family: 'Roboto', sans-serif;
                        font-weight: 1000;
                        line-height: 1.1;
                        color: #dfdfdf;
                        font-size: 25px;
                        vertical-align: middle;
                        } 
                        

                  
                        "))
        ),
      
      
      tags$head(
        tags$style(HTML("
                        @import url(https://fonts.googleapis.com/css?family=Roboto&display=swap');
                        
                        h3{
                        font-family: 'Roboto', sans-serif;
                        font-weight: 1000;
                        line-height: 1.1;
                        color: #e9e5dd;
                        font-size: 25px;
                        vertical-align: middle;
                        } 
                        
                        
                        "))
        ),
      
   headerPanel(title = "Csv to Xml Converter"),
   

   
   
   # tags$style(type = 'text/css', '.navbar { background-color: #415EA3;
   #                         font-family: Muli;
   #                         font-size: 60px;
   #                         color: #ECECEC; }',
   #            
   #            '.navbar-dropdown { background-color: #415EA3;
   #                         font-family: Muli;
   #                         font-size: 60px;
   #                         color: #ECECEC; }',
   #            
   #            '.navbar-default .navbar-brand {
   #                          font-size: 35px;
   #                          color: #ECECEC;
   #                         }'
   # ),
  
           fluidRow(width = 4, 
                   sidebarPanel(
                     fileInput("file", label = h3("Input csv")),
                     selectInput("select", label = h3("Pavement Type"), 
                                 choices = list("ACP" = 1, "JCP" = 2), 
                                 selected = 1), 
                     br(),
                     h3("xml Export"),
                     shinyDirButton("dir", "Select Export Directory", "Select Folder to Store xml", 
                                    style = "vertical-align: 'middle'; height: 50; width: 100%; font-size: 15px;"), 
                     br(),
                     br(),
                     br(), 
                     br(),
                     actionBttn(inputId = "pro_button", label = "Convert", icon = NULL, style = "fill",
                                color = "warning", size = "lg", block = TRUE, no_outline = FALSE), 
                     tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px; font-family: 'Roboto', sans-serif;}")
                     
                     
                   
                     
   
           
           # mainPanel(
           #   img(src = 'C:/Users/afehir/Documents/act_dir/trying_to_get_image/trying_to_get_this_image/www/x.jpg', align = "right")
           # )

    )
  )
)


######################################################################################################################
#                                       _____                          
#                                     / ____|                         
#                                     | (___   ___ _ ____   _____ _ __ 
#                                     \___ \ / _ \ '__\ \ / / _ \ '__|
#                                    ____) |  __/ |    \ V /  __/ |   
#                                   |_____/ \___|_|     \_/ \___|_|                                          #Server
######################################################################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {
  
############################ Server: Directory ########################################################################


  # file select is loaded into R as at dataframe
  observeEvent(input$file, {
                          inFile <- input$file
                          og_data <<- read_csv(inFile$datapath)
  
  })
              
  
  # looks at the pavement type, and uploads reference codes for ACP or JCP accordingly
  observeEvent(input$select,
               if(input$select == 1){
                 paver_code_table <<- read_csv(url_to_acp_distress)
                 pavement_type <<- "acp"
                 
                 #change the PATC in the paver_code_table to PATCH
                 paver_code_table[11,3] <<- "ACPATCH"
               } else {
                 paver_code_table <<- read_csv(url_to_jcp_distress)
                 pavement_type <<- "jcp"
               }
               )
  
  # This dictates which directory will house the xml files created by this app.
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = 'C:'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  
  global <- reactiveValues(datapath = getwd())
  
  dir <- reactive(input$dir)
  
  output$dir <- renderText({
    global$datapath
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 place_to_put_xmls <<- as.character(global$datapath)
               })

  # If convert button is pressed, convert the csv into xmls and deposit them in the specified directory.
   observeEvent(input$pro_button, {
     run_csv_to_xml_converter()
     setwd(og_wd) 
   }
               
   );
}

# Run the application 
shinyApp(ui = ui, server = server)

