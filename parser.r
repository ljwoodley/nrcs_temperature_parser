###############################################################################################################################################
##
##  Name: get_average_temperature
##  Function: Transforms raw text files containing the average climate data, provided by the National Resource
##            Conservation service, into a tidy DataFrame
##  Usage: df <- get_average_temperature("https://www.wcc.nrcs.usda.gov/ftpref/data/climate/table/temperature/history/alaska/33j01s_tavg.txt") 
##        
################################################################################################################################################


##---------Create ruler to measure column widths needed for read.fwf---------###
## cat(paste0(rep(c(1:9,"+"),8), collapse=""))
## cat(temperatures[[1]])
## cat(paste0(rep(c(1:9,"+"),8), collapse=""))
####################################################################################

get_average_temperature <- function(file_url){
  
  library(tidyverse)
  
  #read file 
  txt_file <- readLines(file_url) 
    
  if(length(txt_file) < 4){
    cat("There is no Average Air Temperature Data available for the selected sation\n")
    stop()
  } else{
  
    #collapse file to single line  
    txt_file <- paste(txt_file, collapse = "\n")
    
    #split file according to year
    raw_txt <- str_split(txt_file, "----------\n") %>% unlist()
    
    #remove unecessary lines
    raw_txt <- raw_txt[-length(raw_txt)]
    raw_txt <- str_remove(raw_txt, "\n---    ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---")
    raw_txt <- str_remove(raw_txt, "---")
    
    #extract the year for each file section
    year <- str_extract(raw_txt, "[[:digit:]]{2}[[:space:]]{2}Average") %>% 
    {if_else(. > 80, paste0(19,.), paste0(20,.))} %>% 
      str_extract_all(., "[[:digit:]]+") 
    
    #extract station id and name for each file section
    station <- str_extract(raw_txt, "[[:space:]]:[[:space:]][[:print:]]+") %>% 
      str_remove("[[:space:]]:[[:space:]]")
    
    #extract lines 1:31 (exclude mean,min and max)
    temperatures <- str_extract_all(raw_txt, "day.*(\\n.*){31}")
    temp_file <- tempfile()
    
    #create empty list to store DataFrames
    df_list <- list()
    
    for (i in seq_along(year)){
      writeLines(temperatures[[i]],temp_file)
      
      df <- read.fwf(temp_file, width = c(3, 7, rep(6,11)), stringsAsFactors = FALSE)
      #rename columns
      colnames(df) <- df[1,]
      
      suppressWarnings(
        df_list[[i]] <- df %>% 
          slice(-1) %>% 
          mutate_all(as.numeric) %>% 
          gather(month, temp, -day) %>% 
          mutate(year = year[[i]], station = station[[i]]) %>% 
          separate(station, into = c("id","station"), ",")
      )
    }
    
  }
  #return tidy DataFrame
  return(do.call(rbind, df_list))
}

