library(readxl)
library(stringr)
library(tidyverse)
library(dplyr)
library(readr)
#install.packages('SAScii')
library(SAScii)
library(haven)
library(writexl)

# Set the working directory
source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)

# Specifying the name of input and output files
zip_files <- c('INT_STU12_DEC03',
               'INT_COG12_DEC03',
               'INT_COG12_S_DEC03',
               'INT_PAQ12_DEC03',
               'INT_SCQ12_DEC03')

sas_files <- c('PISA2012_SAS_student.sas',
               'PISA2012_SAS_cognitive_item.sas',
               'PISA2012_SAS_scored_cognitive_item.sas',
               'PISA2012_SAS_parent.sas',
               'PISA2012_SAS_school.sas')

output_files <- c('student',
                  'cognitive',
                  'cognitive_score',
                  'parent',
                  'school')



i <- 1
for (i in 1:length(zip_files)){

  ######################################
  #       Reading dictionary           #
  ######################################

  # Loading the dictionary using SAScii
  dictionary <- parse.SAScii(sas_ri = paste0('input/', sas_files[i]))  

  # Calculating start and end positions for each variable
  dictionary$start <- cumsum(c(1, dictionary$width[-nrow(dictionary)]))
  dictionary$end <- cumsum(dictionary$width)
  
  ######################################
  #    Reading type format part       #
  ######################################
  # Reading the SAS file
  sas_content <- readLines(paste0('input/', sas_files[i])) 
  
  # Combining the content into a single string
  sas_string <- paste(sas_content, collapse = "\n")
  
  # Extracting the "format" sections from the SAS file
  format_sections <- str_extract_all(sas_string, "format\n[\\s\\S]*?;")
  
  # Getting the first one (the one which tells the lengths and types)
  format_type <- format_sections[[1]][1]
  
  # Extracting variable-format pairs from the format_type section
  format_lines <- str_split(format_type, "\n")[[1]]
  
  # Creating a set of variables that are strings along with their sizes
  string_vars <- sapply(format_lines, function(line) {
    # Checking if the format contains a $
    if (grepl("\\$", line)) {
      # Extracting the variable name
      parts <- str_split(str_trim(line), "\\s+")[[1]]
      var_name <- toupper(parts[1])
      size <- as.numeric(gsub("\\$", "", parts[2]))  # Extract size
      return(c(var_name, size))
    }
    return(NULL)
  })
  
  # Filtering out NULL values
  string_vars <- string_vars[!sapply(string_vars, is.null)]
  string_vars <- as.data.frame(matrix(unlist(string_vars), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
  colnames(string_vars) <- c("varname", "size")

  
  ######################################
  # Creating infix part of Stata .do   #
  ######################################

  # Createing the infix command for Stata
  infix_lines <- apply(dictionary, 1, function(row) {
    var_name <- toupper(row['varname'])
    start <- row['start']
    end <- row['end']
    
    # Check if the variable is one of the string columns
    if (var_name %in% string_vars$varname) {
      # Get the size of the string variable
      size <- string_vars[string_vars$varname == var_name, "size"]
      # Add str with the size
      paste0("str", size, " ", var_name, " ", start, "-", end)
    } else {
      # Default numeric variable
      paste(var_name, paste0(start, "-", end), sep = " ")
    }
  })


  ######################################
  #      Initiating Stata .do file     #
  ######################################
  zip <- zip_files[i]
  
  # Initialize the Stata script content
  stata_script <- paste0("

* All raw data from PISA 2012 is available at https://www.oecd.org/en/data/datasets/pisa-2012-database.html

* Unzipping the file and extract the .txt file to the temp directory
local zipfile \"", paste0('input/PISA2012/', zip,'.zip'), "\"

* Extracting the txt file
unzipfile `zipfile', replace

* Moving the extracted file to the temp directory
copy \"", paste0(zip,".txt"), "\" \"temp/", paste0(zip,".txt"), "\", replace

* Defining the path to the extracted text file
local txtfile \"temp/", paste0(zip,".txt"), "\"

* Deleting the file \"", paste0(zip,".txt"), "\" from the current directory
erase \"", paste0(zip,".txt"), "\" 

* Reading the fixed-width file using infix
infix ///
")
  
  # Adding the infix lines to the script
  stata_script <- paste(stata_script, paste(infix_lines, collapse = " ///\n"), " ///", "\nusing `txtfile', clear\n", "\n", sep = "")
  
  
  ######################################
  #        Formatting missings         #
  ######################################
  
  # Reading the SAS file
  sas_content <- readLines(paste0('input/', sas_files[i]))
  
  # Combining the content into a single string
  sas_string <- paste(sas_content, collapse = "\n")
  
  # Splitting the content into chunks based on ";\n value"
  chunks <- str_split(sas_string, ";\\s*\n\\s*value", simplify = TRUE)
  
  # Function to process each chunk
  process_chunk <- function(chunk) {
    # Extracting the variable name (keep $ and F for merging)
    var_name <- toupper(str_extract(chunk, "(\\$?[A-Za-z0-9]+F)"))
    
    # Extracting the missing value patterns
    missing_patterns <- str_extract_all(chunk, "\"?\\d+\"?\\s*=\\s*\"(N/A|Invalid|Missing)\"")[[1]]
    
    # Extracting numeric values from the missing patterns
    missing_values <- as.numeric(str_extract(missing_patterns, "\\d+"))
    
    # Determining the pattern type based on any of the missing values
    pattern_type <- NA
    if (any(missing_values %in% c(7, 8, 9))) {
      pattern_type <- "7, 8, 9 are missing"
    }
    if (any(missing_values %in% c(97, 98, 99))) {
      pattern_type <- "97, 98, 99 are missing"
    }
    if (any(missing_values %in% c(96, 97, 98, 99))) {
      pattern_type <- "96, 97, 98, 99 are missing"
    }
    if (any(missing_values %in% c(9997, 9998, 9999))) {
      pattern_type <- "9997, 9998, 9999 are missing"
    }
    if (any(missing_values %in% c(99997, 99998, 99999))) {
      pattern_type <- "99997, 99998, 99999 are missing"
    }
    if (any(missing_values %in% c(777777, 888888, 999997, 999998, 999999))) {
      pattern_type <- "777777, 888888, 999997, 999998, 999999 are missing"
    }
    if (any(missing_values %in% c(9999997, 9999998, 9999999))) {
      pattern_type <- "9999997, 9999998, 9999999 are missing"
    }
    if (any(missing_values %in% c(7777777, 8888888, 99999997, 99999998, 99999999))) {
      pattern_type <- "7777777, 8888888, 99999997, 99999998, 99999999 are missing"
    }
    return(list(variable = var_name, missing_pattern = pattern_type))
  }
  
  # Processing each chunk
  results <- lapply(chunks, process_chunk)
  
  # Creating a data frame with the results
  result_df <- do.call(rbind, lapply(results, as.data.frame))
  names(result_df) <- c("Format", "MissingPattern")
  
  # Filtering out when variable is NA
  result_df <- result_df %>% filter(!is.na(Format))

  ###############################################
  # Selecting Variables and merging with formats#
  ###############################################
  
  # Extracting the "format" sections from the SAS file (there are multiple)
  format_sections <- str_extract_all(sas_string, "format\n[\\s\\S]*?;")

  # Getting the last one!
  format_section <- format_sections[[1]][length(format_sections[[1]])]

  # Extracting the "format" sections from the SAS file (there are multiple)
  format_sections <- str_extract_all(sas_string, "format\n[\\s\\S]*?;")

  # Getting the first one (the ones which tells the lengths and types)
  format_type <- format_sections[[1]][1]
  
  # Extract variable-format pairs from the format section
  format_lines <- str_split(format_section, "\n")[[1]]
  
  # Remove leading and trailing spaces
  format_lines <- str_trim(format_lines)
  
  # Create a data frame of variable-format pairs
  format_df <- do.call(rbind, lapply(format_lines, function(line) {
    parts <- str_split(line, "\\s+", simplify = TRUE)
    if (length(parts) >= 2) {
      # Remove the period at the end of the format
      format_name <- gsub("\\.$", "", parts[2])
      return(data.frame(Variable = parts[1], Format = format_name, stringsAsFactors = FALSE))
    } else {
      return(NULL)
    }
  }))
  
  # Filter out any NULL results
  format_df <- format_df[!sapply(format_df, is.null), ]

  # Merge result_df with format_df
  merged_df <- merge(format_df, result_df , by = "Format")
  
  # Adding dummy indicating if it is string or not
  merged_df <- merged_df %>% mutate(is_string = ifelse(Variable %in% string_vars$varname, 1, 0),
                                    Variable = toupper(Variable))
  
  
  # Generate Stata syntax to handle missing values
  missing_lines <- apply(merged_df, 1, function(row) {
    var_name <- toupper(row['Variable'])
    pattern <- row['MissingPattern']
    is_string <- row['is_string']
      if (!is.na(pattern) & is_string == 0) {
        if (pattern == "7, 8, 9 are missing") {
          return(paste("replace", var_name, "= . if", var_name, "== 7 |", var_name, "== 8 |", var_name, "== 9"))
        }
        if (pattern == "97, 98, 99 are missing") {
          return(paste("replace", var_name, "= . if", var_name, "== 97 |", var_name, "== 98 |", var_name, "== 99"))
        }
        if (pattern == "96, 97, 98, 99 are missing") {
          return(paste("replace", var_name, "= . if", var_name, "== 96 |", var_name, "== 97 |", var_name, "== 98 |", var_name, "== 99"))
        }
        if (pattern == "9997, 9998, 9999 are missing") {
          return(paste("replace", var_name, "= . if", var_name, "== 9997 |", var_name, "== 9998 |", var_name, "== 9999"))
        }
        if (pattern == "99997, 99998, 99999 are missing") {
          return(paste("replace", var_name, "= . if", var_name, "== 99997 |", var_name, "== 99998 |", var_name, "== 99999"))
        }
        if (pattern == "777777, 888888, 999997, 999998, 999999 are missing") {
          return(paste("replace", var_name, "= . if", var_name, "== 777777 |", var_name, "== 888888 |", var_name, "== 999997 |", var_name, "== 999998 |", var_name, "== 999999"))
        }
        if (pattern == "9999997, 9999998, 9999999 are missing") {
          return(paste("replace", var_name, "= . if", var_name, "== 9999997 |", var_name, "== 9999998 |", var_name, "== 9999999"))
        }
        if (pattern == "7777777, 8888888, 99999997, 99999998, 99999999 are missing") {
          return(paste("replace", var_name, "= . if", var_name, "== 7777777 |", var_name, "== 8888888 |", var_name, "== 99999997 |", var_name, "== 99999998 |", var_name, "== 99999999"))
        }
      # when column is string, adding " in front of numbers
      } else if (!is.na(pattern) & is_string == 1) {
        if (pattern == "7, 8, 9 are missing") {
          return(paste("replace", var_name, "= \"\" if", var_name, "== \"7\" |", var_name, "== \"8\" |", var_name, "== \"9\""))
        }
        if (pattern == "97, 98, 99 are missing") {
          return(paste("replace", var_name, "= \"\" if", var_name, "== \"97\" |", var_name, "== \"98\" |", var_name, "== \"99\""))
        }
        if (pattern == "96, 97, 98, 99 are missing") {
          return(paste("replace", var_name, "= \"\" if", var_name, "== \"96\" |", var_name, "== \"97\" |", var_name, "== \"98\" |", var_name, "== \"99\""))
        }
        if (pattern == "9997, 9998, 9999 are missing") {
          return(paste("replace", var_name, "= \"\" if", var_name, "== \"9997\" |", var_name, "== \"9998\" |", var_name, "== \"9999\""))
        }
        if (pattern == "99997, 99998, 99999 are missing") {
          return(paste("replace", var_name, "= . if", var_name, "== \"99997\" |", var_name, "== \"99998\" |", var_name, "== \"99999\""))
        }
        if (pattern == "777777, 888888, 999997, 999998, 999999 are missing") {
          return(paste("replace", var_name, "= \"\" if", var_name, "== \"777777\" |", var_name, "== \"888888\" |", var_name, "== \"999997\" |", var_name, "== \"999998\" |", var_name, "== \"999999\""))
        }
        if (pattern == "9999997, 9999998, 9999999 are missing") {
          return(paste("replace", var_name, "= \"\" if", var_name, "== \"9999997\" |", var_name, "== \"9999998\" |", var_name, "== \"9999999\""))
        }
        if (pattern == "7777777, 8888888, 99999997, 99999998, 99999999 are missing") {
          return(paste("replace", var_name, "= \"\" if", var_name, "== \"7777777\" |", var_name, "== \"8888888\" |", var_name, "== \"99999997\" |", var_name, "== \"99999998\" |", var_name, "== \"99999999\""))
        }
      }
    return(NULL)
  })
  
  # Filtering out NULL entries
  missing_lines <- missing_lines[!sapply(missing_lines, is.null)]
  
  # Adding the missing value replacement lines to the Stata script (and a comment before)
  stata_script <- paste(stata_script, "
* Dealing with missing values", sep = "")
  
  stata_script <- paste(stata_script, paste(missing_lines, collapse = "\n"), "\n", sep = "")

  
  
  ###############################################
  #             Adding variable labels          #
  ###############################################
  
  # Extracting the "label" section from the SAS file
  label_section <- str_extract(sas_string, ";\\s*\nlabel[\\s\\S]*?;")
  
  # Splitting the label section into lines
  label_lines <- str_split(label_section, "\n")[[1]]
  
  # Function to generate Stata code for variable labels
  generate_stata_variable_labels <- function(label_lines) {
    stata_code <- ""
    
    for (line in label_lines) {
      # Trim whitespace from the line
      line <- str_trim(line)
      
      # Skipping empty lines or lines that don't match the expected pattern
      if (line == "" || !grepl("=", line)) next
      
      # Extracting variable name and label
      var_name <- toupper(str_extract(line, "^\\w+"))
      var_label <- str_extract(line, "(?<=\").*(?=\")")
      
      # Generating the Stata label code
      stata_code <- paste0(stata_code, "label variable ", var_name, " \"", var_label, "\"\n")
    }
    
    return(stata_code)
  }
  
  # Generating the Stata code for variable labels
  stata_variable_label_code <- generate_stata_variable_labels(label_lines)
  
  # Adding the missing value replacement lines to the Stata script
  stata_script <-  paste0(stata_script, "\n
* Including variable labels \n", stata_variable_label_code)  
  
  ###############################################
  # Adding some value labels to the Stata code  #
  ###############################################

  # Splitting the content into chunks based on ";\n value"
  chunks <- str_split(sas_string, ";\\s*\n\\s*value", simplify = TRUE)
  
  # Re-adding "value" to the beginning of each chunk after the first one
  chunks <- chunks[-1]  # Removing the first chunk since it doesn't contain a "value" section
  chunks <- paste0("value", chunks)
  
  
  generate_stata_value_labels <- function(chunks) {
    stata_code <- ""
    
    for (section in chunks) {
      # Extracting the value name (format name) after "value"
      val_name <- toupper(str_match(section, "value\\s+(\\$?\\w+)")[2])
      
      # Removing $ from label name
      val_name <- gsub("\\$", "", val_name)
      
      # Extracting value pairs via regex
      value_pairs <- str_extract_all(section, "\"[^\"]+\"\\s*=\\s*\"[^\"]+\"|\\w+\\s*=\\s*\"[^\"]+\"")[[1]]
      
      # Starting label define command with replace option
      stata_code <- paste0(stata_code, "label define ", val_name, " ")
      
      # Adding each value-label pair
      for (pair in value_pairs) {
        # Split the pair into the value and the label
        parts <- str_split(pair, "\\s*=\\s*")[[1]]
        
        # Extract the number/code and keep quotes if present
        number <- str_trim(parts[1])
        
        # Extract the label and keep quotes
        label <- str_trim(parts[2])
        
        # Construct the label definition part
        stata_code <- paste0(stata_code, number, " ", label, " ")
      }
      
      # Completing the label define command and add label values command
      variable_name <- gsub("F$", "", val_name)  # Removing F from variable name
      stata_code <- paste0(stata_code, ", replace\n")
      stata_code <- paste0(stata_code, "label values ", variable_name, " ", val_name, "\n\n")
    }
    
    return(stata_code)
  }
  
  # Generate the Stata code, filtering by selected variables
  stata_value_label_code <- generate_stata_value_labels(chunks)

  # Adding the missing value replacement lines to the Stata script
  stata_script <-  paste0(stata_script, "\n
* Including value labels \n", stata_value_label_code)
  
  # Converting variable names to lowercase
  stata_script <- paste(stata_script, "
* Converting variable names to lowercase
ds
foreach var of varlist _all {
    local lower_var = lower(\"`var'\")
    if \"`var'\" != \"`lower_var'\" {
        rename `var' `lower_var'
    }
}

* Saving the data as a .dta file using the same name as the Excel file
save \"output/data/PISA2012/",paste0(output_files[i],".dta"),"\", replace
", sep = "")
  
  # Write the Stata script to a file
  writeLines(stata_script, con = paste0('output/',output_files[i],".do"))
  
  cat(paste0("Complete generating Stata script ", paste0(output_files[i],".do"), "\n"))
}





