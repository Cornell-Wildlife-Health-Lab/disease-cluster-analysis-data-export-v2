#_______________________________________________________________________________
#
# DISEASE CLUSTER ANALYSIS DATA EXPORT
# 
# AUTHOR: Georgianna Silveira
# ERROR HANDLING: Brenda Hanley
# MODEL LOGGING: Brenda Hanley
# QAQC: Brenda Hanley
#
# Location: Cornell Wildlife Health Laboratory
# License: MIT
# 
# This code formats data from the CWD Data Warehouse for upload 
# into the external SaTScan software (https://www.satscan.org/),
# Version 10.2.4. SaTScan™ is a trademark of Martin Kulldorff. The 
# SaTScan™ software was developed under the joint auspices of (i) 
# Martin Kulldorff, (ii) the National Cancer Institute, and (iii) 
# the New York City Department of Health and Mental Hygiene. 
#
# This code is adapted from code originally written by Cara Them.
#              
# This code was written under:
# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Copyright (C) 2024 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64
#
#_______________________________________________________________________________

# Load packages.
  library(tidyverse) # Version 4.4.0

# Reusable Functions.

add_item_to_json_array=function(file_path, new_item) {
    # This is a bespoke function that adds a string representing a JavaScript
    # Object to the attachments.json file containing an array listing the model
    # outputs. Although this function has error handling for a missing file and
    # improperly formed file, the existence of the file and a list enclosed in
    # brackets in that file are expected.

    # Check if the file exists.
    if (!file.exists(file_path)) {
        # Write to error log and exit script with an error.
        line=("ERROR")
        write(html_tag_line(line, "h4"),file=model_log_filepath,append=TRUE)
        line=paste0("File '", file_path, "' not found.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
        quit(status=1)}

    # Read the file content.
    file_content=readChar(file_path, file.info(file_path)$size)

    # Check if the file is empty.
    if (nchar(file_content) == 0) {
        line="ERROR"
        write(html_tag_line(line, "h4"),file=model_log_filepath,append=TRUE)
        line=paste0("File '", file_path, "' is empty.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
        quit(status=1)}

    # Remove the last closing bracket.
    file_content=substr(file_content, 1, nchar(file_content) - 1)

    # Create a function for adding double quotes around text.
    double_quote=function(x) {paste0('"', x, '"')}

    # Create the new item as a JSON string using shQuote with double_quote.
    new_item_json=paste0(
        "{",
        paste(
            sapply(names(new_item), double_quote),
            sapply(as.character(new_item), double_quote),
            sep=":", collapse=","
        ),
        "}"
    )

    # Add comma, new item, and closing bracket to the file content
    file_content=paste0(file_content, ",", new_item_json, "]")

    # Write the updated data back to the file
    writeLines(file_content, file_path, sep="")
}

html_tag_line <- function(text, tag = "p") {
  # Given a string and a simple string representing an html tag, this
  # function surrounds the text with the tag.
  paste0("<", tag, ">", text, "</", tag, ">")
}

# Model log file started with Python data processing script. 
    model_log_filepath=file.path("", "data", "attachments", "info.html")

# Continue the log started with the Python script. 
    line='Model Execution'
    write(html_tag_line(line, "h3"),file=model_log_filepath,append=TRUE)

# Info about Required Data.
  # There are two required files that will always be imported:
  # 1. A .csv file with user-selected parameters.
  # 2. A .csv file with the animals sampled that match the user inputs.

# Read in the (Required) Parameters file. 
  params_filepath=file.path("/", "data", "params.csv")
  Params=readr::read_csv(params_filepath)
  # Note: The params.csv has to exist b/c Python generated it and b/c model has 
  # to create it. Therefore, this error handling is in the python code and this 
  # R script will not run if it does not exist. 
  
# Read in (Required) Samples file. 
  Sample_filepath=file.path("", "data", "sample.csv")
  Sample=readr::read_csv(Sample_filepath) 
  # Note: The sample.csv has to exist b/c Python generated it 
  # and b/c model has to create it. Therefore, this error handling is in the 
  # python code and this R script will not run if it does not exist.

# Global cleaning of the Samples data. 

  # If records do not exist at all within the Sample data frame, terminate the script. 
  SampleDim=as.numeric(nrow(Sample))
  
      # If sample data is null, then terminate the script. 
      if (SampleDim==0){
      line="We're sorry. Sample records do not exist for the species and season-year that you selected."
      write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
      line="Please return to the CWD Data Warehouse and select a different species or season-year."
      write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
      # Quit the session.
      quit(status=70)
      } # End if sample records do not exist.
  
      # If sample data exists, continue. 
  
  # Remove the records without season-year. 
  Sample1=Sample[!is.na(Sample$season_year),]
  Sample1Dim=as.numeric(nrow(Sample1))
  
      # If season years do not exist.
      if (Sample1Dim==0){
      line="We're sorry. The sample data that you selected does not contain season-years."
      write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
      line="Please return to the CWD Data Warehouse and select a different set of sample data."
      write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
      # Quit the session.
      quit(status=70)
      } # End if season years do not exist.
  
      # If season years exist, reformat to first year listed.
      if (Sample1Dim>0){
      Sample1$season_year=substr(Sample1$season_year,1,4)%>%as.numeric
      } # End if season years exist.
  
  # Remove records without any test result. 
  Sample2=Sample1[!is.na(Sample1$result),]
  Sample2Dim=as.numeric(nrow(Sample2))
  
      # If results do not exist, terminate the code. 
      if (Sample2Dim==0){
      line="We're sorry. The sample data that you selected does not contain any test results."
      write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
      line="Please return to the CWD Data Warehouse and select a different set of test data."
      write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
      # Quit the session.
      quit(status=70)
      } # End if results do not exist.
  
      # If records with test result exist, proceed, but don't tell a user anything 
      # yet because they picked a species, and we don't know yet if records for that species exist. 
  
  # Retain Samples only have Detected or Not Detected. 
  Sample3=Sample2[Sample2$result=="Detected"|Sample2$result=="Not Detected",]
  Sample3Dim=as.numeric(nrow(Sample3))
  
      # If samples do not have detected or not detected, terminate the code.
      if (Sample3Dim==0){
      line="We're sorry. Sample records with definitive test result need to exist to use the scan tool."
      write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
      line="Please return to the CWD Data Warehouse and select a different set of sample data."
      write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
      # Quit the session.
      quit(status=70)   
      } # End if samples do not have detected or not detected.
  
      # If samples have detecteds and non-detecteds, then proceed, but don't tell 
      # the user anything yet because they picked a species, and we don't know 
      # yet if records of that species exists.  

  # Remove Samples missing sub-administrative area. 
  Sample4=Sample3[!is.na(Sample3$sub_administrative_area_id),]
  Sample4Dim=as.numeric(nrow(Sample4))
  
      # If SubAdmin areas don't exist, terminate the code. 
      if (Sample4Dim==0){
      line="We're sorry. Sample records with definitive subadministrative area do not exist."
      write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
      line="Please return to the CWD Data Warehouse and select a different set of sample data."
      write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
      # Quit the session.
      quit(status=70) 
      } # End if SubAdmin areas don't exist.
  
      # If SubAdmin areas have data, then proceed, but don't tell the user anything 
      # yet because they picked a species, and we don't know yet if records of that species exists.  
  
  # Create the Tidy Data Using only the species of interest. 
  # Elk.
  if (Params$species=="elk"){
      TidyData=subset(Sample4,Sample4$species=="elk")
      TidyDataDim=as.numeric(nrow(TidyData))
      
          # If elk records do not exist.
          if (TidyDataDim==0){
          line="We're sorry. You selected elk, but elk records with sufficient information for a scan do not exist."
          write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
          line="Please return to the CWD Data Warehouse and select a different species."
          write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
          # Quit the session.
          quit(status=70)   
          } # End if elk records do not exist.
          
          # If elk records exist.
          if (TidyDataDim>0){
          line=paste("Loaded",TidyDataDim,"records depicting CWD status in elk.")
          write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
          } # End if elk records exist.
      
  } # End if elk.
  
  # Fallow deer.
  if (Params$species=="fallow deer"){
    TidyData=subset(Sample4,Sample4$species=="fallow deer")
    TidyDataDim=as.numeric(nrow(TidyData))
    
        # If fallow deer records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected fallow deer, but fallow deer records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if fallow deer records do not exist.
    
        # If fallow deer records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in fallow deer.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if fallow deer records exist.
    
  } # End if fallow deer.
  
  # Moose.
  if (Params$species=="moose"){
    TidyData=subset(Sample4,Sample4$species=="moose")
    TidyDataDim=as.numeric(nrow(TidyData))

        # If moose records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected moose, but moose records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if moose records do not exist.
    
        # If moose records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in moose.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
        } # End if moose records exist. 
    
  } # End if moose.
  
  # Mule deer.
  if (Params$species=="mule deer"){
    TidyData=subset(Sample4,Sample4$species=="mule deer")
    TidyDataDim=as.numeric(nrow(TidyData))

        # If mule deer records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected mule deer, but mule deer records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if mule deer records do not exist.
    
        # If mule deer records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in mule deer.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if mule deer records exist.
    
  } # End if mule deer.
  
  # Muntjac.
  if (Params$species=="muntjac"){
    TidyData=subset(Sample4,Sample4$species=="muntjac")
    TidyDataDim=as.numeric(nrow(TidyData))
    
        # If muntjac records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected muntjac, but muntjac records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if muntjac records do not exist.
    
        # If muntjac records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in muntjac.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if muntjac records exist.
    
  } # End if muntjac.
  
  # Sambar deer.
  if (Params$species=="sambar deer"){
    TidyData=subset(Sample4,Sample4$species=="sambar deer")
    TidyDataDim=as.numeric(nrow(TidyData))
    
        # If sambar deer records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected sambar deer, but sambar deer records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if sambar deer records do not exist.
    
        # If sambar deer records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in sambar deer.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if sambar deer records exist.
    
  } # End if sambar deer.
  
  # Tufted deer.
  if (Params$species=="tufted deer"){
    TidyData=subset(Sample4,Sample4$species=="tufted deer")
    TidyDataDim=as.numeric(nrow(TidyData))

        # If tufted deer records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected tufted deer, but tufted deer records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if tufted deer records do not exist.
    
        # If tufted deer records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in tufted deer.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if tufted deer records exist.
    
  } # End if tufted deer.
  
  # All species.
  if (tolower(Params$species) == "all species"){
    TidyData=Sample4 # Note, we know that at least one record exists in Sample4 due to the check above.
    TidyDataDim=as.numeric(nrow(TidyData))
    
    line=paste("Loaded",TidyDataDim,"records depicting CWD status in any cervid species.")
    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
    
    # Change missing species value to Unknown. 
    TidyData$species[is.na(TidyData$species)]="Unknown"
    
  } # End if all species.
  
  # White-tailed deer.
  if (Params$species=="white-tailed deer"){
    TidyData=subset(Sample4,Sample4$species=="white-tailed deer")
    TidyDataDim=as.numeric(nrow(TidyData))
    
        # If white-tailed deer records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected white-tailed deer, but white-tailed deer records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if white-tailed deer records do not exist.
    
        # If white-tailed deer records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in white-tailed deer.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
        } # End if white-tailed deer records exist. 
    
  } # End if white-tailed deer.
  
  # Axis deer.
  if (Params$species=="axis deer"){
    TidyData=subset(Sample4,Sample4$species=="axis deer")
    TidyDataDim=as.numeric(nrow(TidyData))

        # If axis deer records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected axis deer, but axis deer records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if axis deer records do not exist.
    
        # If axis deer records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in axis deer.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if axis deer records exist. 
    
  } # End if axis deer.
  
  # Black-tailed deer.
  if (Params$species=="black-tailed deer"){
    TidyData=subset(Sample4,Sample4$species=="black-tailed deer")
    TidyDataDim=as.numeric(nrow(TidyData))

        # If black-tailed deer records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected black-tailed deer, but black-tailed deer records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)
        } # End if black-tailed deer records do not exist.
    
        # If black-tailed deer records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in black-tailed deer.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if black-tailed deer records exist. 
    
  } # End if black-tailed deer.
  
  # Caribou.
  if (Params$species=="caribou"){
    TidyData=subset(Sample4,Sample4$species=="caribou")
    TidyDataDim=as.numeric(nrow(TidyData))
    
        # If caribou records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected caribou, but caribou records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if caribou records do not exist.
    
        # If caribou records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in caribou.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if caribou records exist. 
    
  } # End if caribou.
  
  # Eld's deer.
  if (Params$species=="Eld's deer"){
    TidyData=subset(Sample4,Sample4$species=="Eld's deer")
    TidyDataDim=as.numeric(nrow(TidyData))

        # If Eld's deer records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected Eld's deer, but Eld's deer records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if Eld's deer records do not exist.
    
        # If Eld's deer records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in Eld's deer.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if Eld's deer records exist.  
    
  } # End if Eld's deer.
  
  # Hybrid.
  if (Params$species=="hybrid"){
    TidyData=subset(Sample4,Sample4$species=="hybrid")
    TidyDataDim=as.numeric(nrow(TidyData))

        # If hybrid records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected hybrid, but hybrid records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if hybrid records do not exist.
    
        # If hybrid records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in hybrid.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if hybrid records exist.
    
  } # End if hybrid.
  
  # Pere David's deer.
  if (Params$species=="Pere David's deer"){
    TidyData=subset(Sample4,Sample4$species=="Pere David's deer")
    TidyDataDim=as.numeric(nrow(TidyData))

        # If Pere David's deer records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected Pere David's deer, but Pere David's deer records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if Pere David's deer records do not exist.
    
        # If Pere David's deer records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in Pere David's deer.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if Pere David's deer records exist.  
    
  } # End if Pere David's deer.
  
  # Red deer.
  if (Params$species=="red deer"){
    TidyData=subset(Sample4,Sample4$species=="red deer")
    TidyDataDim=as.numeric(nrow(TidyData))

        # If red deer records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected red deer, but red deer records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if red deer records do not exist.
      
        # If red deer records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in red deer.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if red deer records exist.  
      
  } # End if red deer.
  
  # Reindeer deer.
  if (Params$species=="reindeer"){
    TidyData=subset(Sample4,Sample4$species=="reindeer")
    TidyDataDim=as.numeric(nrow(TidyData))

        # If reindeer records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected reindeer, but reindeer records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if reindeer records do not exist.
    
        # If reindeer records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in reindeer.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if reindeer records exist.  
    
  } # End if reindeer.
  
  # Sika deer.
  if (Params$species=="sika deer"){
    TidyData=subset(Sample4,Sample4$species=="sika deer")
    TidyDataDim=as.numeric(nrow(TidyData))

        # If sika deer records do not exist.
        if (TidyDataDim==0){
        line="We're sorry. You selected sika deer, but sika deer records with sufficient information for a scan do not exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        line="Please return to the CWD Data Warehouse and select a different species."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70)   
        } # End if sika deer records do not exist.
    
        # If sika deer records exist.
        if (TidyDataDim>0){
        line=paste("Loaded",TidyDataDim,"records depicting CWD status in sika deer.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if sika deer records exist.  
    
   } # End if sika deer.
  
# At this point in the code. 
# TidyData must exist containing at least one record on the species of interest 
# or else the code would be terminated. 
  
# Format the TidyData. 
    # Change NAs to "No age" for age_group.
    TidyData$age_group[is.na(TidyData$age_group)]="No age"
  
    # Change NAs to "Unknown" for sex.
    TidyData$sex[is.na(TidyData$sex)]="Unknown"

# At this point in the code. 
# Record ID, SubadminID, species, season-year, result (Detected or Not Detected), 
# age group, and sex all have values for the species of interest in the sample data.
# Missing data could include date harvested, latitude (of the animal), 
# longitude (of the animal), or geolocation precision. These incomplete 
# sample records will get cleaved out later if an only if they don't have the 
# data necessary to fit the scan type selected.
    
# Make sure at least one detected and one non-detected appear in the data.
    
    # If samples lack Detecteds.
    Positives=TidyData[TidyData$result=="Detected",]
    PositivesDim=as.numeric(nrow(Positives))
    
        # If data lacks positives.
        if (PositivesDim==0){
        line="We're sorry. The scan requires at least one CWD-positive case."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
        line="Please return to the CWD Data Warehouse and select a different species or season-year."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70) 
        } # End if data lacks positives.
    
        # If positives exist. 
        if (PositivesDim>0){
        line=paste("Loaded",PositivesDim,"CWD-positive records.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
        } # End if positives exist. 
    
    # If samples lack Non-Detecteds.
    Negatives=TidyData[TidyData$result=="Not Detected",]
    NegativesDim=as.numeric(nrow(Negatives))
    
        # If data lacks negatives.
        if (NegativesDim==0){
        line="We're sorry. The scan requires at least one CWD-non detect case."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
        line="Please return to the CWD Data Warehouse and select a different species or season-year."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70) 
        } # End if data lacks negatives.
    
        # If negatives exist. 
        if (NegativesDim>0){
        line=paste("Loaded",NegativesDim,"CWD-non detect records.")
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
        } # End if negatives exist. 
    
        # Add message if both positives and negatives exist.
        if (PositivesDim>0 & NegativesDim>0){
        line="Note: Records wihtout latitude, longitude, geospatial precision, or harvest date will be excluded from the final dataset."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End add message.

# Now we are sure that at least one positive and one negative case exists.
    
# Create a header on the error log.    
    line='Data Processing Output'
    write(html_tag_line(line, "h4"),file=model_log_filepath,append=TRUE)

# Create the Exact Spatial Data Set. 
    # Note: Lat/long and a measure of precision must exist. 
    TidyData1=TidyData[!is.na(TidyData$latitude),]
    TidyData1Dim=as.numeric(nrow(TidyData1))
    
        # If no records exist with lat. 
        if (TidyData1Dim==0){
        SpatialEligibilityExact=0
        line="This sample data is ineligible for a scan because latitudes don't exist."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        } # End if no records exist with lat.
    
        # If some records exist with lat.
        if (TidyData1Dim>0){
        TidyData2=TidyData1[!is.na(TidyData1$longitude),]
        TidyData2Dim=as.numeric(nrow(TidyData2))
            
            # If no records exist with long.
            if (TidyData2Dim==0){
            SpatialEligibilityExact=0
            line="This sample data is ineligible for a scan because longitudes don't exist."
            write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
            } # End if no records exist with long.
            
            # If some records exist with long.
            if (TidyData2Dim>0){
                TidyData3=TidyData2[TidyData2$geolocation_precision=="Exact",]
                TidyData3Dim=as.numeric(nrow(TidyData3))
                
                    # If no precision exists.
                    if (TidyData3Dim==0){
                    SpatialEligibilityExact=0 
                    line="This sample data is ineligible for a scan because geospatial precision does not exist."
                    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
                    } # End if no precision exists.
                
                    # If some records with precision exist.
                    if (TidyData3Dim>0){
                    Spatial_Exact_df=TidyData3
                    SpatialEligibilityExact=1
                    # Don't write anything yet because we still don't know if sufficient harvest dates exist.
                    } # End if precision exists.
                
            } # End if long exists.
        
        } # End if lat exists.

# Create the Temporal Data set. -------------
    # Note: The temporal scan requires exact locations.
    
        # If exact spatial locations are passed on.
        if (SpatialEligibilityExact==1){
        Temporal_df=Spatial_Exact_df
        TemporalEligibility=1
        # Don't write anything yet because we still don't know if sufficient lat, long, precision or harvest dates exist.
        } # If exact spatial data is passed on.
    
        # If no exact spatial data is passed on.
        if (SpatialEligibilityExact==0){
        TemporalEligibility=0
        # Don't write anything yet because we don't know if user picked this scan type. 
        } # End if no exact spatial data is passed on. 
    
# At this point in the code. -------------------
# The user knows whether their data is eligible for the spatial or temporal scan, and if not, then why. 
# However, we don't know that scan type the user has selected yet, so we can't just terminate
# the script if the data is ineligible. 

# A By-Lat/Long spatial data set named Spatial_Exact_df exists and contains records whenever SpatialEligibilityExact=1.
# A temporal data set named Temporal_df exists and contains records whenever TemporalEligibility=1.

# ______________________________________________________________________________
# SCAN TYPE: DISCRETE BERNOULLI - TEMPORAL 
# ______________________________________________________________________________ 
# Model requires SubAdmin ID, cases with exact location, non-cases (control) 
# with exact location, harvest date (date), and lat/long for case, control, 
# and coordinates files.
    
if (Params$scan_type=="Discrete Bernoulli Temporal"){

    # If data is ineligible for temporal analysis. 
    if (TemporalEligibility==0){
    line="This sample data is ineligible for the 'Discrete Bernoulli – Temporal' scan type because exact locations do not exist."
    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
    line="Please return to the CWD Data Warehouse and select a different set of sample data or scan type."
    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
    # Quit the session.
    quit(status=70)  
    } # End if data is ineligible for temporal analysis.  
    
    # If data is eligible for temporal analysis.
    if (TemporalEligibility==1){ 
          
        # Remove records that don't have exact harvest date.
        Data=Temporal_df[!is.na(Temporal_df$date_harvested),]
        DataDim=as.numeric(nrow(Data))
          
            # If harvest dates do not exist.
            if (DataDim==0){
            line="This sample data is ineligible for the 'Discrete Bernoulli – Temporal' scan because harvest dates do not exist."
            write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
            line="Please return to the CWD Data Warehouse and select a different set of sample data or scan type."
            write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
            # Quit the session.
            quit(status=70)
            } # End if harvest dates do not exist.
            
            # If harvest dates contain data.
            if (DataDim>0){
                line="This sample data is eligible for the 'Discrete Bernoulli – Temporal' scan."
                write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
              
                # Format date column as a date.
                Data$date_harvested=as.Date(Data$date_harvested)
                
                # Extract date values needed user output
                start_date=min(Data$date_harvested)
                end_date=max(Data$date_harvested)
                
                # Retain the data frame of interest.
                SaTScan_data=data.frame(
                "Location"=seq(1:DataDim),
                "Season-Year"=Data$season_year,
                "Harvest Date"=Data$date_harvested,
                "Longitude"=Data$longitude,
                "Latitude"=Data$latitude,
                "Result"=Data$result)

                # Replace Detected with 1 and Not Detected with 0.
                SaTScan_data$Result[SaTScan_data$Result=="Detected"]=1
                SaTScan_data$Result[SaTScan_data$Result=="Not Detected"]=0
                
                # Create user input information.
                user_inputs_out=data.frame(
                  "Time_Precision"="Day",
                  "Study_Period_Start"=start_date,
                  "Study_Period_End"=end_date,
                  "Coordinates"="Lat/Long",
                  "Scan_Statistic"="Discrete Bernoulli - Temporal")
                
            } # If harvest dates contains data. 
      
    } # End if data is eligible for temporal analysis.
  
} # End if Discrete Bernoulli - Temporal is selected.

# ______________________________________________________________________________
# SCAN TYPE: DISCRETE BERNOULLI - SPATIAL [EXACT LOCATION] 
# ______________________________________________________________________________
# Model requires SubAdmin ID, cases with exact location, non-cases (control) 
# with exact location, harvest date (date), and lat/long for case, control, 
# and coordinates files.
    
if (Params$scan_type=="Discrete Bernoulli Spatial [Exact Location]"){
  
    # If data is ineligible for spatial analysis. 
    if (SpatialEligibilityExact==0){
    # Already told user that the data is ineligible. 
    line="Please return to the CWD Data Warehouse and select a different set of sample data or scan type."
    # Quit the session.
    quit(status=70)  
    } # End if data is ineligible for spatial analysis.  
  
    # If data is eligible for spatial analysis.
    if (SpatialEligibilityExact==1){ 
      
        # Keep only samples with exact location.
        Data=Spatial_Exact_df[!is.na(Spatial_Exact_df$date_harvested),]
        DataDim=as.numeric(nrow(Data))
        
            # If harvest dates do not exist.
            if (DataDim==0){
            line="This sample data is ineligible for the 'Discrete Bernoulli – Spatial [Exact Location]' scan because harvest dates do not exist."
            write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
            line="Please return to the CWD Data Warehouse and select a different set of sample data or scan type."
            write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
            # Quit the session.
            quit(status=70)
            } # End if harvest dates do not exist.

            # If harvest dates exist.
            if (DataDim>0){
            line="This sample data is eligible for the 'Discrete Bernoulli – Spatial [Exact Location]' scan."
            write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
      
            # Extract date values needed user output.
            start_year=min(Data$season_year)
            end_year=max(Data$season_year)
      
            # Retain important columns of data.
            SaTScan_data=data.frame(
            "Location"=seq(1:DataDim),
            "Season-Year"=Data$season_year,
            "Harvest Date"=Data$date_harvested,
            "Longitude"=Data$longitude,
            "Latitude"=Data$latitude,
            "Result"=Data$result)
        
            # Replace positive with 1 and not detected with 0.
            SaTScan_data$Result[SaTScan_data$Result=="Detected"] <- 1
            SaTScan_data$Result[SaTScan_data$Result=="Not Detected"] <- 0
            
            # Create user input information.
            user_inputs_out=data.frame(
              "Time_Precision"="Year",
              "Study_Period_Start"=start_year,
              "Study_Period_End"=end_year,
              "Coordinates"="Lat/Long",
              "Scan_Statistic"="Discrete Bernoulli - Spatial [Exact Location]")
      
            } # End if harvest dates exist.
      
    } # End if data is eligible for spatial analysis. 
  
} # End if Discrete Bernoulli Spatial – [Exact Location].
# ______________________________________________________________________________
# Create and set working directory for output.
    setwd("/data/attachments") 
    
# Make sure final outputs have at least one positive case.
    FinalPositives=SaTScan_data[SaTScan_data$Result=="1",]
    FinalPositivesDim=as.numeric(nrow(FinalPositives))
    
        # If SaTScan_data lacks positives.
        if (FinalPositivesDim==0){
        line="We're sorry. The scan requires at least one CWD-positive case."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
        line="Please return to the CWD Data Warehouse and select a different species or season-year."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70) 
        } # End if SaTScan_data lacks positives.
    
        # If positives exist in SaTScan_data. 
        if (FinalPositivesDim>0){
          Cases=data.frame(
          "Identifier"=FinalPositives$Location,
          "Number of Cases"=FinalPositives$Result,
          "Year"=FinalPositives$Season.Year,
          "Date"=FinalPositives$Harvest.Date)
        write.csv(Cases, "Case_File.csv", row.names = FALSE)
        } # End if positives exist in SaTScan_data. 
    
# Make sure final outputs have at least one negative case.
    FinalNegatives=SaTScan_data[SaTScan_data$Result=="0",]
    FinalNegativesDim=as.numeric(nrow(FinalNegatives))
    
        # If SaTScan_data lacks negatives.
        if (FinalNegativesDim==0){
        line="We're sorry. The scan requires at least one CWD-non detect case."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
        line="Please return to the CWD Data Warehouse and select a different species or season-year."
        write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
        # Quit the session.
        quit(status=70) 
        } # End if SaTScan_data lacks negatives.
    
        # If negatives exist in SaTScan_data. 
        if (FinalNegativesDim>0){
          Controls=data.frame(
            "Identifier"=FinalNegatives$Location,
            "Number of Controls"=rep(1,FinalNegativesDim),
            "Year"=FinalNegatives$Season.Year,
            "Date"=FinalNegatives$Harvest.Date)
        write.csv(Controls, "Control_File.csv", row.names = FALSE)
        } # End if negatives exist in SaTScan_data. 

# If both positives and negatives exist, then print the remaining files.

    # If both positives and negatives exist.
    if(FinalPositivesDim>0 & FinalNegativesDim>0){
    #log number of positives and negatives in final datasets
    line=paste0("Number of CWD-positive records in final datset: ", FinalPositivesDim)
    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
    line=paste0("Number of CWD-non detect records in final datset: ", FinalNegativesDim)
    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 

    write.csv(SaTScan_data, "All_SaTScan_Data.csv", row.names = FALSE)
    FinalCoordinates=data.frame(
      "Identifier"=SaTScan_data$Location,
      "Latitude"=SaTScan_data$Latitude,
      "Longitude"=SaTScan_data$Longitude)
    write.csv(FinalCoordinates, "Coordinates_File.csv", row.names = FALSE)
    write.csv(user_inputs_out, "SaTScan_user_inputs.csv", row.names = FALSE)
    } # End if both positives and negatives exist.
    
    # If one or both of these don't exist, then the session has already been terminated.