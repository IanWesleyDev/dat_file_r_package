#' Splits all .dat files in folder and moves to destination
#'
#' Splits all .dat files in the specified folder, by Employer ID. Spilt files are saved into separate .dat
#' files in the specified destination folder.  It also runs a the validation checks describe in
#' \code{\link{validate.dat.header}}.
#'
#' @inheritParams single.dat.split
#' @param SourceFolder The folder with .dat files needing splitting
#' @param Validate Boolean indicating if validation should be run
#'
#' @examples
#' dats.split("c:/example/source/", "c:/example/destination")
dats.split <- function(SourceFolder, DestinationFolder, Validate = TRUE){

  # get list of .dat | .txt files from source folder
  files <- list.files(SourceFolder, full.names = TRUE)
  files <- tolower(files)
  files <- files[tolower(file_ext(files)) %in% c("dat","txt")]

  # lapply a function that splits individual dat files to the list
  lapply(files, function(x) single.dat.split(x, DestinationFolder))
  if(Validate){
    result <- ValidateMove(SourceFolder, DestinationFolder)
    cat(result)
  } else {
    cat("Validation not run.")
  }
}


#' Reads survey and header data from .dat file with one Employer Id
#'
#' Returns an R object, of type \code{\link{CtrDat-class}}, with survey and header data from a .dat file.
#' It also preforms validation as described in \code{\link{validate.dat.header}}
#'
#' @param Path File path to a .dat file
#' @param RemoveFiller A boolean indicating if the filler fields should be removed
#'
#' @return An S4 object, \code{\link{CtrDat-class}}, with the Employer Id, information from the header of the dat file,
#' the survey data, a logical value indicating if filler has been removed.
#' @examples
#' read.dat("c:/example/datfile.dat")
read.dat <- function(Path, RemoveFiller = TRUE){
  if(!(tolower(file_ext(Path)) %in% c("dat","txt"))){
    stop(sprintf("The file %s is not a .dat or .txt file.", Path))
    geterrmessage()
  }

  # Read dat file into S4 class see functions for details
  dat <- new("CtrDat-class")

  dat@Header        <- read.dat.headers(Path = Path, RemoveFiller = RemoveFiller, OnlyFirstHeader = TRUE)
  dat@Validate      <- validate.dat.header(dat@Header)
  dat@SurveyData    <- read.dat.survey(Path = Path, RemoveFiller = RemoveFiller)
  dat@FillerRemoved <- RemoveFiller
  dat@EmployerID    <- dat@Header$EmployerID

  dat
}


#' Reads the headers of a .dat file
#'
#' LOWER LEVEL FUNCTION - Not meant to be called on its own.\cr \cr
#' It returns a data frame with .dat data formatted for headers.  All data,
#' including survey data, will be in the data frame. This may be used to
#' pull a list of all employer IDs in a .dat file.
#'
#' @inheritParams read.dat
#' @param OnlyFirstHeader A boolean indicating if only the first line of the header should be returned.  Useful for
#' working with .dat files with only one employer ID.
#'
#' @return A data frame with information formatted using the header schema from a .dat file
#' @examples
#' read.dat.header("c:/example/example/datfile.dat")
read.dat.headers <- function(Path, RemoveFiller = TRUE, OnlyFirstHeader = FALSE){

  #Chech if it is a .dat or txt
  if(!(tolower(file_ext(Path)) %in% c("dat","txt"))){
    stop(sprintf("The file %s is not a .dat or .txt file.", Path))
    geterrmessage()
  }

  #use read.fwf to read fixed width file
  Header <- read.fwf(Path,
                     skip = 0,
                     n = -1,                                          #-1 means read everything
                     widths = HeaderSchema$length,
                     col.names = HeaderSchema$Field.name,
                     comment.char= "",                                #default value is #, which appears in .dat files
                     stringsAsFactors = FALSE,
                     colClasses = as.vector(HeaderSchema$field.type)) #Careful reading .dat files with anything other than character lots of leading 0 and spaces can be removed.
  if(RemoveFiller){
    Header <- Header[, HeaderSchema$HasData]
  }
  if(OnlyFirstHeader){
    Header <- Header[1, ]
  }
  Header
}

#' Get list of employers in .dat file
#'
#' LOWER LEVEL FUNCTION - Not meant to be called on its own. \cr \cr
#' Returns a list of employers in a .dat file. This is a helper function around \code{\link{read.dat.headers}}
#'
#' @inheritParams read.dat
#'
#' @return A list of employer IDs in a .dat file
#' @examples
#' read.dat.getEmployers("c:/example/Datfile.dat")
read.dat.getEmployers <- function(Path) {
  headers <- read.dat.headers(Path)
  EmployerIds <- unique(headers$E_ID_again)
  EmployerIds
}


#' Validates header data
#'
#' LOWER LEVEL FUNCTION - Not meant to be called on its own.\cr \cr
#' Preforms various checks on the header to assure the .dat file meets specifications  The following checks are preformed:\cr
#' \itemize{
#'   \item Checks that there is only one valid header in the .dat file
#'   \item The header flag is equal to 'Y'
#'   \item Returned surveys are less than or equal to the number distributed
#'   \item Returned surveys are less than or equal the number of total employees
#'   \item Distributed surveys are less than or equalt to the number of total employees - exempted employees
#' }
#'
#' @param Header a valid header returned from the \code{\link{read.dat.header}} function
#'
#' @return Returns TRUE if it passes validation and an error message if it does not
#' @examples
#' validate.dat.header(read.dat.header("c:/example/example/datfile.dat"))
validate.dat.header <- function(Header){
  df <- Header
  attach(df)

  # Validation checks
    if(length(unique(E_ID_again)) > 1){
      valid <- "Validation Failed. More than one employeer in the .dat file"
    } else if(Header_flag[1]      == "Y" &
       as.numeric(Returned[1])    <= as.numeric(Distributed[1]) &
       as.numeric(Returned[1])    <= as.numeric(Total_emp[1]) &
       as.numeric(Distributed[1]) <= (as.numeric(Total_emp[1]) - as.numeric(Exemp_Emp[1]))
    ){
      valid <- as.character("TRUE")
    } else (

      #constructing an error message
      valid <- paste("",
                      "---------------------",
                      EmployerID,
                      "Validation Failed.",
                      sprintf("Header == 'Y': %s", Header_flag == "Y"),
                      sprintf("Returned <= Distributed: %s", as.numeric(Returned[1]) <= as.numeric(Distributed[1])),
                      sprintf("Returned <= Total_emp: %s", as.numeric(Returned[1]) <= as.numeric(Total_emp[1])),
                      sprintf("Distributed <= (Total_emp - Exemp_Emp): %s", as.numeric(Distributed[1]) <= (as.numeric(Total_emp[1]) - as.numeric(Exemp_Emp[1]))),
                      sep = "\n")
    )
  detach(df)
  valid
}


#' Reads survey data from .dat file
#'
#' LOWER LEVEL FUNCTION - Not meant to be called on its own.\cr \cr
#' Returns a data frame with .dat data formated using the survey schema
#'
#' @inheritParams read.dat
#'
#' @return A data frame with information from the header of the .dat file
#' @examples
#' read.dat.survey("c:/example/example/datfile.dat")
read.dat.survey <- function(Path, RemoveFiller = TRUE){

  #Chech if it is a .dat or txt
  if(!(tolower(file_ext(Path)) %in% c("dat","txt"))){
    stop(sprintf("The file %s is not a .dat or .txt file.", Path))
    geterrmessage()
  }

  #use read.fwf to read fixed width file
  SurveyData <- read.fwf(Path,
                         skip = 1,
                         n = -1,
                         widths = SurveySchema$length,
                         col.names = SurveySchema$Field.name,
                         comment.char= "",
                         stringsAsFactors = FALSE,
                         colClasses = as.vector(SurveySchema$field.type))
  if(RemoveFiller){
    SurveyData <- SurveyData[,SurveySchema$HasData]
  }

  SurveyData
}


#' Splits one .dat file by employer ID
#'
#' Splits one .dat file by Employer ID into separate .dat files.  If there is only one employer in
#' the .dat file, it will be copied to the destination folder.
#'
#' @inheritParams read.dat
#' @param DestinationFolder The folder where split .dat files should be written to
#'
#' @examples
#' single.dat.split("c:/example/datfile.dat", "c:/example/split/")
single.dat.split<- function(Path, DestinationFolder){

  #function to write .dat files only used here
  write.dat <- function(df) {

    #Colapse data frame into single field to be written out
    df$writeLine <- apply(df, 1, paste, collapse="")

    #Historical save name format "YYYY-MM-DD EmployerID.Dat"
    SaveName <- file.path(DestinationFolder, paste0(as.Date(df$Date_cond[1], format="%m%d%Y"),  " ", df$EmployerID[1], ".dat"))
    myCon <- file(SaveName, "w")
    writeLines(df$writeLine, con = myCon)
    close(myCon)
  }

  #Read .dat with header schema
  dat.df <- read.dat.headers(Path, RemoveFiller = FALSE)

  # if there are more one employeer, split the data frame and apply to each subset else write once
  if(length(unique(dat.df$E_ID_again)) > 1) {
    lapply(split(dat.df, dat.df$E_ID_again), write.dat)
  } else {
    write.dat(dat.df)
  }

  return("Success")
}


#' Validates that dats.split moved all files and headers are valid
#'
#' LOWER LEVEL FUNCTION - Not meant to be called on its own. \cr \cr
#' Calls \code{\link{validate.dat.header}} on each file in the destination folder to assure all validation checks are passed.
#' It also confimrs that each Employer Id was moved.
#'
#' @inheritParams dats.split
#'
#' @examples
#' ValidateMove("c:/example/source/", "c:/example/destination/")
ValidateMove <- function(SourceFolder, DestinationFolder) {

  # Get list of all Employer IDs in the source folder
  SourceFiles <- list.files(SourceFolder, full.names = TRUE)
  SourceFiles <- tolower(SourceFiles)
  SourceFiles <- SourceFiles[tolower(file_ext(SourceFiles)) %in% c("dat","txt")]
  SourceEmployerIds <- do.call(c, lapply(SourceFiles, read.dat.getEmployers))
  Source.df <- data.frame(EmployerID = SourceEmployerIds)

  # get all Headers from destination folder
  DestinationFiles <- list.files(DestinationFolder, full.names = TRUE)
  DestinationFiles <- tolower(DestinationFiles)
  DestinationFiles <- DestinationFiles[tolower(file_ext(DestinationFiles)) %in% c("dat","txt")]
  DestinationHeaders <-  do.call(rbind, lapply(DestinationFiles, function(x) read.dat.headers(x, OnlyFirstHeader = TRUE)))

  # Run Validation on all headers from desintation folder
  ValidHeader <- do.call(c, lapply(split(DestinationHeaders, DestinationHeaders$E_ID_again), validate.dat.header))
  ValidHeader.df <- data.frame(EmployerID = names(ValidHeader),
                               ValidationPassed = ValidHeader)

  # Join Sources Employer list to validation results
  Validation <- Source.df %>% left_join(ValidHeader.df, by = "EmployerID")

  # Generate validation message
  ValidCount <- nrow(Validation %>% filter(ValidationPassed == TRUE))
  Total <- nrow(Validation)
  if(Total == ValidCount){
    Output <- sprintf("Success!  The source folder had %i Employer IDs.  Each of moved to the destination folder and passed all validation checks", Total)
  } else(
    Output <- paste(sprintf("Warning! The source folder had %i Employer IDs.  Only %i passed validation. Files failing validation were still moved. Validation errors:", Total, ValidCount),
                    paste(Validation$ValidationPassed[!(Validation$ValidationPassed == TRUE)]))
  )

  # Return validation message
  Output
}

#' An S4 class to represent a .dat file
#'
#' @slot EmployerID The Employer ID for all data in the object
#' @slot Validate True if it passes validation, error message if it does not
#' @slot Header A one row data frame with header data
#' @slot SurveyData A data frame containing the survey data
#' @slot FillerRemoved A logical value indicating if filler has been removed
#' @name CtrDat-class
#' @rdname CtrDat-class
setClass("CtrDat-class",
         slots = list(EmployerID    = "character",
                      Header        = "data.frame",
                      SurveyData    = "data.frame",
                      Validate      = "character",
                      FillerRemoved = "logical"))


#' Header Schema
#'
#' This data frame contains the schema for the header of fix width file formated .dat files. It contains:
#' \itemize{
#'   \item Field.name: The name of the field in the .dat file
#'   \item field.type: The R data type used to read in the field (set to character to account for leading zeros and spaces)
#'   \item length: The fixed character length of the field
#'   \item start: The starting character position of the field
#'   \item end: The ending character position of the field
#'   \item description: The field description
#'   \item HasData: A flag indicating if the field contains data or is filler
#' }
#'
#' @docType data
#' @keywords datasets
#' @name HeaderSchema
#' @usage data(HeaderSchema)
#' @format A data frame with 28 rows and 7 variables
NULL

#' Survey Schema
#'
#' This data frame contains the schema for the suvey of fix width file formated .dat files. It contains:
#' \itemize{
#'   \item Field.name: The name of the field in the .dat file
#'   \item field.type: The R data type used to read in the field (set to character to account for leading zeros and spaces)
#'   \item length: The fixed character length of the field
#'   \item start: The starting character position of the field
#'   \item end: The ending character position of the field
#'   \item description: The field description
#'   \item HasData: A flag indicating if the field contains data or is filler
#' }
#'
#' @docType data
#' @keywords datasets
#' @name SurveySchema
#' @usage data(SurveySchema)
#' @format A data frame with 67 rows and 7 variables
NULL
