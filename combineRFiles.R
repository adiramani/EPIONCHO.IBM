combineRFiles <- function(folderName="R/") {
  sink("all_funcs_combined.R")


  filePrefix <- function(fileName="") {
    return(paste(folderName, fileName, sep=""))
  }

  i <- 1
  files <- filePrefix(list.files(filePrefix()))
  files <- c(files, "runModelRCS.R")

  for (file in files) {
    current_file = readLines(file)
    cat("\n\n#### Current file:",file,"\n\n")
    cat(current_file, sep ="\n")
  }

  sink()

}

