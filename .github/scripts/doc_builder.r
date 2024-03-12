# Taken from https://stackoverflow.com/a/4749909 and slightly edited. Thanks!
list_dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
  full.names=FALSE, ignore.case=FALSE) {

  all <- list.files(path, pattern, all.dirs,
           full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]

  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}

cat("R process started.\n")
cat("Change working directory to documentation directory\n")
setwd("documentation")

cat("Creating the directory list\n")
dirlist <- list_dirs(path="..", pattern=".R", ignore.case = TRUE, full.names = TRUE)

cat("Getting a list of R scripts from the algorithm directories.\n")
scriptlist <- lapply(dirlist, list.files, ".R", ignore.case = TRUE, full.names = TRUE)
cat("Removing from the list empty directories.\n")
scriptlist <- scriptlist[!sapply(scriptlist, identical, character(0))]
print(unlist(scriptlist))

cat("Compiling documentation from scripts.\n")
invisible(lapply(unlist(scriptlist), function(x) tryCatch(knitr::spin(x),
    error = function(e) message("Error compiling: ", e))))

cat("R process done.\n")
