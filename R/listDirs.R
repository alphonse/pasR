# Taken verbatim from: http://stackoverflow.com/questions/4749783/how-to-obtain-a-list-of-directories-within-a-directory-like-list-files-but-i
# User Joshua Ulrich
# Name changed from list.dirs to listDirs

listDirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                     full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names)) return(dirs)
  else  return(basename(dirs))
}