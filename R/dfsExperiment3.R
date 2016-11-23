dfsEnv <- globalenv()
dfsEnv$adj <- c("1,2", "2,3", "3,4", "4,5", "1,3", "3,5")
dfsEnv$srt <- sort(x = dfsEnv$adj, decreasing = FALSE)
dfsEnv$splt <- strsplit(x = dfsEnv$srt, split = ",")
dfsEnv$vis <- hash::hash('1'=FALSE,'2'=FALSE,'3'=FALSE,
                         '4'=FALSE,'5'=FALSE)
dfsEnv$path <- c()
dfsEnv$allPaths <- list()

dfs <- function (loc, des) {
  result <- c()

  if (loc == des) {
    pathSize = length(dfsEnv$path)
    for (i in 0:pathSize) {
      result <- append(x = result, values = dfsEnv$path[i])
    }
    result <- append(x = result, values = des)

    lastVectorOfAll <- (length(dfsEnv$allPaths) + 1)
    dfsEnv$allPaths[[lastVectorOfAll]] <- result
    return()
  }

  if (hash::values(dfsEnv$vis, loc)) {
    return()
  }

  hash::values(dfsEnv$vis, keys=loc) <- TRUE

  for (i in 1:length(dfsEnv$splt)) {
    first <- dfsEnv$splt[[i]][1]
    second <- dfsEnv$splt[[i]][2]
    if (first == loc) {
      dfsEnv$path <- append(x = dfsEnv$path, values = first)
      dfs(second, des)
      # lastElem <- length(dfsEnv$path)
      # dfsEnv$path[[lastElem]] <- NULL
      dfsEnv$path <- head(dfsEnv$path, -1)
    }
  }
}

dfs("1", "5")

print(dfsEnv$allPaths)
