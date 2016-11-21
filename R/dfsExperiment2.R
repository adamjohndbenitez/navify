dfsEnv <- globalenv()
dfsEnv$adj <- hash::hash('1'=2,'2'=3,'3'=4,'4'=5,'1'=3,'3'=5)
dfsEnv$vis <- hash::hash('1'=FALSE,'2'=FALSE,'3'=FALSE,
                         '4'=FALSE,'5'=FALSE)
dfsEnv$path <- c()
dfsEnv$allPaths <- list()

dfs <- function (loc, des) {
  result <- c()

  if (loc == des) {
    pathSize = length(dfsEnv$path)
    sortPath <- sort(x = dfsEnv$path, decreasing = FALSE)
    for (i in 0:pathSize) {
      result <- append(x = result, values = sortPath[i])
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

  for (i in ls(dfsEnv$adj)) {
    if (i == loc) {
      dfsEnv$path <- append(x = dfsEnv$path, values = i)
      valAdj <- hash::values(x = dfsEnv$adj, i)
      dfs(valAdj, des)
      dfsEnv$path[-(length(dfsEnv$path) - 1)]
    }
  }
}

dfs(1, 5)

print(dfsEnv$allPaths)
