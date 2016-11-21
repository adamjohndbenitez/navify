dfsEnv <- new.env()
dfsEnv$adj <- hashmap::hashmap(c(1, 2, 3, 4, 5), c(2, 3, 4, 5, 6))
dfsEnv$vis <- hashmap::hashmap(c(1, 2, 3, 4, 5), c(FALSE, FALSE, FALSE, FALSE, FALSE))
dfsEnv$path <- c()
dfsEnv$allPaths <- list()

dfs <- function(loc, des) {

  # when destination is same as
  if (loc == des) {
    pathSize = length(dfsEnv$path)
    for (i in 0:pathSize) {
      result <- append(x = dfsEnv$path, values = path[i])
    }
    lastVectorOfAll <- length(dfsEnv$allPaths + 1)
    dfsEnv$allPaths[[lastVectorOfAll]] <- result
    return
  }

  if (dfsEnv$vis[[loc]]) {
    return
  }

  dfsEnv$vis[[loc]] <- TRUE

  for (key in ls(dfsEnv$adj)) {
    if (key == loc) {
      dfsEnv$path <- append(x = dfsEnv$path, values = key)
      # adj[[key]] access value of the key
      dfs(dfsEnv$adj[[key]], des)
      dfsEnv$path <- dfsEnv$path[-(length(dfsEnv$path)-1)]
    }
  }
}

dfs(1, 6)
print(dfsEnv$allPaths)

