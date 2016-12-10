dfsEnv <- globalenv()
dfsEnv$adj <- c("1,2","2,1","2,3","3,2","2,4","4,2","4,33",
                "33,4","4,5","5,4","5,14","14,5","5,13",
                "13,5","4,7","7,4","3,6","6,3","6,7",
                "7,6","6,9","9,6","8,9","9,8","9,10",
                "10,9","10,11","11,10","7,11","11,7","11,12",
                "12,11","12,13","13,12","14,15","15,14","15,18",
                "18,15","15,24","24,15","24,12","12,24","12,16",
                "16,12","17,16","16,17","17,18","18,17","10,17",
                "17,10","9,20","20,9","20,18","18,20","18,19",
                "19,18","19,16","16,19","19,23","23,19","24,23",
                "23,24","20,21","21,20","21,22","22,21","22,23",
                "23,22","24,29","29,24","24,25","25,24","25,26",
                "26,25","14,26","26,14","26,27","27,26","27,28",
                "28,27","25,28","28,25","28,29","29,28","29,30",
                "30,29","30,31","31,30","31,22","22,31","21,31",
                "31,21","30,34","34,30","34,30","30,34","30,32","32,30")
dfsEnv$srt <- sort(x = dfsEnv$adj, decreasing = FALSE)
dfsEnv$splt <- strsplit(x = dfsEnv$srt, split = ",")
dfsEnv$vis <- hash::hash()
street_nodes <- 34
for (i in 1:street_nodes) {
  hash::.set(dfsEnv$vis, keys=i, values=FALSE)
}
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

dfs("1", "4")

print(dfsEnv$allPaths)
