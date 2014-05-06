load_perf_data <- function(perf_JSON){

  # load necessary libraries
  library(RJSONIO)

  # reading throug the file one line (record) at a time
  inputFile <- perf_JSON
  con  <- file(inputFile, open = "r")

  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {

    # parse each line into it's date and JSON
    splitLine <- strsplit(oneLine, split="] ")      
    lineDate <- gsub("\\[", "", splitLine[[1]][1])
    lineJSON <- fromJSON(splitLine[[1]][2])

    ### Job Performance ###
    JSON_id <- lineJSON$id # character
    JSON_queued <- lineJSON$queued # numeric
    JSON_start <- lineJSON$start # numeric
    JSON_end <- lineJSON$end # numeric
    JSON_resp <- lineJSON$resp # numeric
    JSON_task <- lineJSON$task_stats # list
 
    ### ### Task Performancs ### ###
    num_tasks <- dim(as.matrix(JSON_task))[1]

    # get the values for the first task
    JSON_task.queued <- as.matrix(JSON_task)[[1]]['queued']
    JSON_task.start <- as.matrix(JSON_task)[[1]]['start']
    JSON_task.end <- as.matrix(JSON_task)[[1]]['end']
    JSON_task.resp <- as.matrix(JSON_task)[[1]]['resp']
    JSON_task.size_infile <- sum(as.matrix(JSON_task)[[1]]['size_infile'][[1]]) # summing what could be multiple values
    JSON_task.size_outfile <- sum(as.matrix(JSON_task)[[1]]['size_outfile'][[1]]) # summing what could be multiple values
     
    # get the values for tasks 2..n
    if( num_tasks > 1 ){
      for (i in 2:num_tasks){
        JSON_task.queued <- append(JSON_task.queued, as.matrix(JSON_task)[[i]]['queued'])
        JSON_task.start <- append(JSON_task.start, as.matrix(JSON_task)[[i]]['start'])
        JSON_task.end <- append(JSON_task.end, as.matrix(JSON_task)[[i]]['end'])
        JSON_task.resp <- append(JSON_task.resp, as.matrix(JSON_task)[[i]]['resp'])
        JSON_task.size_infile <- append(JSON_task.size_infile,  sum(as.matrix(JSON_task)[[i]]['size_infile'][[1]])) # can be a vector
        JSON_task.size_outfile <- append(JSON_task.size_outfile, sum(as.matrix(JSON_task)[[i]]['size_outfile'][[1]])) # can be a vector
      }
    }

    task_stats <- as.matrix(rbind( JSON_task.queued, JSON_task.start, JSON_task.end, JSON_task.resp, JSON_task.size_infile, JSON_task.size_outfile))

    task.column_names <- "task_0"
    if( num_tasks > 1  ){
      for (i in 1:(num_tasks-1) ){
        task.column_names <- append(task.column_names, paste("task_", i, sep="", collapse="")) 
        }
    }  
    dimnames(task_stats)[[2]] <- task.column_names
     
    
    ### ### ### Workunit Performance ### ### ###
                          
    JSON_work <- lineJSON$work_stats # list

    
    # workunit id
    id_task_wu
    # so something like this
    wu_id <- paste(JSON_id, "_", m, "_", n, sep="", collapse="" )

    # but this works
    JSON_work[[1]]
    JSON_work[[10]]
    # but only because one work unit per task? -- no - depends on if you want to be able to map wu's back to tasks
    
    
    JSON_work.
    JSON_work.
    JSON_work.
    JSON_work.
    JSON_work.
    JSON_work.
    JSON_work.
    JSON_work.
    JSON_work.
    JSON_work.
    JSON_work.
    
    
  } 
  close(con)

  
split_line <- noquote(strsplit(my_line, split="\t"))

  
  my_con <- "test_file"
  while ( length(my_line <- readLines(my_con,n = 1, warn = FALSE)) > 0){ 
    print(my_line)
  }

  
while (length(oneLine <- readLines(my_con, n = 1, warn = FALSE)) > 0) {
  print(oneLine)# do stuff
} 



  
my_con  <- file(inputFile="test_file", open = "r")

  while ( length(oneLine <- readLines(my_con,n = 1, warn = FALSE)) > 0) {
    #if ( length( grep("PCO", my_line) ) == 1  ){
    record <<- my_line
    num_records <- num_records + 1
    print(num_records)
    #}
  }
  print(num_values)



  while (length(my_line <- readLines(my_con, n = 1)) > 0) {

    split_line <- noquote(strsplit(my_line, split=" "))
    print(split_line[1])
      
  } 

close(my_con)
  



  
  close(con_1)
  # create object for values
  eigen_values <- matrix("", num_values, 1)
  dimnames(eigen_values)[[1]] <- 1:num_values
  eigen_vectors <- matrix("", num_values, num_values)
  dimnames(eigen_vectors)[[1]] <- 1:num_values
  # read through a second time to populate the R objects
  value_index <- 1
  vector_index <- 1
  open(con_2)
  current.line <- 1
  data_type = "NA"
  while ( length(my_line <- readLines(con_2,n = 1, warn = FALSE)) > 0) {
    if ( length( grep("#", my_line) ) == 1  ){
      if ( length( grep("EIGEN VALUES", my_line) ) == 1  ){
        data_type="eigen_values"
      } else if ( length( grep("EIGEN VECTORS", my_line) ) == 1 ){
        data_type="eigen_vectors"
      }
    }else{
      split_line <- noquote(strsplit(my_line, split="\t"))
      if ( identical(data_type, "eigen_values")==TRUE ){
        dimnames(eigen_values)[[1]][value_index] <- noquote(split_line[[1]][1])
        eigen_values[value_index,1] <- noquote(split_line[[1]][2])       
        value_index <- value_index + 1
      }
      if ( identical(data_type, "eigen_vectors")==TRUE ){
        dimnames(eigen_vectors)[[1]][vector_index] <- noquote(split_line[[1]][1])
        for (i in 2:(num_values+1)){
          eigen_vectors[vector_index, (i-1)] <- as.numeric(noquote(split_line[[1]][i]))
        }
        vector_index <- vector_index + 1
      }
    }
  }
  close(con_2)
