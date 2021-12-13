inject_data <- function(Outputs) {
 
  # Temp
  names <- paste0("(", paste0(c('site_fk', 'year', 'day', 'temp'), collapse = ", "), ")")
  
  xx <- cbind(i,Outputs[,c(1,2,3)])
  
  values <- paste0(apply(xx, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  values <- stringr::str_replace_all(values, "'NA'", "NULL")
  
  cmd <- paste("INSERT INTO tbl_temp", names, " VALUES ", values)
  dbSendQuery(con, cmd, as.is=TRUE)
  
  # Precip
  names <- paste0("(", paste0(c('site_fk', 'year', 'day', 'ppt'), collapse = ", "), ")")
  
  xx <- cbind(i,Outputs[,c(1,2,4)])
  
  values <- paste0(apply(xx, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  values <- stringr::str_replace_all(values, "'NA'", "NULL")
  
  cmd <- paste("INSERT INTO tbl_ppt", names, " VALUES ", values)
  dbSendQuery(con, cmd, as.is=TRUE)
  
  # VWC ---------------------------------------------------------------------------------------------------------------
  names1 <- sort(gsub('VWC.', '', grep('VWC', names(Outputs), value = TRUE)))
  
  if(length(names1) == 1)  {
    xx <- cbind(i,Outputs[,c(1,2,5)])
    names <- paste0("(", paste0(c('site_fk', 'year', 'day', 'shallow'), collapse = ", "), ")")
  }
  if(length(names1) == 2) {
    xx <- cbind(i,Outputs[,c(1,2,6,5)])
    names <- paste0("(", paste0(c('site_fk', 'year', 'day', 'shallow', 'intermediate'), collapse = ", "), ")")
  } 
  
  if(length(names1) == 3)  {
    xx <- cbind(i,Outputs[,c(1,2,7,6,5)])
    names <- paste0("(", paste0(c('site_fk', 'year', 'day', 'shallow', 'intermediate', 'deep'), collapse = ", "), ")")
  }
  
  values <- paste0(apply(xx, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  values <- stringr::str_replace_all(values, "'NA'", "NULL")
  
  cmd <- paste("INSERT INTO tbl_vwc", names, " VALUES ", values)
  dbSendQuery(con, cmd, as.is=TRUE)
}