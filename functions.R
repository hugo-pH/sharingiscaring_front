# Function to catch errors in a given submit action
catchy <- function(submit_type, f, args){
  
  # User-experience stuff
  shinyjs::disable(submit_type)
  shinyjs::show("submit_msg")
  shinyjs::hide("error")
  
  # Save the data (show an error message in case of error)
  tryCatch({
    f(args)
    # update.current.orders(orderformData())
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  },
  error = function(err) {
    shinyjs::html("error_msg", err$message)
    shinyjs::show(id = "error", anim = TRUE, animType = "fade")
  },
  finally = {
    shinyjs::enable(submit_type)
    shinyjs::hide("submit_msg")
  })
}







# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}


# load.orders.data  <- function(con, orders_table) {
#   # browser()
#   d_orders <- tbl(con, orders_table) %>% as_data_frame()
#   return(d_orders)
# }
# 
# 

pull.data.from.db <- function(sqlite_path, db_table){
  pool <- dbPool(RSQLite::SQLite(), dbname = sqlite_path)
  d_db <- tbl(pool, db_table) %>% as_data_frame()
  poolClose(pool)
  return(d_db)
}

# load.purchase.data <- function(con, purchases_table) {
#   d_purchases <- tbl(con, purchases_table) %>% as_data_frame()
#   return(d_purchases)
# }



update.current.orders <- function(arg.list) {
  # browser()
  # get present indexes
  db_data <- pull.data.from.db(sqlite_path = arg.list$sqlite_path,
                              db_table = arg.list$orders_table)
  # if(length(db_data$idx > 0)){
  # new_idx <- max(db_data$idx) + 1 
  # }else{
  #   new_idx <- 0
  # }
  pool <- dbPool(RSQLite::SQLite(), dbname = arg.list$sqlite_path)
  # db_insert_into(pool, arg.list$orders_table, arg.list$data %>% mutate(idx = new_idx) , temporary = F)
  db_insert_into(pool, arg.list$orders_table, arg.list$data, temporary = F)
  poolClose(pool)
}

remove.orders <- function(arg.list) {
  db.responses.dir <- arg.list$db.responses.dir
  data  <- arg.list$data
  drop.folder = arg.list$drop.folder
  orders.file.name = arg.list$orders.file.name
  purchase.file.name = arg.list$purchase.file.name
  id.rm = arg.list$id
  
  data.updated <- data %>% 
    filter(id != id.rm)
  # filter(!(date == args$date & name == args$name & item == args$item))
  
  filePath <- file.path(tempdir(), orders.file.name)
  write.csv(data.updated, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = drop.folder)
}


update.purchase.answers <- function(arg.list) {
   # browser()
  browser()
  db_purchases <- pull.data.from.db(sqlite_path = arg.list$sqlite_path,
                              db_table = arg.list$purchases_table)
  # if(length(db_purchases$idx > 0)){
  #   new_idx <- max(db_purchases$idx) + 1 
  # }else{
  #   new_idx <- 0
  # }
  pool <- dbPool(RSQLite::SQLite(), dbname = arg.list$sqlite_path)
  # db_insert_into(pool, arg.list$purchases_table, arg.list$data %>% mutate(idx = new_idx), temporary = F)
  db_insert_into(pool, arg.list$purchases_table, arg.list$data, temporary = F)
  # poolClose(pool)
  
  
  print("loaded second time")
  
  if(arg.list$new_item == FALSE){
    
    
    db_orders <- pull.data.from.db(sqlite_path = arg.list$sqlite_path,
                                      db_table = arg.list$orders_table)
    item <-  arg.list$data %>% pull(item)
    # item  = "h"
    # time <- db_orders %>% filter(item == item) %>% pull(item)
    query<- dbplyr::build_sql("UPDATE orders SET purchased =" , "True", " WHERE item = ", item)
    conn <-  poolCheckout(pool)
    pool::dbSendQuery( conn, query)
    poolClose(pool)
  }
}



remove.entry <- function(arg.list) {
  # browser()
  pool <- dbPool(RSQLite::SQLite(), dbname = arg.list$sqlite_path)
  query <- dbplyr::build_sql("DELETE FROM ", sql(arg.list$table), " WHERE timestamp = " ,arg.list$id)
  conn <-  poolCheckout(pool)
  pool::dbSendQuery( conn, query)
  poolClose(pool)

}



render.tables <- function(purdata){
  # browser()
  df <- purdata %>% 
    mutate(timestamp = ymd_hms(timestamp)) %>%
    arrange(desc(timestamp)) %>% 
    select(-year, -month, -day) %>% 
    mutate(timestamp = as.character(timestamp)) 

  DT::renderDataTable(
    d_orders <- df,
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  )
}
