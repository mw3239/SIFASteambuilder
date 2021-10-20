library(rvest)

current_db_jp <- DB_JP_CUR
current_db_en <- DB_EN_CUR

url_jp <- DB_JP_DIR
url_en <- DB_EN_DIR

download_dbs <- function(current_db, url, jp = TRUE) {
  directory_list <- read_html(url) %>%
    html_nodes(xpath = '//*[@class="link"]') %>%
    html_text() %>%
    magrittr::extract(2:length(.))
  
  updated <- ifelse(current_db == directory_list[1], TRUE, FALSE)
  i <- 1
  
  while(!updated) {
    current_dir <- str_c(url, directory_list[i]) %>%
      read_html() %>%
      html_nodes(xpath = '//*[@class="link"]') %>%
      html_text() %>%
      magrittr::extract(2:length(.))
    
    if("masterdata.db" %in% current_dir) {
      if(current_db == directory_list[i]) {
        updated <- TRUE
        break
      }
      else {
        if(jp) {
          file.rename("masterdata.db",
                      str_c("masterdata-",
                            str_sub(current_db, 1, -2),
                            ".db"))
          file.rename("dictionary_ja_k.db",
                      str_c("dictionary_ja_k-",
                            str_sub(current_db, 1, -2),
                            ".db"))
          
          file.copy(from = str_c("masterdata-",
                                 str_sub(current_db, 1, -2),
                                 ".db"),
                    to = str_c("old_databases/masterdata-",
                               str_sub(current_db, 1, -2),
                               ".db"))
          file.copy(from = str_c("dictionary_ja_k-",
                                 str_sub(current_db, 1, -2),
                                 ".db"),
                    to = str_c("old_databases/dictionary_ja_k-",
                               str_sub(current_db, 1, -2),
                               ".db"))
          
          file.remove(str_c("masterdata-",
                            str_sub(current_db, 1, -2),
                            ".db"))
          file.remove(str_c("dictionary_ja_k-",
                            str_sub(current_db, 1, -2),
                            ".db"))
          
          # This needs to be done with a curl rather than the default (wininet).
          # I'm not sure why.
          download.file(str_c(url, directory_list[i], "masterdata.db"),
                        "masterdata.db", method = "curl", quiet = TRUE)
          download.file(str_c(url, directory_list[i], "dictionary_ja_k.db"),
                        "dictionary_ja_k.db", method = "curl", quiet = TRUE)
          
          tryCatch({
            write_lines(directory_list[i], "current_db_jp.txt") 
          },
          error = function(e) {
            message("Could not open jp file. Writing to temp file.")
            write_lines(directory_list[i], "tempfile1.txt")
          })
        }
        
        else {
          file.rename("dictionary_en_k.db",
                      str_c("dictionary_en_k-",
                            str_sub(current_db, 1, -2),
                            ".db"))
          
          file.copy(from = str_c("dictionary_en_k-",
                                 str_sub(current_db, 1, -2),
                                 ".db"),
                    to = str_c("old_databases/dictionary_en_k-",
                               str_sub(current_db, 1, -2),
                               ".db"))
          
          file.remove(str_c("dictionary_en_k-",
                            str_sub(current_db, 1, -2),
                            ".db"))
          
          download.file(str_c(url, directory_list[i], "dictionary_en_k.db"),
                        "dictionary_en_k.db", method = "curl", quiet = TRUE)
          
          tryCatch({
            write_lines(directory_list[i], "current_db_en.txt") 
          },
          error = function(e) {
            message("Could not open en file. Writing to temp file.")
            write_lines(directory_list[i], "tempfile2.txt")
          })
        }
        updated <- TRUE
      }
    }
    
    else {
      i <- i + 1
    }
  }
}

download_dbs(current_db_jp, url_jp, TRUE)
download_dbs(current_db_en, url_en, FALSE)

remove_temp_file <- function(temp, perm) {
  if (temp %in% list.files()) {
    file.remove(perm)
    file.rename(temp, perm)
  }
}

remove_temp_file("tempfile1.txt", "current_db_jp.txt")
remove_temp_file("tempfile2.txt", "current_db_en.txt")
