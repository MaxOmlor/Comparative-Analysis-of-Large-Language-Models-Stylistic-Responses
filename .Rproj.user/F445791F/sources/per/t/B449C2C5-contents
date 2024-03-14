if (!require("stringr")) install.packages("stringr")
if (!require("plotly")) install.packages("plotly")
if (!require("webshot")) install.packages("webshot")

library(stringr)
library(purrr)
library(plotly)
library(webshot)
library(ggplot2)


load_txt_files <- function(dir_path) {
  # Liste aller .txt-Dateien im Verzeichnis erhalten
  txt_files <- list.files(dir_path, pattern = "\\.txt$", full.names = TRUE)
  
  # Text aus allen .txt-Dateien laden
  all_text <- lapply(txt_files, readLines)
  
  return(all_text)
}

remove_comments <- function(text) {
  # Zeilen des Textes in ein Vektor aufteilen
  lines <- unlist(strsplit(text, "\n"))
  
  # Entferne Zeilen, die mit "#" am Anfang beginnen
  cleaned_lines <- lines[!grepl("^#", lines)]
  
  # Modifizierten Text zusammenführen
  cleaned_text <- paste(cleaned_lines, collapse = "\n")
  
  return(cleaned_text)
}

get_clean_text <- function(text) {
  # Satzzeichen, Zeilenumbrüche und Tabulatoren entfernen
  clean_text <- gsub("[[:punct:]]", " ", text)
  clean_text <- gsub("\\n", " ", clean_text)
  clean_text <- gsub("\\t", " ", clean_text)
  # Entferne zusätzliche Leerzeichen, die durch die Ersetzung entstanden sein könnten
  clean_text <- gsub("\\s+", " ", clean_text)
  clean_text <- trimws(clean_text)
  return(clean_text)
}

count_words <- function(text) {
  clean_text <- get_clean_text(text)
  # print(clean_text)
  
  # Text in Wörter aufteilen
  words <- strsplit(clean_text, "\\s+")[[1]]
  # print(words)
  
  # Anzahl der Wörter zählen
  word_count <- length(words)
  
  return(word_count)
}

agg_word_counts <- function(dir_path) {
  text_list <- load_txt_files(dir_path)
  text_list <- sapply(text_list, remove_comments)
  # print(text_list)
  word_counts <- unname(sapply(text_list, count_words))
  #print(word_counts)
  return(word_counts)
}

#agg_dates <- function(dir_path) {
#  txt_files <- list.files(dir_path, pattern = "\\.txt$", full.names = TRUE)
#  txt_files <- unname(sapply(txt_files, basename))
#  
#  prefixes <- str_extract_all(txt_files, "\\d{4}_\\d{2}_\\d{2}", simplify = TRUE)
#  
#  date_list <- as.Date(prefixes, format = "%Y_%m_%d")
#  return(date_list)
#}

agg_dates <- function(dir_path) {
  txt_files <- list.files(dir_path, pattern = "\\.txt$", full.names = TRUE)
  txt_files <- unname(sapply(txt_files, basename))
  
  prefixes <- str_extract_all(txt_files, "^\\d{4}", simplify = TRUE)
  # prefixes_int <- as.integer(prefixes)
  prefixes_int <- unname(sapply(prefixes, as.integer))
  
  return(prefixes_int)
}

hist_agg_values <- function(values, breaks) {
  # Berechne die Anzahl der breaks
  num_breaks <- length(breaks) - 1
  
  # Initialisiere das Dictionary für die aggregierten Werte
  aggregated <- list()
  
  # Durchlaufe jeden break
  for (i in seq_len(num_breaks)) {
    # Zähle die Anzahl der Werte, die in diesem break liegen
    count <- sum(values >= breaks[i] & values < breaks[i + 1])
    # Füge den aggregierten Wert dem Dictionary hinzu
    aggregated[[paste0(breaks[i], "-", breaks[i + 1])]] <- count
  }
  
  return(aggregated)
}

## Beispielaufruf
#values <- c(100, 200, 300, 700, 800, 900, 1500, 1600, 1800, 2200, 2400, 3000, 4000, 5000, 6000, 7000)
#breaks <- seq(0, 8000, by = 500)

#aggregated_values <- hist_agg_values(values, breaks)
#print(aggregated_values)

multi_hist <- function(datas, main_title, legend, breaks) {
  lists <- unname(lapply(datas, partial(hist_agg_values, breaks=breaks)))
  
  # Automatische Farbwahl für die Säulen
  num_lists <- length(lists)
  colors <- rainbow(num_lists)
  
  # Extrahiere alle eindeutigen keys
  all_keys <- unique(unlist(lapply(lists, names)))
  
  # Plot erstellen
  barplot_matrix <- matrix(nrow = length(all_keys), ncol = num_lists)
  for (i in seq_along(lists)) {
    list <- lists[[i]]
    for (j in seq_along(all_keys)) {
      key <- all_keys[j]
      if (key %in% names(list)) {
        value <- list[[key]]
        barplot_matrix[j, i] <- value
      } else {
        barplot_matrix[j, i] <- 0
      }
    }
  }
  
  # Plot erstellen
  barplot(
    barplot_matrix,
    beside = TRUE,
    main = main_title,
    col = colors,
    width = num_lists * length(breaks)
  )
  
  # x-Achse mit benutzerdefinierten Einheiten beschriften
  axis(1, at = 1:length(breaks), labels = breaks)
  # Legende hinzufügen
  legend("topright", legend = legend, fill = colors)
}

## Beispielaufruf
#list1 <- list(1, 1, 1, 2, 2, 3)
#list2 <- list(1, 1, 2, 3, 3)
#list3 <- list(1, 2, 2, 3, 3, 3)

#lists <- list(list1, list2, list3)

#multi_hist(lists, "test", list("a", "b", "c"), c(1, 2, 3, 4))


multi_hist <- function(datas, main_title, legend, breaks) {
  # Automatische Farbwahl
  num_datas <- length(datas)
  colors <- rainbow(num_datas)
  
  # Berechnung des Minimums und Maximums über alle Daten
  #min_val <- min(unlist(datas))
  #max_val <- max(unlist(datas))
  min_val <- min(unlist(breaks))
  max_val <- max(unlist(breaks))
  
  # Histogramme erstellen
  # par(mar = c(5, 5, 2, 2)) # Setze die Grafikparameter für die Margen
  for (i in seq_along(datas)) {
    if (i == 0) {
      hist(
        datas[[i]],
        col = colors[i],
        xlim = c(min_val, max_val),
        main = main_title,
        breaks = breaks
      )
    } else {
      hist(
        datas[[i]],
        col = colors[i],
        add=TRUE
      )
      
    }
  }
  
  # Legende hinzufügen
  legend("topright", legend = legend, fill = colors)
}

gen_agg_hists <- function(dir_paths, agg_func, agg_title, breaks, img_output_dir) {
  # dir_name <- basename(dir_path)
  # out_file_path <- file.path(img_output_dir, paste0("hist", agg_title, ".jpeg"))
    
  legend = lapply(dir_paths, basename)
    
  agg_vecs <- lapply(dir_paths, agg_func)
  print(agg_vecs)
  
  fig <- plot_ly() 
  for (i in seq_along(agg_vecs)){
    fig <- fig %>%
      add_histogram(x = agg_vecs[[i]], name = legend[i])
  }
  fig <- fig %>%
    layout(title = paste("Histogram over", agg_title),
           xaxis = list(title = agg_title),
           yaxis = list(title = "Frequency"),
           barmode = "group")

  # Speichern der Figur als JPG
  # orca(fig, file = out_file_path, width = 800, height = 600)
  # export(fig, file = out_file_path)
  
  # Render the plot
  print(fig)
}

#dir_paths <- c(
#  "data/politicians/John F. Kennedy",
#  "data/politicians/Margaret Thatcher",
#  "data/politicians/Winston Churchill",
#  "data/climate change/greta thunberg",
#  "data/climate change/un",
#  "data/climate change/unfccc"
#)
dir_paths <- c(
  "data/gpt3_5/climate change",
  "data/gpt3_5/climate change John F. Kennedy",
  "data/gpt3_5/climate change Margaret Thatcher",
  "data/gpt3_5/climate change winston churchill",
  "data/gpt3_5/John F. Kennedy",
  "data/gpt3_5/Margaret Thatcher",
  "data/gpt3_5/winston churchill",
  "data/gpt3_5/rag winston churchill"
)
#dir_paths <- c(
#  "data/gpt4/climate change",
#  "data/gpt4/climate change John F. Kennedy",
#  "data/gpt4/climate change Margaret Thatcher",
#  "data/gpt4/climate change winston churchill",
#  "data/gpt4/John F. Kennedy",
#  "data/gpt4/Margaret Thatcher",
#  "data/gpt4/winston churchill",
#  "data/gpt4/rag winston churchill"
#)

gen_agg_hists(
  dir_paths = dir_paths,
  agg_func = agg_word_counts,
  agg_title = "word counts",
  breaks = seq(0, 8000, by = 100),
  img_output_dir
)

gen_agg_hists(
  dir_paths = dir_paths,
  agg_func = agg_dates,
  agg_title = "date",
  breaks = seq(1900, 2030, by = 10),
  img_output_dir
)

