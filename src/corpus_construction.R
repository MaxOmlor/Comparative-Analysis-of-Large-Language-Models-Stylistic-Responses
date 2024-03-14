# Installieren und Laden benötigter Pakete
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("fs")) install.packages("fs")
if (!require("tm")) install.packages("tm")
if (!require("topicmodels")) install.packages("topicmodels")

library(jsonlite)
library(fs)
library(stringr)
library(tm)
library(topicmodels)

# text encoding funcs------------------------
encode_none <- function(text) {
  return(text)
}

encode_sentence_len <- function(text) {
  # Zerlege den Text in Sätze unter Berücksichtigung von .!? gefolgt von Leerzeichen oder Zeilenumbrüchen
  # sentences <- unlist(str_split(text, "(?<=[.!?])([\\s]|\\r?\\n)+"))
  sentences <- unlist(str_split(text, "((?<=[.!?])([\\s]|\\r?\\n)+)|(?=[\\r\\n])"))
  # print(sentences)
  
  clean_sentences <- unname(sapply(sentences, function(sentence) {
    # Satzzeichen, Zeilenumbrüche und Tabulatoren entfernen
    clean_sentence <- gsub("[[:punct:]]", " ", sentence)
    clean_sentence <- gsub("\\n", " ", clean_sentence)
    clean_sentence <- gsub("\\t", " ", clean_sentence)
    # Entferne zusätzliche Leerzeichen, die durch die Ersetzung entstanden sein könnten
    clean_sentence <- gsub("\\s+", " ", clean_sentence)
    clean_sentence <- trimws(clean_sentence)
    return(clean_sentence)
  }))
  # print(clean_sentences)
  
  # Kodiere jeden Satz durch ein Wort basierend auf seiner Wortanzahl
  encoded_sentences <- unname(sapply(clean_sentences, function(sentence) {
    words <- str_split(sentence, "\\s+")[[1]] # Zerlege jeden Satz in Wörter
    word_count <- length(words) # Zähle die Anzahl der Wörter
    # return(paste0("len", word_count)) # Erstelle das kodierende Wort basierend auf der Wortanzahl
    return(strrep("x", word_count))
  }))
  # print(encoded_sentences)
  
  # Kombiniere die kodierten Wörter zu einem Text
  encoded_text <- paste(encoded_sentences, collapse=" ")
  
  return(encoded_text)
}

encode_punctuation <- function(text) {
  # Schritt 1: Entferne alle Zeichen, die nicht .,!?; sind
  text <- gsub("[^.,!?;]", "", text)
  
  # Schritt 2: Dieser Schritt ist technisch nicht nötig, da alle nicht Satzzeichen bereits entfernt wurden
  
  # Schritt 3: Ersetze jedes Satzzeichen durch das entsprechende Wort
  text <- gsub("\\.", " PERIOD ", text)
  text <- gsub(",", " COMMA ", text)
  text <- gsub("\\?", " QUESTIONMARK ", text)
  text <- gsub("!", " EXCLAMATIONMARK ", text)
  text <- gsub(";", " SEMICOLON ", text)
  
  # Entferne zusätzliche Leerzeichen, die durch die Ersetzung entstanden sein könnten
  text <- gsub("\\s+", " ", text)
  text <- trimws(text) # Entferne Leerzeichen am Anfang und Ende des Strings
  
  return(text)
}

encode_sentence_len_punctuation <- function(text) {
  sentence_len <- encode_sentence_len(text)
  punctuation <- encode_punctuation(text)
  
  combined_text <- paste(sentence_len, punctuation, sep = "\n")
  
  return(combined_text)
}

encode_topic_modelling <- function(text, k = 5, word_count = 20) {
  # Vorbereitung des Texts
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Erstellung einer Document-Term-Matrix
  dtm <- DocumentTermMatrix(corpus)
  
  # Durchführung des Topic-Modellings
  lda_model <- LDA(dtm, k = k)
  
  # Extraktion der wichtigsten Themen
  topics <- terms(lda_model, word_count)
  
  # Erstellen eines Bag-of-Words
  words <- unlist(topics)
  
  # Umwandeln der Liste von Wörtern in einen String
  word_string <- paste(words, collapse = " ")
  
  return(word_string)
}

encode_topic_words <- function(text) {
  # Umwandlung in Kleinbuchstaben
  text <- tolower(text)
  
  # Entfernung von Satzzeichen
  text <- gsub("[[:punct:]]", " ", text)
  
  # Entfernung von Zahlen
  text <- gsub("[[:digit:]]", " ", text)
  
  # Entfernung von Stoppwörtern
  text <- removeWords(text, stopwords("english"))
  
  # Entfernung von überflüssigen Leerzeichen
  text <- gsub("\\s+", " ", text)
  
  return(text)
}

# print(encode_sentence_len("das ist ein\ntest satz \n zum test von \nzeilenumbrüchen. und hier, noch ein punctuation test!"))

# Dictionary der Kodierungsfunktionen
encoding_functions <- list(
  "none" = encode_none,
  "sentence_len" = encode_sentence_len,
  "punctuation" = encode_punctuation,
  "sentence_len_punctuation" = encode_sentence_len_punctuation,
  "topic_modelling" = encode_topic_modelling,
  "topic_words" = encode_topic_words
)

# Funktion zur Auswahl der Encoding-Funktion basierend auf dem Dictionary
select_encoding_func <- function(config) {
  if ("encoding" %in% names(config)) {
    encoding <- config$encoding
  } else {
    cat("Encode field not found in config. Setting encoding to 'none'.\n")
    encoding <- "none"
  }
  cat(encoding, "selected.\n")
  if (encoding %in% names(encoding_functions)) {
    return(encoding_functions[[encoding]])
  } else {
    stop("Invalid encoding option in config file.")
  }
}
# --------------------------------------

remove_comments <- function(text) {
  # Zeilen des Textes in ein Vektor aufteilen
  lines <- unlist(strsplit(text, "\n"))
  
  # Entferne Zeilen, die mit "#" am Anfang beginnen
  cleaned_lines <- lines[!grepl("^#", lines)]
  
  # Modifizierten Text zusammenführen
  cleaned_text <- paste(cleaned_lines, collapse = "\n")
  
  return(cleaned_text)
}

construct_corpus <- function(json_path) {
  cat("json_path:", json_path, "\n")
  
  # JSON-Datei einlesen
  config <- fromJSON(json_path)
  # print(config)
  
  # Output-Verzeichnis aus dem config-Objekt
  output_dir <- config$output_dir
  # print(output_dir)
  
  if (dir.exists(output_dir)) {
    return()
  }
  
  # clear output_dir from old files
  unlink(output_dir, recursive = TRUE)
  
  # Verzeichnis erstellen, falls es nicht existiert
  dir_create(output_dir)
  
  # Encoding-Funktion auswählen
  encoding_func <- select_encoding_func(config)
  
  process_sources <- function(input_dir, label, sublabel) {
    # cat("Input Dir:", input_dir, "\nLabel:", label, "\nSublabel:", sublabel, "\n")
    
    # Liste aller .txt-Dateien im input_dir
    files <- dir_ls(path = input_dir, regexp = "\\.txt$")
    # print(length(files))
    
    # Dateien durchlaufen und kopieren/umbenennen
    for (i in seq_along(files)) {
      file_path <- files[i]
      new_name <- sprintf("%s_%s%02d.txt", label, sublabel, i)
      # print(file.path(output_dir, new_name))
      
      # Text einlesen
      text <- readLines(file_path, encoding = "UTF-8")
      
      # remove comments
      clean_text <- remove_comments(text)
      
      # Text mit der ausgewählten Encoding-Funktion kodieren
      encoded_text <- encoding_func(clean_text)
      
      # Kodierten Text im Ausgabeverzeichnis abspeichern
      writeLines(encoded_text, file.path(output_dir, new_name), useBytes = TRUE)
    }
  }
  
  # copy and rename files
  mapply(FUN=process_sources, input_dir=config$sources$input_dir, label=config$sources$label, sublabel=config$sources$sublabel)
}


config_dir <- "data/corpus_configs"
config_names <- list.files(config_dir)
config_paths <- file.path(config_dir, config_names)
print(config_paths)

mapply(FUN=construct_corpus, json_path=config_paths)

