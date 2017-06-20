################################################################################
# Package: wordprediction                                                      #
# Type: Package                                                                #
# Title: Word Prediction                                                       #
# Author: Michael David Gill                                                   #
# Maintainer: Michael David Gill <michaelgill1969@gmail.com>                   #
# URL: https://github.com/michaelgill1969/coursera-data-science-capstone-      #
# assignment-final-project-submission                                          #
# Description: This package, submitted in partial fulfillment of the           #
# requirements of the Coursera course, "Data Science Capstone", contains       #
# functions for an auto-completion model that predicts the next word to be     #
# typed based on a word or phrase previously typed.                            #
# License: Apache License 2.0                                                  #
################################################################################


#' Download Data File
#'
#' Downloads a data file.
#'
#' @param location URL of the data file to be used formatted as a character.
#' @param data_file name of the data file to be used formatted as a character.
#' @author Michael David Gill
#' @details
#' This function downloads a data file from a specified location URL for use in
#' the package "wordprediction".
#' @export
#' @importFrom utils download.file

download_data_file <- function(location, data_file) {

    if (!file.exists(data_file)) {

        download.file(
            url = location,
            destfile = data_file,
            method = "curl"
        )

    }

}


#' Uncompress Data File
#'
#' Uncompresses a data file
#'
#' @param location local directory created by unpacking the data file.
#' @param data_file name of the data file to be used formatted as a character.
#' @author Michael David Gill
#' @details
#' This function uncompresses a data file for use in the package
#' "wordprediction".
#' @export
#' @importFrom utils unzip

uncompress_data_file <- function(location, data_file) {

    if (!dir.exists(location)) {

        unzip(data_file)

    }

}


#' Sample Data File
#'
#' Samples a data file.
#'
#' @author Michael David Gill
#' @details
#' This function samples 1000 lines each from the data files via Mac OS X shell.
#' @export

sample_data_file <- function() {

    if (!file.exists("./final/de_DE/de_DE.blogs.sample.txt")) {

        system(
            "
            cd ./final/de_DE;
            gshuf -n 1000 de_DE.blogs.txt > de_DE.blogs.sample.txt;
            gshuf -n 1000 de_DE.news.txt > de_DE.news.sample.txt;
            gshuf -n 1000 de_DE.twitter.txt > de_DE.twitter.sample.txt;
            rm de_DE.blogs.txt;
            rm de_DE.news.txt;
            rm de_DE.twitter.txt;
            cd ..;
            cd ./en_US;
            gshuf -n 1000 en_US.blogs.txt > en_US.blogs.sample.txt;
            gshuf -n 1000 en_US.news.txt > en_US.news.sample.txt;
            gshuf -n 1000 en_US.twitter.txt > en_US.twitter.sample.txt;
            rm en_US.blogs.txt;
            rm en_US.news.txt;
            rm en_US.twitter.txt;
            cd ..;
            cd ./fi_FI;
            gshuf -n 1000 fi_FI.blogs.txt > fi_FI.blogs.sample.txt;
            gshuf -n 1000 fi_FI.news.txt > fi_FI.news.sample.txt;
            gshuf -n 1000 fi_FI.twitter.txt > fi_FI.twitter.sample.txt;
            rm fi_FI.blogs.txt;
            rm fi_FI.news.txt;
            rm fi_FI.twitter.txt;
            cd ..;
            cd ./ru_RU;
            gshuf -n 1000 ru_RU.blogs.txt > ru_RU.blogs.sample.txt;
            gshuf -n 1000 ru_RU.news.txt > ru_RU.news.sample.txt;
            gshuf -n 1000 ru_RU.twitter.txt > ru_RU.twitter.sample.txt;
            rm ru_RU.blogs.txt;
            rm ru_RU.news.txt;
            rm ru_RU.twitter.txt;
            cd ..; cd ..;
            "
        )

    }

}


#' Read Data
#'
#' Reads data into a corpus.
#'
#' @param language
#' a character giving the language as IETF language tags "de" for German, "en"
#' for English, "fi" for Finnish, or "ru" for Russian.
#' @return corpus a volatile text corpus as a "tm" package object
#' @author Michael David Gill
#' @details
#' This function reads from a data file as an object of type "corpus" from the
#' "tm" ppackage. It chooses the files corresponding to the language indicated
#' by the language parameter.
#' @export
#' @importFrom tm VCorpus
#' @importFrom tm DirSource
#' @importFrom tm readPlain

read_data <- function(language) {

    if (language == "de") {

        if (!exists("corpus")) {

            VCorpus(
                DirSource("./final/de_DE"),
                readerControl = list(reader = readPlain, language  = "de")
            )

        }

    } else if (language == "en") {

        if (!exists("corpus")) {

            VCorpus(
                DirSource("./final/en_US"),
                readerControl = list(reader = readPlain)
            )

        }

    } else if (language == "fi") {

        if (!exists("corpus")) {

            VCorpus(
                DirSource("./final/fi_FI"),
                readerControl = list(reader = readPlain, language  = "fi")
            )

        }

    } else if (language == "ru") {

        if (!exists("corpus")) {

            VCorpus(
                DirSource("./final/ru_RU"),
                readerControl = list(reader = readPlain, language  = "ru")
            )

        }

    }

}


#' Preprocess Corpus
#'
#' Preprocesses the corpus.
#'
#' @param corpus a volatile text corpus as a "tm" package object
#' @return a preprocessed volatile text corpus as a "tm" package object
#' @author Michael David Gill
#' @details
#' This function preprocesses a corpus by removing punctuation, numbers, and
#' English stopwords, and stripping whitespace.
#' @export
#' @importFrom tm tm_map
#' @importFrom tm removePunctuation
#' @importFrom tm removeNumbers
#' @importFrom tm content_transformer
#' @importFrom tm stripWhitespace
#' @importFrom tm removeWords
#' @importFrom tm stopwords

preprocess_corpus <- function(corpus) {

    # Remove punctuation from text.
    corpus_preprocessed <- tm_map(corpus, removePunctuation)

    # Remove numbers from text.
    corpus_preprocessed <- tm_map(corpus_preprocessed, removeNumbers)

    # Convert text to lowercase.
    corpus_preprocessed <-
        tm_map(corpus_preprocessed, content_transformer(tolower))

    # Strip whitespace from text.
    corpus_preprocessed <- tm_map(corpus_preprocessed, stripWhitespace)

    # Stem the text.
    # corpus_preprocessed <- tm_map(corpus_preprocessed, stemDocument)

    # Remove stopwords.
    corpus_preprocessed <-
        tm_map(corpus_preprocessed, removeWords, stopwords("en"))

    # Return value.
    return(corpus_preprocessed)

}


#' Create n-Gram
#'
#' Creates a n-gram-tokenized term-document matrix (TDM).
#'
#' @param corpus corpus to be tokenized
#' @param n size of n-gram
#' @return a n-gram-tokenized term-document matrix
#' @author Michael David Gill
#' @details
#' This function creates a n-gram-tokenized term-document matrix.
#' @export
#' @importFrom tm TermDocumentMatrix
#' @importFrom ngramrr tdm2

create_ngram <- function(corpus, n) {

    if (n == 1) {

        TermDocumentMatrix(corpus)

    } else {

        tdm2(corpus, ngmin = n, ngmax = n)

    }

}


#' Katz's Back-Off
#'
#' Creates a Katz's back-off model.
#'
#' @param phrase a character-type input to the model
#' @return a character-type word predicted by the model
#' @author Michael David Gill
#' @details
#' This function creates a simplified Katz's back-off model that backs off to
#' smaller n-grams when the key is not found in the larger n-gram.  The maximum
#' n-gram handled is a trigram.  The word returned is the match found in the
#' largest n-gram where the key is found.  When the key is not found in the
#' unigram, the word "will" is returned.
#' @export
#' @importFrom tm Terms
#' @importFrom tm tm_term_score
#' @importFrom tm findAssocs
#' @importFrom tm VectorSource
#' @importFrom tm PlainTextDocument
#' @importFrom tm scan_tokenizer

katz_backoff_model <- function(phrase) {

    if (typeof(phrase) == "character") {

        trigram_model <- function(tokens) {

            key <- function(tokens) {
                paste(
                    tail(
                        tokens,
                        n = 2
                    )[1],
                    tail(
                        tokens,
                        n = 2
                    )[2]
                )
            }

            # find matches and their count
            matches_count <- function(phrase) {
                sapply(
                    names(
                        which(
                            sapply(
                                Terms(tdm_trigram),
                                function(terms) {
                                    grepl(
                                        phrase,
                                        paste(
                                            strsplit(
                                                terms, split = " "
                                            )[[1]][1],
                                            strsplit(
                                                terms, split = " "
                                            )[[1]][2]
                                        ),
                                        ignore.case = TRUE
                                    )
                                }
                            )
                        )
                    ),
                    function(match) sum(tm_term_score(tdm_trigram, match))
                )
            }

            # find the last word of the most frequent match
            tail_of_most_frequent_match <- function(phrase) {
                matches <- matches_count(phrase)
                if (length(matches) > 0) {
                    tail(
                        strsplit(
                            names(
                                head(
                                    which(matches == max(matches)),
                                    n = 1
                                )
                            )
                            , split = " ")[[1]],
                        n = 1
                    )
                } else bigram_model(tail(corpus_input, n = 1))
            }

            return(
                tail_of_most_frequent_match(key(tokens))
            )

        }

        bigram_model <- function(token) {

            # find matches and their count
            matches_count <- function(phrase) {
                sapply(
                    names(
                        which(
                            sapply(
                                Terms(tdm_bigram),
                                function(terms) {
                                    grepl(
                                        phrase,
                                        strsplit(
                                            terms, split = " "
                                        )[[1]][1],
                                        ignore.case = TRUE
                                    )
                                }
                            )
                        )
                    ),
                    function(match) sum(tm_term_score(tdm_bigram, match))
                )
            }

            # find the last word of the most frequent match
            tail_of_most_frequent_match <- function(phrase) {
                matches <- matches_count(phrase)
                if (length(matches) > 0) {
                    tail(
                        strsplit(
                            names(
                                head(
                                    which(matches == max(matches)),
                                    n = 1
                                )
                            )
                            , split = " ")[[1]],
                        n = 1
                    )
                } else unigram_model(tail(corpus_input, n = 1))
            }

            return(
                tail_of_most_frequent_match(token)
            )

        }

        unigram_model <- function(token) {

            associations <-
                findAssocs(tdm_unigram, token, corlimit = .99)[[1]]
            if (length(associations) > 0) {
                names(sample(which(associations == max(associations)), 1))
            } else return("will")

        }

        # preprocess phrase
        corpus_input <-
            VCorpus(
                VectorSource(phrase),
                list(reader = PlainTextDocument)
            )
        corpus_input <- preprocess_corpus(corpus_input)
        corpus_input <- scan_tokenizer(corpus_input[[1]][[1]][1])

        return(
            if (length(corpus_input) >= 2) {
                trigram_model(corpus_input)
            } else if (length(corpus_input) == 1) {
                bigram_model(corpus_input)
            } else unigram_model(corpus_input)
        )

    } else {
        stop("non-character or null input")
    }

}


# # Main function calls.
#
# # load libraries
# library("tm")
# library("ngramrr")
#
# download_data_file(
#     "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
#     "Coursera-SwiftKey.zip"
# )
#
# uncompress_data_file("final", "Coursera-SwiftKey.zip")
#
# sample_data_file()
#
# corpus <- read_data("en")
#
# corpus_cleaned <- preprocess_corpus(corpus)
#
# tdm_unigram <- create_ngram(corpus_cleaned, 1)
# tdm_bigram <- create_ngram(corpus_cleaned, 2)
# tdm_trigram <- create_ngram(corpus_cleaned, 3)
#
# # save the n-grams
# if (!file.exists("./R/shiny/ngrams.RData")) {
#
#     save(tdm_unigram, tdm_bigram, tdm_trigram, file = "./R/shiny/ngrams.RData")
#     # load("./R/shiny/ngrams.RData")
#
# }

