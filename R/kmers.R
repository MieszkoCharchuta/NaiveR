get_all_kmers <- function(x, kmer_size = 8){

  n_kmers <- nchar(x) - kmer_size + 1
  sapply(1:n_kmers, get_kmer, sequence = x, kmer_size = kmer_size)

}


get_kmer <- function(sequence, start, kmer_size = 8){

  if(start + kmer_size - 1 > nchar(sequence)) {
    stop("Cannot extract kmer beyond end of sequence")
  }

  substr(sequence, start, start + kmer_size - 1)

}

seq_to_base4 <- function(sequence) {
  toupper(sequence) |>
  gsub(pattern = "[^ACGT]", replacement = "N", x = _) |>
    chartr(old = "ACGT", new = "0123", x = _)

}


base4_to_index <- function(base4_str) {
  # I want output to be indexed to start at position 1 rather than 0
  # so we are adding 1 to all base 10 values
  na.omit(strtoi(base4_str, base = 4) + 1) |> as.numeric()
}


detect_kmers <- function(sequence, kmer_size = 8) {

  kmers <- get_all_kmers(sequence, kmer_size)
  indices <- base4_to_index(kmers)

  n_kmers <- 4^kmer_size

  kmers_detected <- numeric(n_kmers)
  kmers_detected[indices] <- 1

  return(kmers_detected)

}

detect_kmers_across_sequences <- function(sequences, kmer_size = 8) {

  sapply(sequences, detect_kmers, kmer_size = kmer_size, USE.NAMES = FALSE)
}


calc_word_specific_priors <- function(detect_matrix) {

  (apply(detect_matrix, 1, sum) + 0.5) / (1 +  ncol(detect_matrix))

}


#(m(wi) + Pi) / (M + 1)
calc_genus_conditional_prob <- function(detect_matrix,
                                        genera, #needs to be an integer, not character
                                        word_specific_priors) {

  genus_counts <- table(genera) |> as.vector()
  n_genera <- length(genus_counts)
  n_sequences <-  length(genera)

  genus_count <- matrix(0,
                                   nrow = nrow(detect_matrix),
                                   ncol = n_genera)

  for(i in 1:n_sequences) {
    genus_count[, genera[i]] <- detect_matrix[, i] + genus_count[, genera[i]]
  }

  t(t(genus_count + word_specific_priors) / (genus_counts + 1))
}


build_kmer_database <- function(sequences, genera, kmer_size) {

  genera_indices <- genera_str_to_index(genera)

  detected_kmers <- seq_to_base4(sequences) |>
    detect_kmers_across_sequences(kmer_size = kmer_size)

  priors <- calc_word_specific_priors(detected_kmers)

  cond_prob <- calc_genus_conditional_prob(detected_kmers, genera_indices, priors)
  genera_names <- get_unique_genera(genera)

  return(list(conditional_prob = cond_prob,
              genera = genera_names))
}

genera_str_to_index <- function(string) {

  factor(string) |> as.numeric()

}

# factor(string) |> levels())

get_unique_genera <- function(string) {

  factor(string) |> levels()

}
