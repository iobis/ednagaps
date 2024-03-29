reference_species_filename <- "reference_species.csv.gz"
input_taxonomy_filename <- "input_taxonomy.csv.gz"

#' Match names with WoRMS.
#'
#' @param x A character vector of names.
#' @return A data frame with the input names and their WoRMS matches.
worms_for_names <- possibly(function(x) {
  worrms::wm_records_names(x, marine_only = FALSE) %>%
    setNames(x) %>%
    bind_rows(.id = "input")
}, otherwise = NULL)

#' Get a list of species names as provided from a reference database.
#'
#' @param reference_database_taxa_path Reference database taxon table path.
#' @param marker Marker name.
get_marker_species_list <- function(reference_database_path, marker) {
  ncol <- max(count.fields(reference_database_path, sep = "\t"), na.rm = TRUE)
  reference_database <- read.table(reference_database_path, sep = "\t", header = FALSE, col.names = 1:ncol, fill = TRUE, quote = "\"", na.strings = c("", "nan"))[,1:9] %>%
    setNames(c("seqid", "taxid", "kingdom", "phylum", "class", "order", "family", "genus", "species")) %>%
    filter(species != "Homo_sapiens") %>%
    distinct(taxid, kingdom, phylum, class, order, family, genus, species) %>%
    mutate(species = str_replace_all(species, "_", " ")) %>%
    mutate(marker = marker)
  return(reference_database)
}

read_reference_database <- function(reference_database_path, marker) {
  ncol <- max(count.fields(reference_database_path, sep = "\t"), na.rm = TRUE)
  reference_database <- read.table(reference_database_path, sep = "\t", header = FALSE, col.names = 1:ncol, fill = TRUE, quote = "\"", na.strings = c("", "nan"))[,1:10] %>%
    setNames(c("seqid", "taxid", "kingdom", "phylum", "class", "order", "family", "genus", "species", "sequence")) %>%
    filter(species != "Homo_sapiens") %>%
    mutate(species = str_replace_all(species, "_", " ")) %>%
    mutate(marker = marker)
  return(reference_database)
}

#' @export
read_reference_databases <- function(reference_databases) {
  purrr::map(names(reference_databases), ~read_reference_database(reference_databases[[.]][["taxa"]], .), .progress = TRUE) %>%
    bind_rows()
}

#' Create species lists with WoRMS accepted names for a set of reference databases.
#' Results are written to a compressed CSV file in the workspace.
#'
#' @export
#' @param reference_databases A named list of reference databases, with at least taxon table paths.
#' @param workspace Path to the workspace.
#' @return Output path.
generate_reference_species <- function(reference_databases, workspace = system.file("data", package = "ednagaps")) {

  # marker and species names as found in reference database

  reference_input_names <- purrr::map(names(reference_databases), ~get_marker_species_list(reference_databases[[.]][["taxa"]], .), .progress = TRUE) %>%
    bind_rows() %>%
    select(marker, species)

  # WoRMS matches for all input species names

  distinct_input_names <- reference_input_names %>%
    filter(
      !str_detect(species, " sp.") &
        !str_detect(species, " aff.") &
        !str_detect(species, " cf.")
    ) %>%
    distinct(species) %>%
    pull(species)

  input_name_batches <- split(distinct_input_names, as.integer((seq_along(distinct_input_names) - 1) / 50))
  plan(multisession)
  matched_names <- future_map(input_name_batches, worms_for_names, .progress = TRUE) %>%
    bind_rows()

  # WoRMS taxonomy for all valid AphiaIDs

  valid_aphiaids <- unique(matched_names$valid_AphiaID)
  aphiaid_batches <- split(valid_aphiaids, as.integer((seq_along(valid_aphiaids) - 1) / 50))
  plan(multisession)
  valid_taxa <- future_map(aphiaid_batches, worrms::wm_record, .progress = TRUE) %>%
    bind_rows()

  # input and valid taxonomy

  input_valid_taxonomy <- matched_names %>%
    select(input, valid_AphiaID) %>%
    group_by(input) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    left_join(valid_taxa, by = c("valid_AphiaID" = "AphiaID"))

  input_taxonomy_path <- file.path(workspace, input_taxonomy_filename)
  write.csv(input_valid_taxonomy, file = gzfile(input_taxonomy_path), row.names = FALSE, na = "")

  # marker and valid taxonomy

  marker_species <- reference_input_names %>%
    select(marker, species) %>%
    left_join(input_valid_taxonomy, by = c("species" = "input"), keep = TRUE) %>%
    filter(!is.na(scientificname) & rank == "Species") %>%
    select(marker, phylum, class, order, family, genus, species = scientificname, isMarine, isBrackish, isFreshwater, isTerrestrial, input)

  reference_species_path <- file.path(workspace, reference_species_filename)
  write.csv(marker_species, file = gzfile(reference_species_path), row.names = FALSE, na = "")
}

#' Read species lists with WoRMS accepted names from the compressed CSV file in the workspace.
#'
#' @param workspace Path to the workspace.
#' @export
read_reference_species <- function(workspace = system.file("data", package = "ednagaps")) {
  read_csv(gzfile(file.path(workspace, reference_species_filename)), show_col_types = FALSE)
}

#' @export
read_input_taxonomy <- function(workspace = system.file("data", package = "ednagaps")) {
  read_csv(gzfile(file.path(workspace, input_taxonomy_filename)), show_col_types = FALSE)
}
