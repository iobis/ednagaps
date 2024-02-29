#' Higher level groups for analysis purposes.
#'
#' @export
groups <- list(
  "fish" = list(
    class = c("Actinopteri", "Cladistii", "Coelacanthi", "Elasmobranchii", "Holocephali", "Myxini", "Petromyzonti", "Teleostei")
  ),
  "turtles" = list(
    order = c("Testudines")
  ),
  "molluscs" = list(
    phylum = c("Mollusca")
  ),
  "mammals" = list(
    class = c("Mammalia")
  ),
  "birds" = list(
    class = c("Aves")
  ),
  "amphibians" = list(
    class = c("Amphibia")
  ),
  "algae" = list(
    phylum = c("Chlorophyta", "Haptophyta", "Rhodophyta", "Ochrophyta", "Bacillariophyta")
  ),
  "echinoderms" = list(
    phylum = c("Echinodermata")
  ),
  "sponges" = list(
    phylum = c("Porifera")
  ),
  "cnidarians" = list(
    phylum = c("Cnidaria", "Ctenophora")
  ),
  "unicellular" = list(
    phylum = c("Cercozoa", "Amoebozoa", "Myzozoa")
  ),
  "fungi" = list(
    phylum = c("Ascomycota", "Oomycota")
  ),
  "worms" = list(
    phylum = c("Nemertea", "Gnathostomulida", "Annelida")
  ),
  "bryozoa" = list(
    phylum = c("Bryozoa")
  ),
  "phoronida" = list(
    phylum = c("Phoronida")
  ),
  "copepods" = list(
    class = c("Copepoda")
  ),
  "crustaceans" = list(
    class = c("Malacostraca", "Thecostraca", "Branchiopoda")
  ),
  "arrowworms" = list(
    phylum = c("Chaetognatha")
  ),
  "ascidians" = list(
    class = c("Ascidiacea")
  )
)

#' Add higher level groups to a taxonomy table.
#'
#' @export
#' @param x A taxonomy table.
#' @return A taxonomy table with higher level groups added.
add_groups <- function(x) {
  groups_df <- groups %>% lapply(as.data.frame) %>% bind_rows(.id = "group")
  x <- x %>%
    left_join(groups_df %>% filter(!is.na(phylum)) %>% select(group, phylum), by = c("phylum")) %>%
    left_join(groups_df %>% filter(!is.na(class)) %>% select(group, class), by = c("class")) %>%
    left_join(groups_df %>% filter(!is.na(order)) %>% select(group, order), by = c("order")) %>%
    mutate(
      group = coalesce(group.x, group.y, group),
      .keep = "unused"
    )
  x
}
