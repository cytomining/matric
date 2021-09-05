test_that("`sim_collate` works", {
  ## 0. Filter out some rows

  drop_group <-
    data.frame(Metadata_gene_name = "EMPTY")

  ## 1. Similarity to reference

  # Fetch similarities between
  #
  # a. all rows (except, optionally those containing `reference`)
  #
  # and
  #
  # b. all rows containing `reference`
  #
  # Do so only for those (a, b) pairs that
  #
  # - have *same* values in *all* columns of `all_same_cols_ref`

  reference <-
    data.frame(Metadata_gene_name = c("Chr2"))


  all_same_cols_ref <-
    c("Metadata_cell_line",
      "Metadata_Plate")

  ## 2. Similarity to replicates (no references)

  # Fetch similarities between
  #
  # a. all rows except `reference` rows
  #
  # and
  #
  # b. all rows except `reference` rows (i.e. to each other)
  #
  # Do so for only those (a, b) pairs that
  #
  # - have *same* values in *all* columns of `all_same_cols_rep`
  #
  # Keep, both, (a, b) and (b, a)

  all_same_cols_rep <-
    c("Metadata_cell_line",
      "Metadata_gene_name",
      "Metadata_pert_name")

  ## 3. Similarity to replicates (only references)

  # Fetch similarities between
  #
  # a. all rows containing `reference`
  #
  # and
  #
  # b. all rows containing `reference` (i.e. to each other)
  #
  # Do so for only those (a, b) pairs that
  #
  # - have *same* values in *all* columns of `all_same_cols_rep_ref`.
  #
  # Keep, both, (a, b) and (b, a)

  all_same_cols_rep_ref <-
    c(
      "Metadata_cell_line",
      "Metadata_gene_name",
      "Metadata_pert_name",
      "Metadata_Plate"
    )

  ## 4. Similarity to non-replicates

  # Fetch similarities between
  #
  # a. all rows (except, optionally, `reference` rows)
  #
  # and
  #
  # b. all rows except `reference` rows
  #
  # Do so for only those (a, b) pairs that
  #
  # - have *same* values in *all* columns of `all_same_cols_non_rep`
  #
  # - have *different* values in *all* columns `all_different_cols_non_rep`
  #
  # - have *different* values in *at least one* column of `any_different_cols_non_rep`
  #
  # Keep, both, (a, b) and (b, a)

  any_different_cols_non_rep <-
    c("Metadata_cell_line",
      "Metadata_gene_name",
      "Metadata_pert_name")

  all_same_cols_non_rep <-
    c("Metadata_cell_line",
      "Metadata_Plate")

  all_different_cols_non_rep <-
    c("Metadata_gene_name")


  ## 5. Similarity to group

  # Fetch similarities between
  #
  # a. all rows
  #
  # and
  #
  # b. all rows
  #
  # Do so only for those (a, b) pairs that
  #
  # - have *same* values in *all* columns of `all_same_cols_group`
  #
  # - have *different* values in *at least one* column of `any_different_cols_group`

  all_same_cols_group <-
    c("Metadata_cell_line",
      "Metadata_gene_name")

  any_different_cols_group <-
    c("Metadata_cell_line",
      "Metadata_gene_name",
      "Metadata_pert_name")

  ## Combine all and annotate the similarity matrix

  annotation_cols <-
    c("Metadata_cell_line",
      "Metadata_gene_name",
      "Metadata_pert_name")

  sim_df <- sim_calculate(cellhealth)

  collated_sim <-
    sim_collate(
      sim_df,
      reference,
      all_same_cols_rep = all_same_cols_rep,
      all_same_cols_rep_ref = all_same_cols_rep_ref,
      all_same_cols_ref = all_same_cols_ref,
      any_different_cols_non_rep = any_different_cols_non_rep,
      all_same_cols_non_rep = all_same_cols_non_rep,
      all_different_cols_non_rep = all_different_cols_non_rep,
      any_different_cols_group = any_different_cols_group,
      all_same_cols_group = all_same_cols_group,
      annotation_cols = annotation_cols,
      drop_group = drop_group
    )

  answer <-
    structure(
      list(
        type = c("non_rep", "ref", "rep", "rep_group"),
        n = c(48L, 144L, 60L, 72L)
      ),
      row.names = c(NA, -4L),
      class = c("tbl_df",
                "tbl", "data.frame")
    )

  expect_equal(
    answer,
    collated_sim %>%
      dplyr::group_by(Metadata_cell_line,
                      Metadata_gene_name,
                      type) %>%
      dplyr::tally() %>%
      dplyr::filter(Metadata_gene_name == "AKT1" &
                      Metadata_cell_line == "A549") %>%
      dplyr::ungroup() %>%
      dplyr::select(type, n)
  )

  answer <-
    structure(
      list(
        type = c("non_rep", "ref", "rep", "rep_group"),
        n = c(1152L, 2052L, 468L, 3672L)
      ),
      row.names = c(NA, -4L),
      class = c("tbl_df",
                "tbl", "data.frame")
    )

  expect_equal(answer,
               collated_sim %>%
                 dplyr::group_by(type) %>%
                 dplyr::tally())

  expect_equal(mean(collated_sim$sim), 0.103919374)


  sim_df <- sim_calculate(cellhealth, method = "cosine")

  collated_sim <-
    sim_collate(
      sim_df,
      reference,
      all_same_cols_rep = all_same_cols_rep,
      all_same_cols_rep_ref = all_same_cols_rep_ref,
      all_same_cols_ref = all_same_cols_ref,
      any_different_cols_non_rep = any_different_cols_non_rep,
      all_same_cols_non_rep = all_same_cols_non_rep,
      all_different_cols_non_rep = all_different_cols_non_rep,
      any_different_cols_group = any_different_cols_group,
      all_same_cols_group = all_same_cols_group,
      annotation_cols = annotation_cols,
      drop_group = drop_group
    )

  sim_df_lazy <-
    matric::sim_calculate(matric::cellhealth, method = "cosine", lazy = TRUE)

  collated_sim_lazy <-
    matric::sim_collate(
      sim_df_lazy,
      reference,
      all_same_cols_rep = all_same_cols_rep,
      all_same_cols_rep_ref = all_same_cols_rep_ref,
      all_same_cols_ref = all_same_cols_ref,
      any_different_cols_non_rep = any_different_cols_non_rep,
      all_same_cols_non_rep = all_same_cols_non_rep,
      all_different_cols_non_rep = all_different_cols_non_rep,
      any_different_cols_group = any_different_cols_group,
      all_same_cols_group = all_same_cols_group,
      annotation_cols = annotation_cols,
      drop_group = drop_group
    )

  collated_sim_lazy <-
    sim_calculate_ij(matric::cellhealth, collated_sim_lazy, method = "cosine")

  col_names <- names(collated_sim_lazy)

  collated_sim <-
    collated_sim %>%
    dplyr::arrange(across(-sim)) %>%
    dplyr::select(col_names)

  collated_sim_lazy <-
    collated_sim_lazy %>%
    dplyr::arrange(across(-sim)) %>%
    dplyr::select(col_names)

  expect_equal(collated_sim, collated_sim_lazy)

})
