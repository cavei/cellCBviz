#' Convert properly format data.frame into json elements for cytoscape
#'
#' @param df a data frame. You must specify 6 columns:
#'     3 for src (id, label, parent) and 3 for dest.
#' @param colors the colors of the parent nodes. If null colors are imputed
#'
#' @return a json
#'
#' @importFrom jsonlite toJSON
#'
#' @export
#'
dataframe2json_cy <- function(df, colors=NULL) {
  if (!all(c("src_id", "src_label", "src_parent",
             "dest_id", "dest_label", "dest_parent")
      %in%
        colnames(df)))
    stop("some columns are missing")

  json_list <- create_network_list(df, colors)
  toJSON(list(elements=json_list), auto_unbox=T)
}


#' Create a list that represent the network
#'
#' Reformat a dataframe with 6 columns to a list
#' that can be converted in elements json
#'
#' @inheritParams dataframe2json_cy
#' @return a list that can be converted in json
#'
#' @rdname dataframe2json_cy
#' @importFrom RColorBrewer brewer.pal
#'
create_network_list <- function(df, colors=NULL) {
  if (!all(c("src_id", "src_label", "src_parent","dest_id", "dest_label", "dest_parent")
               %in%
               colnames(df)))
    stop("some columns are missing")

  stopifnot(is.data.frame(df))
  stopifnot(!grepl("factor", as.character(lapply(df, class))))

  if (is.null(colors)) {
    all_parents <- unique(c(df$dest_parent, df$src_parent))
    color_n <- max(c(3, length(all_parents)))
    color_n <- min(c(color_n, 9))
    colors <- RColorBrewer::brewer.pal(color_n, "Set1")[seq_along(all_parents)]
    names(colors) <- all_parents
  }

  parents <- unique(df$dest_parent)
  parent_node <- lapply(parents, function(p) {
    list(group="nodes", data=list(id=p, label=p, col=colors[p], shape="ellipse"))
  })

  groups <- unique(df$src_label)
  parent_group <- lapply(groups, function(p) {
    list(group="nodes", data=list(id=p, label=p, col="grey", shape="ellipse"))
  })

  df_src <- unique(df[, c("src_id", "src_label", "src_parent")])

  ligand <- lapply(seq_len(nrow(df_src)), function(i) {
    list(group="nodes", data=list(id=df_src$src_id[i], label="", parent=df_src$src_label[i],
                                  col=colors[df_src$src_parent[i]]))
  })

  df_dest <- unique(df[, c("dest_id", "dest_label", "dest_parent")])

  receptor <- lapply(seq_len(nrow(df_dest)), function(i) {
    list(group="nodes", data=list(id=df_dest$dest_id[i], label=df_dest$dest_label[i],
                                  parent=df_dest$dest_parent[i], col=colors[df_dest$dest_parent[i]]))
  })

  simplify_df <- unique(df[, c("src_label","dest_id")])

  edges <- lapply(seq_len(nrow(simplify_df)), function(i) {
    data = list(id = paste("e", i, sep="_"),
                source=simplify_df$src_label[i],
                target=simplify_df$dest_id[i])
    list(group="edges", data=data)
  })

  c(parent_node, parent_group, ligand, receptor, edges)
}
