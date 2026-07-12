##data: sagittal slice of mouse brain


library(Seurat)
library(tidyr)
library(patchwork)
library(ggplot2)

#load data

seu_list <- lapply(c("Anterior", "Posterior"), function(slice) {
  # load slices
  seu <- Load10X_Spatial(data.dir = file.path("course_data", slice),
                         slice = slice)
  # set spot ids 
  seu$orig.ident <- slice
  return(seu)
})

#merge anterior and posterior data into one object.

seu <- merge(seu_list[[1]], seu_list[[2]])

#quality control
seu <- SetIdent(seu, value = "orig.ident")

#plot nCount vs nFeatures

FeatureScatter(seu,
               feature1 = "nCount_Spatial",
               feature2 = "nFeature_Spatial",
               shuffle = TRUE
) 
# percent MT genes
seu <- PercentageFeatureSet(seu,
                            pattern = "^MT-|^Mt-|^mt-",
                            col.name = "percent_mt"
)


SpatialPlot(seu, features = "percent_mt", pt.size.factor = 2.5) + 
  plot_layout(guides='collect') & theme(legend.position = "right")


#filter out low quality spots
# high mt count: remove
seu$percent_mt_keep <- !(seu$orig.ident == "Anterior" & seu$percent_mt > 38)

cells_mt_keep <- colnames(seu)[seu$percent_mt_keep]

SpatialPlot(seu, cells.highlight = cells_mt_keep,
            cols.highlight = c("grey50", "red"),
            pt.size.factor = 2.5) +
  plot_annotation(title = "Filter % mitochondrial UMI") +
  plot_layout(guides='collect') & theme(legend.position = "none") 

#filter out low gene number
seu$nFeature_Spatial_keep <- seu$nFeature_Spatial > 100

cells_nfeature_keep <- colnames(seu)[seu$nFeature_Spatial_keep]

SpatialPlot(seu, cells.highlight = cells_nfeature_keep,
            cols.highlight = c("grey50", "red"),
            pt.size.factor = 2.5) +
  plot_annotation(title = "Filter % low number of features") +
  plot_layout(guides='collect') & theme(legend.position = "none") 


seu <- seu[, seu$percent_mt_keep & seu$nFeature_Spatial_keep]

SpatialPlot(seu, group.by = "percent_mt_keep", pt.size.factor = 2.5)  +
  plot_layout(guides='collect') & theme(legend.position = "none") 


#plot very highly expressed genes
most_expressed_boxplot <- function(raw_counts) {
  raw_counts@x <-
    raw_counts@x / rep.int(colSums(raw_counts), diff(raw_counts@p)) * 100
  most_expressed <-
    order(Matrix::rowSums(raw_counts), decreasing = TRUE)[30:1]
  
  raw_counts[most_expressed,] |>
    as.matrix() |>
    t() |>
    boxplot(
      cex.axis = 0.5,
      cex.lab = 0.8,
      cex = 0.1,
      las = 1,
      xlab = "% total count per spot",
      col = (scales::hue_pal())(30)[30:1],
      horizontal = TRUE
    )
}


most_expressed_boxplot(seu[["Spatial"]]$counts.1)
most_expressed_boxplot(seu[["Spatial"]]$counts.2)

###########################################

#normalization and scaling
#split seu back to each slice, and normalize each of them

seu_list <- SplitObject(seu, split.by = "orig.ident")

# preparing the objects for SCTransform
for(slice in names(seu_list)) {
  
   
  seu_list[[slice]]@images <- setNames(
    list(seu_list[[slice]]@images[[slice]]),
    slice)
  
  seu_list[[slice]][["RNA"]] <- seu_list[[slice]][["Spatial"]]
  DefaultAssay(seu_list[[slice]]) <- "RNA"
  
}

seu_list <- lapply(X = seu_list, FUN = SCTransform, assay = "RNA",
                   vars.to.regress = "percent_mt")

#merge the objects back
seu <- merge(seu_list[[1]], seu_list[[2]])

# find HCF in both slices
VariableFeatures(seu) <- intersect(VariableFeatures(seu_list$Anterior),
                                   VariableFeatures(seu_list$Posterior))

#PCA
seu <- RunPCA(seu, assay = "SCT", npcs = 50, verbose = FALSE)

DimPlot(seu, reduction = "pca", group.by = "orig.ident") 

#UMAP based on all PCs
seu <- RunUMAP(seu, reduction = "pca", dims = 1:50)

DimPlot(seu, reduction = "umap", group.by = "orig.ident")


#integrate


