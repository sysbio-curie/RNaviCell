source('../R/NaviCell_RC.R')

n <- NaviCell()

# proxy_url <<- "https://navicell.curie.fr/cgi-bin/nv_proxy.php"
# map_url <<- "https://navicell.curie.fr/navicell/maps/cellcycle/master/index.php"

n$proxy_url <- "https://acsn.curie.fr/cgi-bin/nv_proxy.php"
#n$map_url <- "https://acsn/navicell/maps/cellcycle/master/index.php"
#n$map_url <- "https://acsn.curie.fr/navicell/maps/acsn/master/index.php"
n$map_url <- "https://acsn.curie.fr/navicell/maps/survival/master/index.php"

n$launchBrowser()


Sys.sleep(10)

#mat <- as.matrix(read.table('ovca_expression.txt', header=T, row.names=1))
mat <- n$readDatatable('ovca_expression.txt')
print("import expression...")
n$importDatatable("mRNA expression data", "ovca-exp", mat)
print("done.")
print("import annot")
n$importSampleAnnotationFromFile('ovca_sampleinfo.txt')
print("done.")

#mat <- as.matrix(read.table('ovca_mutations.txt', header=T, row.names=1))
#n$importDatatable("Mutation data", "ovca-mut", mat)

#mat <- as.matrix(read.table('mat_0_col', header=T, row.names=1))
#n$importDatatable("Gene list", "test", mat)


