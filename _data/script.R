source('../R/NaviCell_RC.R')

n <- NaviCell()

n$launchBrowser()
Sys.sleep(3)

#mat <- as.matrix(read.table('ovca_expression.txt', header=T, row.names=1))
#n$importDatatable("mRNA expression data", "ovca-exp", mat)
#n$importSampleAnnotationFromFile('ovca_sampleinfo.txt')

#mat <- as.matrix(read.table('ovca_mutations.txt', header=T, row.names=1))
#n$importDatatable("Mutation data", "ovca-mut", mat)

#mat <- as.matrix(read.table('mat_0_col', header=T, row.names=1))
#n$importDatatable("Gene list", "test", mat)

