source('../R/NaviCell_RC.R')

n <- NaviCell()

n$launchBrowser()
Sys.sleep(3)

mat <- as.matrix(read.table('ovca_expression.txt', header=T, row.names=1))

# import data into active NaviCell session
n$importDatatable("mRNA expression data", "ovca-exp", mat)

n$importSampleAnnotationFromFile('ovca_sampleinfo.txt')



