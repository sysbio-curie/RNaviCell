source('../R/NaviCell_RC.R')

n <- NaviCell()

n$launchBrowser()
Sys.sleep(2)

#mat <- as.matrix(read.table('ovca_expression.txt', header=T, row.names=1))
mat <- as.matrix(read.table('DU145_data.txt', header=T, row.names=1))


n$importDatatable("mRNA expression data", "DU145", mat)
#
n$datatableConfigSwitchSampleTab("DU145", "color")
n$datatableConfigSetStepCount("sample", 'color', 'DU145', 2)
n$datatableConfigSetColorAt("DU145", "sample", 1, 'FFFFFF')
n$datatableConfigSetValueAt("DU145", "color", "sample", 0, -1)
n$datatableConfigSetValueAt("DU145", "color", "sample", 2, 1)
n$datatableConfigApply("DU145", "color")

n$mapStainingEditorSelectDatatable('DU145')
n$mapStainingEditorSelectSample('data')
n$mapStainingEditorApply()

