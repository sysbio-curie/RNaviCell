# load the code
source('../R/NaviCell_RC.R')

# create NaviCell object
n <- NaviCell()

# create active NaviCell session with the server
n$launchBrowser()
Sys.sleep(2)

# read in prostate expression data as R matrix 
mat <- as.matrix(read.table('DU145_data.txt', header=T, row.names=1))

# import data into active NaviCell session
n$importDatatable("mRNA expression data", "DU145", mat)

# configure color and treshold parameters 
n$continuousConfigSwitchSampleTab("DU145", "color")
n$continuousConfigSetStepCount("sample", 'color', 'DU145', 2)
n$continuousConfigSetColorAt("DU145", "sample", 1, 'FFFFFF')
n$continuousConfigSetValueAt("DU145", "color", "sample", 0, -1)
n$continuousConfigSetValueAt("DU145", "color", "sample", 2, 1)
n$continuousConfigApply("DU145", "color")

# select map staining as graphical representation for prostate cancer data
# and show the results
n$mapStainingEditorSelectDatatable('DU145')
n$mapStainingEditorSelectSample('data')
n$mapStainingEditorApply()

