require(RJSONIO)
require(RCurl)

#' NaviCell reference class
#'
#' NaviCell (https://navicell.curie.fr) is a web-based environment for browsing, commenting and analyzing very large
#' biological molecular networks using Google Maps and for visualizing 'omics' data on top of
#' the network maps.
#' NaviCell can also act as a server allowing to be remotely controlled through
#' a REST API. A python and a R language bindings have been developped on top of
#' the REST API to hide technical details and to provide a user's friendly
#' interface. A Java binding has been initiated. 
#' This is the R binding implementation.

NaviCell <- setRefClass(
    # class name
    "NaviCell",

    # Define the fields 
    fields = list( 
        proxy_url = "character",
        map_url = "character",
        msg_id = "numeric",
        session_id = "character",
        hugo_list = "vector"
    ),

    # Set default values
    methods = list(
        initialize = function(...) {
            proxy_url <<- "https://navicell.curie.fr/cgi-bin/nv_proxy.php"
            map_url <<- "https://navicell.curie.fr/navicell/maps/cellcycle/master/index.php"
            msg_id <<- 1000
            session_id <<- ""
        }
    )
)

# methods

NaviCell$methods(
    incMessageId = function(...) {
    "Increase message ID counter." 
        msg_id <<- msg_id + 1
    }
)

NaviCell$methods(
    generateSessionId =  function(...) {
    "Generate a session ID."    
        .self$incMessageId()
        response = postForm(.self$proxy_url, style = 'POST', id = "1", perform = "genid", msg_id = .self$msg_id, mode = "session", .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        if (response != "") {
            response <- .self$formatResponse(response)
            .self$session_id <- response
        }
    }
)

NaviCell$methods(
    setZoom = function(module, zoom_level) {
    "Set a given zoom level on associated NaviCell map in browser."
        list_param <- list(module=module, args = array(zoom_level), msg_id = .self$msg_id, action = 'nv_set_zoom')
        .self$incMessageId()
        str_data <- .self$makeData(.self$formatJson(list_param))
        #print(str_data)
        response <- postForm(.self$proxy_url, style='POST', id = .self$session_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        message(response)
    }
)

NaviCell$methods(
    launchBrowser = function(...) {
    "Launch client browser and points to the default NaviCell map."
        .self$incMessageId()
        if (.self$session_id == "") {
            .self$generateSessionId()
        }
        url <- paste(.self$map_url, '?id=', .self$session_id, sep = '')
        browseURL(url)
    }
)

NaviCell$methods(
    listSessions = function(...) {
    "List all NaviCell server sessions."
        response <- postForm(.self$proxy_url, style='POST', id="1", perform="list", msg_id = .self$msg_id, mode="session", .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        message(response)
    }
)

NaviCell$methods(
    attachSession = function(session_id) {
    "Attach a NaviCell server session ID."
        if (.self$session_id != "") {
            warning("Session id already set.")
            return()
        }
        .self$incMessageId()
        # check session id on NaviCell server
        response <- postForm(.self$proxy_url, style='POST', id = session_id, msg_id = .self$msg_id, mode = 'session', perform = 'check', .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        if (response == "ok") {
            .self$session_id <- session_id
        }
        else {
            warning("Wrong session id.")
        }
    }
)

NaviCell$methods(
    attachLastSession = function(...) {
    "Attach NaviCell handle to the last existing NaviCell Web Service session."
        response <- postForm(.self$proxy_url, style='POST', id = '1', msg_id = .self$msg_id, mode = 'session', perform = 'get', which = '@@', .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        message(response)
        .self$attachSession(response)
    }
)

NaviCell$methods(
    formatResponse = function(response) {
    "Format response obtained from the RCurl 'postForm' command (internal utility)."
        ret = ''
        if (class(response) == 'raw') {
            ret <- rawToChar(response)
        }
        else if (class(response) == "character") {
            ret <- response[1]
        }
        return(ret)
    }
)

NaviCell$methods(
    makeData = function(json_string) {
    "Create NaviCell server command string from a list of parameters (internal utility)."
         ret <- paste("@COMMAND ", json_string, sep = "")
         return(ret)
    }
)

NaviCell$methods(
    formatJson = function(list_param) {
    "Format list of parameters to NaviCell server compatible JSON format (internal utility)."
        data <- toJSON(list_param)
        data <- gsub("\n", '', data)
        data <- gsub(" ", "", data)
        return(data)
    }
)

NaviCell$methods(
    getHugoList = function(module_name, zoom_level) {
    "Get the list of the HUGO gene symbols for the current map (the list is stored in the object field hugo_list."
        .self$incMessageId()
        list_param <- list(module='', args = array(), msg_id = .self$msg_id, action = 'nv_get_hugo_list')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        #message(response)
        response <- fromJSON(response)
        .self$hugo_list <- response$data
        return(response$data)
    }
)

NaviCell$methods(
    getBiotypeList = function(object, mod, zoom_level) {
    "Return the list of biotypes understood by NaviCell Web Service."
        .self$incMessageId()
        list_param <- list(module='', args = array(), msg_id = .self$msg_id, action = 'nv_get_biotype_list')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        response <- fromJSON(response)
        return(response$data)
    }
)

NaviCell$methods(
    getModuleList = function(object, mod, zoom_level) {
    "Return the module list of the current NaviCell map."
        .self$incMessageId()
        list_param <- list(module='', args = array(), msg_id = .self$msg_id, action = 'nv_get_module_list')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        response <- fromJSON(response)
        return(response$data)
    }
)


NaviCell$methods(
    importDatatable = function(datatable_biotype, datatable_name, mat) {
    "Import a datatable (matrix) in the current map session."

        # check if the field hugo_list is set
        if (length(.self$hugo_list) == 0) {
            hl <- n$getHugoList()
        }

        # filter matrix on hugo_list
        # abort if there is no overlap
        rownames(mat) %in% n$hugo_list -> idx 
        if (sum(idx) < 1) {
            warning("Error: no overlap between map and matrix HUGO gene symbols.")
            return()
        }

        # select rows on hugo_list
        # watch out: if matrix has 1 col, return type is vector, not matrix, so cast the return to matrix.
        # watch out: when matrix as only one col. colname is lost with subselect, so set it back
        mat_select <- as.matrix(mat[idx,])
        if (ncol(mat) == 1) {
            colnames(mat_select) <- colnames(mat)
        }
        
        data_string <- n$matrix2string(mat_select)
        #print(data_string)

        .self$incMessageId()
        list_param <- list(module='', args = list(datatable_biotype, datatable_name, "", data_string, emptyNamedList), msg_id = .self$msg_id, action = 'nv_import_datatables')
        str_data <- .self$makeData(.self$formatJson(list_param))

        .self$incMessageId()
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F)) 
        #print(.self$formatResponse(response))
    }
)


NaviCell$methods(
    readDatatable = function(fileName) {
    "Read a data file and create an R matrix. Returns a matrix object."
        mat <- as.matrix(read.table(fileName, header=T, row.names=1))
        return(mat)
    }
)


NaviCell$methods(
    matrix2string = function(mat) {
    "Convert an R matrix object to a formatted string (internal utility)."
        header = ""
        if (nrow(mat) == 1) {
            header <- paste(colnames(mat), sep="") 
        }
        else {
            header <- paste(colnames(mat), collapse='\t', sep="") 
        }

        string <- paste('@DATA\ngenes\t', header, sep="")
        string <- paste(string, '\n', sep="")
        nb_row = nrow(mat) 
        for (row in 1:nb_row) {
            gene_name <- paste(rownames(mat)[row], '\t', sep="")  
            row_string = ""
            if (nb_row == 1) {
                row_string <- paste(mat[row], sep="")
            }
            else {
                row_string <- paste(mat[row,], collapse='\t', sep="")
            }
            row_string <- paste(row_string, '\n', sep="")
            string <- paste(string, gene_name, row_string, sep="")
        }
        return(string)
    }
)

#------------------------------------------------------------------------------
#
# Barplot Editor functions 
#
#------------------------------------------------------------------------------

NaviCell$methods(
    barplotEditorOpen = function(...) {
    "Open the barplot editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('open'), msg_id = .self$msg_id, action = 'nv_barplot_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    barplotEditorClose = function(...) {
    "Close the barplot editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('close'), msg_id = .self$msg_id, action = 'nv_barplot_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    barplotEditorApply = function(...) {
    "Apply changes for the barplot editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('apply'), msg_id = .self$msg_id, action = 'nv_barplot_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    barplotEditorApplyAndClose = function(...) {
    "Apply changes and close the barplot editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('apply_and_close'), msg_id = .self$msg_id, action = 'nv_barplot_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    barplotEditorCancel = function(...) {
    "Cancel changes and close the barplot editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('cancel'), msg_id = .self$msg_id, action = 'nv_barplot_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    barplotEditorSelectSample = function(col_num, sample_name) {
    "Select a sample or a group in the barplot editor. col_num = column index number, sample_name = sample or group name"
        .self$incMessageId()
        list_param <- list(module='', args = array(c('select_sample', col_num, sample_name)), msg_id = .self$msg_id, action = 'nv_barplot_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    barplotEditorSelectDatatable = function(datatable_name) {
    "Select a datatable in the barplot editor."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('select_datatable', datatable_name)), msg_id = .self$msg_id, action = 'nv_barplot_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    barplotEditorClearSamples = function(...) {
    "Clear all samples in the barplot editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('clear_samples'), msg_id = .self$msg_id, action = 'nv_barplot_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    barplotEditorSelectAllSamples = function(...) {
    "Select all samples in the barplot editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('all_samples'), msg_id = .self$msg_id, action = 'nv_barplot_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    barplotEditorSelectAllGroups = function(...) {
    "Select all groups in the barplot editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('all_groups'), msg_id = .self$msg_id, action = 'nv_barplot_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    barplotEditorSetTransparency = function(value) {
    "Select transparency parameter in the barplot editor. value = integer between 1 and 100"
        .self$incMessageId()
        list_param <- list(module='', args = array(c('set_transparency', value)), msg_id = .self$msg_id, action = 'nv_barplot_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)


#------------------------------------------------------------------------------
#
# Heatmap Editor functions 
#
#------------------------------------------------------------------------------

NaviCell$methods(
    heatmapEditorOpen = function(...) {
    "Open the heatmap editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('open'), msg_id = .self$msg_id, action = 'nv_heatmap_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    heatmapEditorClose = function(...) {
    "Close the heatmap editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('close'), msg_id = .self$msg_id, action = 'nv_heatmap_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    heatmapEditorCancel = function(...) {
    "Cancel changes and close the heatmap editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('cancel'), msg_id = .self$msg_id, action = 'nv_heatmap_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    heatmapEditorApply = function(...) {
    "Apply changes for the heatmap editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('apply'), msg_id = .self$msg_id, action = 'nv_heatmap_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    heatmapEditorApplyAndClose = function(...) {
    "Apply changes and close the heatmap editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('apply_and_close'), msg_id = .self$msg_id, action = 'nv_heatmap_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    heatmapEditorSelectSample = function(col_num, sample_name) {
    "Select sample or group in heatmap editor. col_num = editor column number, sample_name = sample or group name."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('select_sample', col_num, sample_name)), msg_id = .self$msg_id, action = 'nv_heatmap_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    heatmapEditorSelectDatatable = function(row_num, datatable_name) {
    "Select datatable in heatmap editor. row_num = editor row number."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('select_datatable', row_num, datatable_name)), msg_id = .self$msg_id, action = 'nv_heatmap_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    heatmapEditorClearSamples = function(...) {
    "Clear all samples in heatmap editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('clear_samples'), msg_id = .self$msg_id, action = 'nv_heatmap_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    heatmapEditorSelectAllSamples = function(...) {
    "Select all samples in heatmap editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('all_samples'), msg_id = .self$msg_id, action = 'nv_heatmap_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    heatmapEditorSelectAllGroups = function(...) {
    "Select all groups in heatmap editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('all_groups'), msg_id = .self$msg_id, action = 'nv_heatmap_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    heatmapEditorSetTransparency = function(value) {
    "Set transparency parameter in heatmap editor. value = integer between 1 and 100."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('set_transparency', value)), msg_id = .self$msg_id, action = 'nv_heatmap_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)


#------------------------------------------------------------------------------
#
# Continuous Configuration Editor functions 
#
#------------------------------------------------------------------------------

NaviCell$methods(
    continuousConfigOpen = function(datatable_name, datatable_parameter) {
    "Open continuous configuration editor for a given type of parameter. datatable_parameter = 'shape' or 'color' or 'size'."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('open', datatable_name, datatable_parameter)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    continuousConfigClose = function(datatable_name, datatable_parameter) {
    "Close continuous configuration editor for a given type of parameter. datatable_parameter = 'shape' or 'color' or 'size'."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('close', datatable_name, datatable_parameter)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    continuousConfigApplyAndClose = function(datatable_name, datatable_parameter) {
    "Apply changes and close continuous configuration editor for a given type of parameter. datatable_parameter = 'shape' or 'color' or 'size'."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('apply_and_close', datatable_name, datatable_parameter)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    continuousConfigCancelAndClose = function(datatable_name, datatable_parameter) {
    "Cancel changes and close continuous configuration editor for a given type of parameter. datatable_parameter = 'shape' or 'color' or 'size'."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('cancel', datatable_name, datatable_parameter)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    continuousConfigSetAbsVal = function(datatable_parameter, datatable_name, checked) {
    "Set absolute value mode for continuous configuration editor for a given type of parameter. datatable_parameter = 'shape' or 'color' or 'size', checked = TRUE or FALSE."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('set_sample_absval', datatable_parameter, datatable_name, checked)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    continuousConfigSetSampleMethod = function(datatable_parameter, datatable_name, method_index) {
    "Set the method used when multiple symbols map to the same entity. datatable_parameter = 'shape' or 'color' or 'size', method_index = integer."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('set_sample_method', datatable_parameter, datatable_name, method_index)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    continuousConfigSetGroupMethod = function(datatable_parameter, datatable_name, method_index) {
    "Set the method used when multiple symbols map to the same entity. datatable_parameter = 'shape' or 'color' or 'size', method_index = integer."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('set_group_method', datatable_parameter, datatable_name, method_index)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    continuousConfigSetSelectionSize = function(datatable_name, sample_or_group, index, size) {
    "Set the size selection to a given value for the 'size' parameter. sample_or_group = 'sample' or 'group', index = integer, size = integer."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('set_select_size', datatable_name, "size", sample_or_group, index, size)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)


NaviCell$methods(
    continuousConfigSetSelectionShape = function(datatable_name, sample_or_group, index, shape) {
    "Set the shape selection to a given value for the 'shape' parameter. sample_or_group = 'sample' or 'group', index = integer, shape = integer."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('set_select_shape', datatable_name, "shape", sample_or_group, index, shape)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)


NaviCell$methods(
    continuousConfigSwitchSampleTab = function(datatable_name, datatable_parameter) {
    "Switch continuous configuration editor window to 'sample' tab. Parameter = 'shape' or 'color' or 'size'."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('switch_sample_tab', datatable_name, datatable_parameter)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    continuousConfigSwitchGroupTab = function(datatable_name, datatable_parameter) {
    "Switch continuous configuration editor window to 'group' tab. Parameter = 'shape' or 'color' or 'size'."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('switch_group_tab', datatable_name, datatable_parameter)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)


NaviCell$methods(
    continuousConfigSetStepCount = function(sample_or_group, datatable_parameter, datatable_name,  step_count) {
    "Set continuous configuration step count parameter to a given value. sample_or_group = 'sample' or 'group'. parameter = 'shape' or 'color' or 'size' step_count = integer value."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('step_count_change', sample_or_group, datatable_parameter, datatable_name, step_count)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    continuousConfigSetColorAt = function(datatable_name, sample_or_group, index, color_hex_value) {
    "Set continuous configuration color value. sample_or_group = 'sample' or 'group'."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('set_input_color', datatable_name, 'color', sample_or_group, index, color_hex_value)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    continuousConfigSetValueAt = function(datatable_name, parameter_type, sample_or_group, index, continuous_value) {
    "Set continuous configuration continuous value at a given index. sample_or_group = 'sample' or 'group'. parameter_type = 'size' or 'shape' or 'color'. "
        .self$incMessageId()
        list_param <- list(module='', args = array(c('set_input_value', datatable_name, parameter_type, sample_or_group, index, continuous_value)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    continuousConfigApply = function(datatable_name, parameter_type) {
    "Apply changes to the continuous configuration editor. parameter_type = 'size' or 'shape' or 'color'. "
        .self$incMessageId()
        list_param <- list(module='', args = array(c('apply', datatable_name, parameter_type)), msg_id = .self$msg_id, action = 'nv_display_continuous_config_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)


#------------------------------------------------------------------------------
#
# Map Staining  functions 
#
#------------------------------------------------------------------------------


NaviCell$methods(
    mapStainingEditorSelectDatatable = function(datatable_name) {
    "Select a datatable for the map staining editor."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('select_datatable', datatable_name)), msg_id = .self$msg_id, action = 'nv_map_staining_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    mapStainingEditorSelectSample = function(sample_name) {
    "Select a sample for the map staining editor."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('select_sample', sample_name)), msg_id = .self$msg_id, action = 'nv_map_staining_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    mapStainingEditorApply = function(...) {
    "Apply modifications for the map staining editor."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('apply')), msg_id = .self$msg_id, action = 'nv_map_staining_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    mapStainingEditorCancel = function(...) {
    "Cancel modifications and close the map staining editor."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('cancel')), msg_id = .self$msg_id, action = 'nv_map_staining_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    mapStainingEditorOpen = function(...) {
    "Open the map staining editor."
        .self$incMessageId()
        list_param <- list(module='', args = array('open'), msg_id = .self$msg_id, action = 'nv_map_staining_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    mapStainingEditorClose = function(...) {
    "Close the map staining editor."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('close')), msg_id = .self$msg_id, action = 'nv_map_staining_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    mapStainingEditorApplyAndClose = function(...) {
    "Apply changes and close the map staining editor."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('apply_and_close')), msg_id = .self$msg_id, action = 'nv_map_staining_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    mapStainingEditorSetTransparency = function(transparency_value) {
    "Set the transparency value parameter for the map staining editor (integer value between 1 and 100)."
        .self$incMessageId()
        list_param <- list(module='', args = array(c('set_transparency'), transparency_value), msg_id = .self$msg_id, action = 'nv_map_staining_editor_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        #print(.self$formatResponse(response))
    }
)


#------------------------------------------------------------------------------
#
# Sample Annotation functions 
#
#------------------------------------------------------------------------------


NaviCell$methods(
    importSampleAnnotationFromFile = function(fileName) {
        data_string <- paste(readLines(fileName, warn=F),collapse='\n')
        if (substr(data_string, nchar(data_string), nchar(data_string)) != "\n") {
            data_string <- paste(data_string, '\n', sep="")
        }
        data_string <- paste("@DATA\n", data_string, sep="")
        .self$incMessageId()
        list_param <- list(module='', args = list("import", data_string), msg_id = .self$msg_id, action = 'nv_sample_annotation_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))

        .self$incMessageId()
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F)) 
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    sampleAnnotationSelectAnnotation = function(annotation_name, true_or_false) {
    "Select or un-select an annotation for creating groups from a sample annotation table. true_or_false = TRUE, select, true_or_false = FALSE, un-select."
        .self$incMessageId()
        list_param <- list(module='', args = list("select_annotation", annotation_name, true_or_false), msg_id = .self$msg_id, action = 'nv_sample_annotation_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))

        .self$incMessageId()
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F)) 
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    sampleAnnotationApply = function(...) {
    "Apply the modifications done on a sample annotation table."
        .self$incMessageId()
        list_param <- list(module='', args = list("apply"), msg_id = .self$msg_id, action = 'nv_sample_annotation_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))

        .self$incMessageId()
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F)) 
        #print(.self$formatResponse(response))

    }
)

NaviCell$methods(
    sampleAnnotationOpen = function(...) {
    "Open sample annotation dialog."
        .self$incMessageId()
        list_param <- list(module='', args = list("open"), msg_id = .self$msg_id, action = 'nv_sample_annotation_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))

        .self$incMessageId()
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F)) 
        #print(.self$formatResponse(response))

    }
)

NaviCell$methods(
    sampleAnnotationClose = function(...) {
    "Close sample annotation dialog."
        .self$incMessageId()
        list_param <- list(module='', args = list("close"), msg_id = .self$msg_id, action = 'nv_sample_annotation_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))

        .self$incMessageId()
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F)) 
        #print(.self$formatResponse(response))
    }
)

NaviCell$methods(
    sampleAnnotationCancel = function(...) {
    "Cancel changes and close sample annotation dialog."
        .self$incMessageId()
        list_param <- list(module='', args = list("cancel"), msg_id = .self$msg_id, action = 'nv_sample_annotation_perform')
        str_data <- .self$makeData(.self$formatJson(list_param))

        .self$incMessageId()
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F)) 
        #print(.self$formatResponse(response))
    }
)
