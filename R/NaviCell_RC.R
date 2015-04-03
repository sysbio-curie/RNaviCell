require(RJSONIO)
require(RCurl)

#' NaviCell reference class
#'
NaviCell <- setRefClass(
    # name for the class
    "NaviCell",

    # Define the fields 
    fields = list( 
        proxy_url = "character",
        map_url = "character",
        msg_id = "numeric",
        session_id = "character"
        #hugo_list = "vector"
    ),

    # default values
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
    getHugoList = function( module_name, zoom_level) {
    "Get the list of the HUGO gene symbols for the current map (the list is stored in the object field hugo_list."
        .self$incMessageId()
        list_param <- list(module='', args = array(), msg_id = .self$msg_id, action = 'nv_get_hugo_list')
        str_data <- .self$makeData(.self$formatJson(list_param))
        response <- postForm(.self$proxy_url, style = 'POST', id = .self$session_id, msg_id = .self$msg_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        #message(response)
        response <- fromJSON(response)
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
