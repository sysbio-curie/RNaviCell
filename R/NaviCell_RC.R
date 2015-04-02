require(RJSONIO)
require(RCurl)

#' NaviCell reference class
#'
#' @export
NaviCell <- setRefClass(
    # name for the class
    "NaviCell",

    # Define the fields 
    fields = list( 
        proxy_url = "character",
        map_url = "character",
        msg_id = "numeric",
        session_id = "character",
        hugo_list = "vector"
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

#' Increase message ID counter 
#' 
#' @param None 
#' @return None
NaviCell$methods(
    incMessageId = function(...) {
        msg_id <<- msg_id + 1
    }
)

#' Generate a session ID    
#' 
#' @param None 
#' @return None 
#' @export
NaviCell$methods(
    generateSessionId =  function(...) {
        .self$incMessageId()
        response = postForm(.self$proxy_url, style = 'POST', id = "1", perform = "genid", msg_id = .self$msg_id, mode = "session", .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        if (response != "") {
            response <- .self$formatResponse(response)
            .self$session_id <- response
        }
    }
)

#' Set a given zoom level on associated NaviCell map in browser
#'
#' @param module map module name (optional, empty string "" for current map)
#' @param zoom_level value 
#' @export
NaviCell$methods(
    setZoom = function(module, zoom_level) {
        list_param <- list(module=module, args = array(zoom_level), msg_id = .self$msg_id, action = 'nv_set_zoom')
        .self$incMessageId()
        str_data <- .self$makeData(.self$formatJson(list_param))
        #print(str_data)
        response <- postForm(.self$proxy_url, style='POST', id = .self$session_id, mode='cli2srv', perform='send_and_rcv', data=str_data, .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        message(response)
    }
)


#' Launch client browser and points to the default NaviCell map 
#' 
#' @param None 
#' @return None 
#' @export
NaviCell$methods(
    launchBrowser = function(...) {
        .self$incMessageId()
        if (.self$session_id == "") {
            .self$generateSessionId()
        }
        url <- paste(.self$map_url, '?id=', .self$session_id, sep = '')
        browseURL(url)
    }
)

#' List all NaviCell server sessions.
#'
#' @param None
#' @return None
#' @export 
NaviCell$methods(
    listSessions = function(...) {
        response <- postForm(.self$proxy_url, style='POST', id="1", perform="list", msg_id = .self$msg_id, mode="session", .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        message(response)
    }
)

#' Attach a NaviCell server session ID 
#'
#' @param session_id a valid NaviCell session ID (e.g. returned by the function listSessions) 
#' @return None
#' @export 
NaviCell$methods(
    attachSession = function(session_id) {
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


#' Attach NaviCell handle to the last existing NaviCell Web Service session.
#'
#' @param
#' @return
#' @export
NaviCell$methods(
    attachLastSession = function(...) {
        response <- postForm(.self$proxy_url, style='POST', id = '1', msg_id = .self$msg_id, mode = 'session', perform = 'get', which = '@@', .opts=curlOptions(ssl.verifypeer=F))
        response <- .self$formatResponse(response)
        message(response)
        .self$attachSession(response)
    }
)






#' Format response obtained from the RCurl 'postForm' command 
#' 
#' In some cases return from postForm call is 'raw' (e.g. R session in a terminal) otherwise it's plain text (e.g. R GUI)
#' @param None 
#' @return None 
NaviCell$methods(
    formatResponse = function(response) {
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

#' Create NaviCell server command string from a list of parameters 
#' 
#' @param json string 
#' @return NaviCell command string 
NaviCell$methods(
    makeData = function(json_string) {
         ret <- paste("@COMMAND ", json_string, sep = "")
         return(ret)
    }
)

#' Format list of parameters to NaviCell server compatible JSON format
#'
#' @param list of parameters
#' @return json formatted string
NaviCell$methods(
    formatJson = function(list_param) {
        data <- toJSON(list_param)
        data <- gsub("\n", '', data)
        data <- gsub(" ", "", data)
        return(data)
    }
)
