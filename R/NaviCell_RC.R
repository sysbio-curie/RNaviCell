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
)


#' Format session ID response to appropriate value 
#' 
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



