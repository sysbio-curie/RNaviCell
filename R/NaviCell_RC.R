
library(RJSONIO)
library(RCurl)

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

    methods = list(
        initialize = function(...) {
            proxy_url <<- "https://navicell.curie.fr/cgi-bin/nv_proxy.php"
            map_url <<- "https://navicell.curie.fr/navicell/maps/cellcycle/master/index.php"
            msg_id <<- 1000
        }
    )
)

NaviCell$methods(
    incMessageId = function(...) {
        msg_id <<- msg_id + 1
    }
)



