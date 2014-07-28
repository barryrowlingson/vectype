
payload <- function(a,b,c){
### our new vector will only contain these
    obj = list(a=a,b=b,c=c)
    class(obj) <- "payload"
    obj
}

print.payload <- function(x,...){
    print(format(x,...))
}

format.payload <- function(x,...){
    paste("Payload: a=",x$a," b=",x$b," c=",x$c,sep="")
}

payloadadd <- function(V){
### adds up the components of a payload.
    vectype(
        Reduce(function(p1,p2){
            p1$a=p1$a+p2$a
            p1$b=p1$b+p2$b
            p1$c=p1$c+p2$c
            p1
        }, V)
        )
}

vectype <- function(...){
###
### new 'list'-based vector type
###
    require(plyr)
    obs = llply(list(...),function(ob){
        if(!inherits(ob,"payload")){
            stop("non-payload object in constructor")
        }
        ob
    })
    class(obs) <- "vectype"
    obs
}


c.vectype <- function(...,recursive=FALSE){
###
### make a vectype - check all args are vectypes
###
    llply(list(...),function(ob){
        if(!inherits(ob,"vectype")){
            stop("Can't join non-vectypes to vectype")
        }
    })
    obs = NextMethod()
    class(obs) <- "vectype"
    obs
}

print.vectype <- function(x,...){
    print(format(x))
}

format.vectype <- function(x,...){
    unlist(llply(x,function(ob){
        paste("a=",ob$a," b=",ob$b," c=",ob$c,sep="")
    }))
}
        
as.data.frame.vectype <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x),width.cutoff = 500L), collapse = " ")) {
###
### needed for data.frame(z=vectype(...))
###
    force(nm)
    nrows <- length(x)
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) == nrows && !anyDuplicated(row.names)) {
        }
        else row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}
