#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLTable.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

#' Extract part of FLMatrix object.
#'
#' \code{[]} acts on FLMatrix objects and extracts parts of them.
#'
#'
#' @param object is a FLMatrix object
#' @param rows is a vector input corresponding to rows to be extracted
#' @param cols is a vector input corresponding to columns to be extracted
#' @param drop logical if dimnames to be dropped
#' @return \code{[]} returns FLMatrix object after extraction
#' which replicates the equivalent R extraction.
#' @section Constraints:
#' Applying UDT functions on subsetted matrices with discontinuous row and col ids' 
#' may result in error
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 2,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLmatrix <- flmatrix[1,]
#' @export
`[.FLMatrix`<-function(object,rows=1,cols=1, drop=TRUE)
{
    ##browser()
	connection<-getConnection(object)


    newrownames <- rows
    newcolnames <- cols

    if(missing(cols))
    {
        if (missing(rows)) return(object)
        if(length(unique(rows))!=length(rows))
            stop("Duplicate use of indices not supported so far")
        return(restrictFLMatrix(
                 object = object,
                 whereconditions = object@select@whereconditions,
                 dimnames = list(newrownames,
                                 object@dimnames[[2]]),
                 conditionDims=c(TRUE,FALSE)))
    }
    else { ## !missing(cols)
        if(missing(rows)) {
            if(length(unique(cols))!=length(cols))
                stop("Duplicate use of indices not supported so far")
            return(restrictFLMatrix(
                object = object,
                whereconditions = object@select@whereconditions,
                dimnames = list(object@dimnames[[1]],
                                newcolnames),
                conditionDims=c(FALSE,TRUE)))
        } else {  ## !missing(cols) and !missing(rows)
            if(length(unique(rows))!=length(rows) | length(unique(cols))!=length(cols))
                stop("Duplicate use of indices not supported so far")
            return(restrictFLMatrix(
                object = object,
                whereconditions = object@select@whereconditions,
                dimnames = list(newrownames,
                                newcolnames),
                conditionDims=c(TRUE,TRUE)))
        }
    }
}

#' Extract part of FLTable object.
#'
#' \code{[]} acts on FLMatrix objects and extracts parts of them.
#'
#'
#' @param object is a FLTable object
#' @param rows is a vector input corresponding to rows to be extracted
#' @param cols is a vector input corresponding to columns to be extracted
#' @param drop logical if dimnames to be dropped
#' @return \code{[]} returns FLMatrix object after extraction
#' which replicates the equivalent R extraction.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' fltable <- FLTable( "FL_DEMO", "tblAbaloneWide", "ObsID")
#' resultFLtable <- fltable[1:10,4:6]
#' @export
`[.FLTable`<-function(object,rows=1,cols=1,drop=TRUE)
{
  if(class(object@select)=="FLTableFunctionQuery")
  object <- store(object)
	connection<-getConnection(object)
    if(is.numeric(rows))
        newrownames <- object@dimnames[[1]][rows]
    else
        newrownames <- rows

    if(is.numeric(cols))
        newcolnames <- object@dimnames[[2]][cols]
    else
        newcolnames <- cols

    if(any(is.na(newrownames)) || any(is.na(newcolnames)))
    stop("index out of bounds")
    ##browser()
    if(missing(cols))
    {
        if (!missing(rows)) {
            if(!setequal(object@dimnames[[1]],
                         newrownames))
                object@select@whereconditions <- c(object@select@whereconditions,
                                            inCondition(paste0(object@select@database,".",
                                                               object@select@table_name,".",
                                                               getVariables(object)$obs_id_colname),
                                                        newrownames))
            object@dimnames <- list(newrownames,
                                   object@dimnames[[2]])
        }
    } else if(missing(rows)) { ## !missing(cols)
        ifelse(any(is.na(as.numeric(object@dimnames[[1]]))),
               newrownames <- sort(object@dimnames[[1]]),
               newrownames <- sort(as.numeric(object@dimnames[[1]])))
        object@dimnames <- list(newrownames,
                                newcolnames)
        if(object@isDeep){
            object@select@whereconditions <-
                c(object@select@whereconditions,
                  inCondition(paste0(object@select@database,".",
                                     object@select@table_name,".",
                                     getVariables(object)$var_id_colname),
                              object@dimnames[[2]]))
        }
    } else {  ## !missing(cols) and !missing(rows)
        ##browser()
        if(!setequal(object@dimnames[[1]], newrownames))
            object@select@whereconditions <-
            c(object@select@whereconditions,
              inCondition(paste0(object@select@database,".",
                                 object@select@table_name,".",
                                 getVariables(object)$obs_id_colname),
                          newrownames))
        if(object@isDeep & !setequal(object@dimnames[[2]], newcolnames)){
            object@select@whereconditions <-
                c(object@select@whereconditions,
                  inCondition(paste0(object@select@database,".",
                                     object@select@table_name,".",
                                     getVariables(object)$var_id_colname),
                              newcolnames))
        }
        object@dimnames = list(newrownames, newcolnames)
    }
    if(drop & (ncol(object)==1 | nrow(object) == 1))
    {
      vcolnames <- object@dimnames[[2]]
      vrownames <- object@dimnames[[1]]
      newnames <- NULL
      if(ncol(object)==1 && 
        (!all(vrownames==(1:nrow(object)))))
      {
        MID <- getMaxValue(vdatabase=getOption("ResultDatabaseFL"),
                vtable=getOption("MatrixNameMapTableFL"),
                vcolName="MATRIX_ID",
                vconnection=connection)+1
        newrownames <- storeVarnameMapping(connection=getOption("connectionFL"),
                        tablename=object@select@table_name,
                        matrixId=MID,
                        dimId= 1,
                        mynames=vrownames
                        )
        newrownames <- names(newrownames)
        newcolnames <- vcolnames
        newnames <- newrownames
      }
      else if(object@isDeep && nrow(object)==1 &&
        (!all(vcolnames==(1:ncol(object)))))
      {
        MID <- getMaxValue(vdatabase=getOption("ResultDatabaseFL"),
                vtable=getOption("MatrixNameMapTableFL"),
                vcolName="MATRIX_ID",
                vconnection=connection)+1
        newcolnames <- storeVarnameMapping(connection=getOption("connectionFL"),
                        tablename=object@select@table_name,
                        matrixId=MID,
                        dimId= 1,
                        mynames=vcolnames
                        )
        newcolnames <- names(newcolnames)
        newrownames <- vrownames
        newnames <- newcolnames
      }
      if(!is.null(newnames))
      {
        vtableref <- paste0(getOption("ResultDatabaseFL"),".",
                      getOption("MatrixNameMapTableFL"))
        select <- new(
            "FLSelectFrom",
            connection = connection, 
            database = getOption("ResultDatabaseFL"), 
            table_name = getOption("MatrixNameMapTableFL"),
            variables = list(
                    numIdColname = "Num_ID",
                    nameColname = "NAME"),
            whereconditions=c(paste0(vtableref,".MATRIX_ID=",MID),
                  paste0(vtableref,".DIM_ID=1")),
            order = "")
        
        return(new("FLVector",
                  select=object@select,
                  dimnames=list(newrownames,newcolnames),
                  isDeep=object@isDeep,
                  mapSelect=select))
      }
      else
      return(new("FLVector",
                select=object@select,
                dimnames=list(vrownames,vcolnames),
                isDeep=object@isDeep))
    }
    else return(object)
}


#' Extract part of FLVector object.
#'
#' \code{[]} acts on FLVector objects and extracts parts of them.
#'
#'
#' @param object is a FLVector object
#' @param pSet is a vector representing the indices of elements to extract
#' @return \code{[]} returns FLVector object after extraction
#' which replicates the equivalent R extraction.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' WideTable <- FLTable( "FL_DEMO", "tblAbaloneWide","ObsID")
#' flvector <- FLVector[,"Diameter"]
#' resultFLVector <- flvector[10:1]
#' @export

`[.FLVector` <- function(object,pSet=1:length(object))
{
    if((is.numeric(pSet) && (any(pSet>length(object))
        || any(pSet<=0)))) stop("index out of bounds")
    # browser()
    if(FLNamesMappedP(object) || class(object@select)=="FLTableFunctionQuery") 
    object <- store(object)
    newrownames <- rownames(object)
    newcolnames <- colnames(object)
    if(ncol(object)==1) namesvector <- rownames(object)
    else namesvector <- colnames(object)

    MID <- getMaxValue(vdatabase=getOption("ResultDatabaseFL"),
            vtable=getOption("MatrixNameMapTableFL"),
            vcolName="MATRIX_ID",
            vconnection=connection)+1
    
    if(ncol(object)>1 && !object@isDeep)
    {
      if(is.numeric(pSet))
      pSet <- object@dimnames[[2]][pSet]
      if(!all(pSet %in% colnames(object)))
      stop("index out of bounds")
      object@dimnames[[2]] <- pSet

      if(length(pSet)==1 && object@dimnames[[1]]!=1)
      {
        newnames <- storeVarnameMapping(connection=getOption("connectionFL"),
                      tablename=getOption("ResultVectorTableFL"),
                      matrixId=MID,
                      dimId= 1,
                      mynames=object@dimnames[[1]]
                      )

        vtableref <- paste0(getOption("ResultDatabaseFL"),".",
                            getOption("MatrixNameMapTableFL"))
        mapselect <- new(
                      "FLSelectFrom",
                      connection = getOption("connectionFL"), 
                      database = getOption("ResultDatabaseFL"), 
                      table_name = getOption("MatrixNameMapTableFL"),
                      variables = list(
                              numIdColname = "Num_ID",
                              nameColname = "NAME"),
                      whereconditions=c(paste0(vtableref,".MATRIX_ID=",MID),
                            paste0(vtableref,".DIM_ID=1")),
                      order = "")
        object@dimnames[[1]] <- 1
        object@mapSelect <- mapselect
      }
      return(object)
    }
    if(is.numeric(pSet) && 
      !all(pSet %in% base::charmatch(namesvector,base::unique(namesvector)))) 
    stop("index out of bounds or duplicates in names of vector")
    if(is.character(pSet) && !all(pSet %in% namesvector))
    stop("index out of bounds")
    if(is.character(pSet) && 
      base::identical(as.character(namesvector),as.character(1:length(object))))
    stop("vector names not assigned or same as indices")

    charpSet <- pSet
    if(is.character(pSet) ||
      !base::identical(as.character(namesvector),as.character(1:length(object))))
    {
      if(is.character(pSet))
      {
        charpSet <- pSet
        pSet <- base::charmatch(pSet,base::unique(namesvector))
      }
      else if(is.numeric(pSet) && is.character(namesvector))
      charpSet <- namesvector[pSet]
      namesvector <- base::charmatch(namesvector,base::unique(namesvector))
    }
    options(warn=-1)
    if(base::identical(as.character(pSet),as.character(namesvector))) return(object)
    options(warn=0)
    
    newpSet <- base::charmatch(pSet,base::unique(namesvector))

    newnames <- storeVarnameMapping(connection=getOption("connectionFL"),
                      tablename=getOption("ResultVectorTableFL"),
                      matrixId=MID,
                      dimId= 1,
                      mynames=newpSet
                      )

    vtableref <- paste0(getOption("ResultDatabaseFL"),".",
                        getOption("MatrixNameMapTableFL"))
    mapselect <- new(
                  "FLSelectFrom",
                  connection = getOption("connectionFL"), 
                  database = getOption("ResultDatabaseFL"), 
                  table_name = getOption("MatrixNameMapTableFL"),
                  variables = list(
                          numIdColname = "Num_ID",
                          nameColname = "NAME"),
                  whereconditions=c(paste0(vtableref,".MATRIX_ID=",MID),
                        paste0(vtableref,".DIM_ID=1")),
                  order = "")
    if(is.character(charpSet)) newnames <- charpSet
    else newnames <- 1:length(charpSet)

    if(ncol(object)==1) newrownames <- newnames
    else newcolnames <- newnames

    return(new("FLVector",
                select=object@select,
                dimnames=list(newrownames,newcolnames),
                isDeep=object@isDeep,
                mapSelect=mapselect))
}