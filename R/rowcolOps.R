aggregationOps<-function(object,margin,operation,...)
{
	connection<-getConnection(object)
	flag3Check(connection)
	var <- genRandVarName()

	if (operation=="Sum")
		opt<-"SUM"
	else if(operation=="Mean")
		opt<-"AVG"

	else stop("Please enter either \"Sum\" or \"Mean\"")

	sqlstr<-paste0( " SELECT '%insertIDhere%' AS vectorIdColumn ",#getMaxVectorId(connection),
			        ",",var,".",object@dimColumns[[margin]]," AS vectorIndexColumn",
			        ", ",opt,"(",var,".",object@dimColumns[[3]],") AS vectorValueColumn 
					FROM ",
					"( ",constructSelect(object),
					" ) AS ",var,
					" GROUP BY ",var,".",object@dimColumns[[margin]])

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	flv <- new("FLVector",
				select = tblfunqueryobj,
				dimnames = list(1:nrow(object),
								"vectorValueColumn"),
				isDeep = FALSE)

	return(flv)
}
