#declarations
clydeRExportTypesToSkip <- c("data.frame");
clydeRExportMaxObjectSize <- 100; #in Mb
clydeRExportMaxMemberSize <- 10; #in Mb, members of an object above this size are set to NULL before export
#clydeRExportPredFuncName <- NULL;
#clydeRExportPredFuncArgList <- NULL;
#clydeRExportPredFuncReturnList <- NULL;
#clydeRExportLibraryList <- NULL;
clydeRExportAlwaysKeepMemberList <- list(glm=c("qr"), lm=c("qr"));

exportAllObjects <- function(exportFileName) {
	objList <- ls(pos=1);
	objListToExport <- NULL;
	for (obj in objList) {
		objType <- as.character(class(get(obj))[1]);
		if (is.na(objType)) {
			objType <- mode(get(obj));
		}

		if (!(objType %in% clydeRExportTypesToSkip)) {
			objNames <- names(get(obj));
			if (!is.null(objNames)) {
				tempObj <- NULL;
				for (objName in objNames) {
					keepObjName <- FALSE;
					if (!is.null(clydeRExportAlwaysKeepMemberList[[objType]]) &&
						objName %in% clydeRExportAlwaysKeepMemberList[[objType]]) {
							keepObjName <- TRUE;
					}
					if (as.numeric(as.vector(object.size(get(obj)[objName])/1024^2)) < clydeRExportMaxMemberSize) {
						keepObjName <- TRUE;
					}
					if (!keepObjName) {
						if (is.null(tempObj)) {
							tempObj <- get(obj);
						}
						tempObj[objName] <- NULL;
					}
				}
				if (!is.null(tempObj)) {
					#special cases
					if (objType %in% c("lm", "glm")) {
						tempObj$qr$qr <- NULL;
					}
					assign(obj, tempObj);
				}
			}
			objSize <- as.numeric(as.vector(object.size(get(obj)))) / 1024^2;
			if (objSize < clydeRExportMaxObjectSize) {
				objListToExport <- c(objListToExport, obj);
			}
		}
	}
	if (!is.null(objListToExport)) {
		save(list=objListToExport, file=exportFileName);
	}
}


#' Export to Clyde Analytical Platform
#'
#' @description
#' Export the calculations in an R script to an .rda file that can be imported into the Clyde
#'     Analytical Platform. This allows models fitted in R to be used as derived columns or packaged
#'     into RESTful web services for real-time and batch scoring.
#'
#' @param exportFileName The file name used to export, with .rda extension.
#' @param predFuncName The name of the user function making the prediction, as a string (see 'Details').
#' @param predColumnList The list of names returned by the prediction function, as a character vector.
#' @param libraryList The list of libraries needed by the prediction function, as a character vector. If NULL,
#'   only the packages attached by default can be used inside the prediction function.
#'
#' @details
#' The script needs to contain a user-defined function that takes an explicit list of formal arguments
#'     and returns a dataframe. The \code{predFuncName} stores the name of this function as a
#'     string (see 'Examples'). The \code{predColumnList} stores the list of columns calculated by the
#'     \code{predFuncName}, and can be either a character vector or a list. If this argument is a
#'     character vector, the returned type of all the computed columns is assumed to be 'numeric'.
#'     Otherwise, this argument should be a named list specifying the return types of all computed
#'     columns (one of either 'integer', 'numeric' or 'factor', see the Random Forest example).
#' The \code{libraryList} argument should be set to the list of packages needed for the \code{predFuncName},
#' if these packages are not attached by default.
#'
#' @examples
#' # GLM
#' data <- data.frame(
#'    x1 = c(5,10,15,20,30,40,60,80,100),
#'    x2 = c(118,58,42,35,27,25,21,19,18),
#'    y = c(69,35,26,21,18,16,13,12,12))
#'
#' g1 <- glm(y ~ x1 + x2, data=data)
#'
#' # function storing the calculations, takes as arguments the predictors used in the glm model
#' # returns a dataframe with one column named 'y_pred'
#' glmPredict <- function(x1, x2) {
#'    df <- as.data.frame(cbind(x1, x2))
#'    res <- as.data.frame(predict(g1, newdata=df, type="response"))
#'    # set the column name(s) for the returned data frame
#'    names(res) <- "y_pred"
#'    return(res)
#' }
#'
#' # the argument predFuncName points to the user-defined function storing the calculations
#' # the argument predColumnList is set to the columns names of the function result
#' # no need to specify the libraryList argument, since the glm prediction uses only the base and stats
#' # packages
#' clydeExport("glm.rda", "glmPredict", c("y_pred"))
#'
#'
#' # Random Forest
#' library(randomForest)
#' data(iris)
#' # replace dot(.) in names(data) with underscore(_)
#' names(iris) <- gsub("\\.", "_", names(iris))
#'
#' set.seed(71)
#' rf <- randomForest(Species ~ ., data=iris)
#'
#' # this function takes as arguments the predictors used in the RF model
#' # returns a data frame with one column named 'Species_pred'
#' rfPredict <- function(Sepal_Length, Sepal_Width, Petal_Length, Petal_Width) {
#'    df <- as.data.frame(cbind(Sepal_Length, Sepal_Width, Petal_Length, Petal_Width));
#'    res <- as.data.frame(predict(rf, newdata=df))
#'    names(res) <- "Species_pred"
#'    return(res)
#' }
#'
#' # the predColumnList is a named list, specifying that the returned value is a factor
#' # package 'randomForest' is needed in the 'rfPredict' function, so set the libraryList argument
#' clydeExport("rf.rda", "rfPredict", list(Species_pred = "factor"), libraryList = c("randomForest"))
#'
#' # use multiple models, return multiple columns in prediction function
#'library(rpart)
#'data(iris)
#'names(iris) <- gsub("\\.", "_", names(iris))
#'
#'g2 <- glm(I(Species == "virginica") ~ ., data=iris, family=binomial(logit));
#'t1 <- rpart(Species ~ ., data=iris)
#'
#'allPredict <- function(Sepal_Length, Sepal_Width, Petal_Length, Petal_Width) {
#'    df <- as.data.frame(cbind(Sepal_Length, Sepal_Width, Petal_Length, Petal_Width));
#'    res <- as.data.frame(predict(g2, type="response"))
#'    names(res) <- c("viginica_pred")
#'    res$Species_pred <- predict(t1, newdata=df, type="class")
#'    res$agree <- as.integer((res$viginica_pred > 0.5) == (res$Species_pred == "virginica"))
#'    return(res)
#'}
#'
#'clydeExport("all.rda", "allPredict",
#'    list(viginica_pred = "numeric", Species_pred = "factor", agree = "integer"),
#'    c("rpart"))


clydeExport <- function(exportFileName, predFuncName, predColumnList, libraryList = NULL) {

  if (missing(exportFileName)) {
    stop("Please specify the 'exportFileName' argument.");
  }
  if (!is.character(exportFileName) || length(exportFileName) > 1) {
    stop("Invalid 'exportFileName' argument.");
  }
  if (length(grep("\\.rda$", exportFileName)) == 0) {
    stop("The export file name should have .rda extension.");
  }
  if (missing(predFuncName)) {
    stop("Please specify the 'predFuncName' argument.");
  }
  if (!is.character(predFuncName) || length(predFuncName) > 1) {
    stop("Invalid 'predFuncName' argument.");
  }
  if (!exists(predFuncName) || class(get(predFuncName)) != "function") {
    stop(paste("Function '", predFuncName, "' not defined.", sep=""));
  }
  if (missing(predColumnList)) {
    stop("Please specify the 'predColumnList' argument.");
  }
  if (!is.character(predColumnList) && !is.list(predColumnList)) {
    stop("Invalid 'predColumnList' argument.");
  }
  if (is.character(predColumnList) && length(predColumnList) == 0) {
    stop("Argument 'predColumnList' is a zero-length vector.");
  }
  if (is.list(predColumnList) && length(predColumnList) == 0) {
    stop("Argument 'predColumnList' is a zero-length list.");
  }
  if (is.list(predColumnList) && FALSE %in% (unique(unlist(predColumnList)) %in% c("integer", "numeric", "factor"))) {
    stop("The only values allowed for components of list 'predColumnList' are 'integer', numeric' or 'factor'.");
  }

  clydeRExportPredFuncName <<- predFuncName;
  clydeRExportPredFuncArgList <<- as.vector(names(formals(get(predFuncName))));
  clydeRExportPredFuncReturnList <<- predColumnList;
  clydeRExportLibraryList <<- libraryList;
  exportAllObjects(exportFileName);
}


