#declarations
clydeRExportTypesToSkip <- c("data.frame");
clydeRExportMaxObjectSize <- 100; #in Mb
clydeRExportMaxMemberSize <- 10; #in Mb, members of an object above this size are set to NULL before export
clydeRExportPredFuncName <- NULL;
clydeRExportPredFuncArgList <- NULL;
clydeRExportPredFuncReturnList <- NULL;
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
	return(objListToExport);
}



clydeExport <- function(exportFileName, predFuncName, predColumnList) {
	clydeRExportPredFuncName <<- predFuncName;
	clydeRExportPredFuncArgList <<- as.vector(names(formals(get(predFuncName))));
	clydeRExportPredFuncReturnList <<- predColumnList;
	exportAllObjects(exportFileName);
}

getObjMembers <- function(obj) {
	objNames <- names(get(obj));
	objClassList <- NULL;
	objSizeList <- NULL;
	for (objName in objNames) {
		objClassList <- c(objClassList, as.character(class(get(obj)[[objName]])[1]));
		objSizeList <- c(objSizeList, as.vector(object.size(get(obj)[[objName]]) / 1024^2));
	}
	res <- as.data.frame(cbind(objNames, objClassList, objSizeList));
	names(res) <- c("Name", "Class", "Size");
	res$Size <- as.numeric(as.vector(res$Size));
	res <- res[order(-res$Size), ];
	return(res);
}
