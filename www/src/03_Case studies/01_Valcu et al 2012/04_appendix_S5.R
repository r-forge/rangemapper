
#' APPENDIX S5 
#' ---------------
#' Supporting information in  _Valcu, M., Dale, J., and Kempenaers, B. (2012). 
#' rangeMapper: a platform for the study of macroecology of life-history traits. 
#' Global Ecology and Biogeography 21, 945-951._

#' Case study 3: The influence of range size on the relationship between species richness and body size
#' ----------------------------------------------------------------------------------------------------------------

#+, results = 'hide', tidy=FALSE
require(rangeMapper)

# 1) SET UP PROJECT 
td = tempdir()
#Initiates a new rangeMapper project (wrens.sqlite) to a temporary directory 
dbcon = rangeMap.start(file = "wrens3.sqlite", dir = td, overwrite = TRUE)

#Location of the vector(*.shp) breeding ranges on disk
branges = system.file(package = "rangeMapper", "extdata", 
									"wrens", "vector_combined")

# Uploads a data.frame containing life history data
data(wrens)
bio.save(con = dbcon, loc = wrens, ID = "sci_name")

# Saves the global bounding box as the union of all species bounding boxes.
global.bbox.save(con = dbcon, bbox = branges)

# Saves the grid size (i.e. the size of a canvas cell) using the default value
gridSize.save(dbcon)
canvas.save(dbcon)

# Performs vector range maps interpolation with the canvas.
r = readOGR(branges, "wrens", verbose = FALSE)
processRanges(spdf = r, con =  dbcon, ID = "sci_name", metadata = rangeTraits() )

rangeMap.save(dbcon, tableName = "species_richness")

#+, results = 'hide', tidy=FALSE

# 2) SET UP PARAMETERS 

W = 3   # size of the moving window

#  range size classes
mt = RMQuery(dbcon, "SELECT * FROM metadata_ranges order by Area")

Q = quantile(log(mt$Area),  probs = seq(0.05, 1, 0.1) )
rangeA = data.frame(area = exp(Q), quant =  gsub("%", "", names(Q)) )

output = vector(mode = "list", length = nrow(rangeA))
names(output) = rangeA$quant

#+, results = 'hide', tidy=FALSE

# 3) Performs log10(median_body_mass) ~ sqrt(species_richness) regression
 # for each range size interval

for(i in seq(1:(nrow(rangeA) - W) ) ) {
	
	# Define  map subset
	area_subset =list(metadata_ranges = paste("Area between",  rangeA[i,"area"], 
	"and", rangeA[i+W,"area"]))
	
	# Save map
	rangeMap.save(dbcon, subset = area_subset , biotab = "wrens", 
	biotrait = "body_mass", FUN = "median", tableName = "temp", 
	overwrite = TRUE)
	
	# Fetch map
	M = rangeMap.fetch( dbcon, c("species_richness", "temp") )
	m  = as(M, "SpatialPointsDataFrame")
	names(m) = c(c("species_richness", "median_body_mass") )
	m = m[!is.na(m$median_body_mass) & !is.na(m$species_richness), ]
	
	# Perform OLS regression
	# NOTE: In order to perform a spatial simultaneous autoregressive error
	 # regression
	#  see 'errorsarlm ' function and the auxiliary neighbours list functions in 
	 # 'spdep' package.
	fm = lm( log10(median_body_mass) ~  sqrt(species_richness), m)

	output[[i]] =  fm

	print(i)
}

output = output[!sapply(output, is.null)]

#+, results = 'hide', tidy=FALSE
# 3) EXTRACT REGGRESION PARAMETERS 

prm = lapply(output, function(x) data.frame(slope = coef(x)[2], 
			  ci2.5 = confint(x)[2,1], ci97.5 = confint(x)[2,2]) )
			  
prm = do.call(rbind, prm)
prm$rangeSize = as.numeric(row.names(prm))


#' Plot
#+ , results = 'hide', fig = TRUE, warning = FALSE, tidy=FALSE
require(Hmisc)
with(prm, errbar(rangeSize, slope, ci2.5, ci97.5) )

