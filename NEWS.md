# dsExposome (development version)

+ solved bug on loadExposomeDS affecting single exposure sets
+ added `addExposure2ExposomeSetDS`
+ added `transformDS`

# dsExposome 2.0.8

+ added function `netcdf_varsDS` to retrieve the variables of a netcdf object

# dsExposome 2.0.7

+ added argument `rownames2col` to the function `exposures_pData` to be able to retrieve the ids and later merge

# dsExposome 2.0.6

Added a new resource resolver to

+ load NetCDF data
+ extract the exposure and spatial information
+ relate this information with clinical location of individuals

This allows us to create new exposure data to be analyzed as an ExposomeSet object with the already present `dsExposome` functions.

Preliminary version, testing to be performed soon as well as throughout documentation in a vignette or bookdown on `dsExposomeClient` repo

# dsExposome 2.0.4

# dsExposome 2.0.3

+ Added Anderson-Darling normality testing `anderson.darling.testDS` to perform normality testing on samples with over 5000 individuals.

# dsExposome 2.0.2

+ Changed output of `invExWASDS` function to return a list of tables instead of an `ExposomeSet` object.

# dsExposome 2.0.1


First release with news file. Integration with Azure DevOps pipeline.
