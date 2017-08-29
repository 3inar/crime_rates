# Supporting data and code to "Empirical bayes shrinkage estimation of crime rate statistics"

This repo holds code and data to reproduce all analyses and figures in our article.
The `scr/` directory holds the pertinent code. Please see the different `README` files
in the different sub-directories for details. The data should be fetched and loaded 
for each analysis in `src/`. However, some times SSB changes things (such as column names). In case that happens we have provided a backup data set in the proper format in `src/data/`. The `reports/` directory holds some
different analyses that aren't used directly in the article. We don't guarantee that 
these still run by themselves.

## How to use this resource
We created this project with ProjectTemplate. ProjectTemplate is an R package that helps you organize your statistical
analysis projects. To load this project, you'll first need to `setwd()` into the directory
where this README file is located. Then you need to run the following two
lines of R code:

	library('ProjectTemplate')
	load.project()

After you enter the second line of code, you'll see a series of automated
messages as ProjectTemplate goes about doing its work. This work involves:
* Reading in the global configuration file contained in `config`.
* Loading any R packages you listed in the configuration file.
* Reading in any datasets stored in `data` or `cache`.
* Preprocessing these data using the files in the `munge` directory.

If you simply want to reproduce our work without looking into details, it should be 
enough to `setwd()` to this directory and rund the scripts in `src/`.

For more details about ProjectTemplate, see http://projecttemplate.net
