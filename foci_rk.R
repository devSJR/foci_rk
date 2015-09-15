# RKWard plugin visualization of spots

require(rkwarddev)

local({
  ## Author names and contact information
  about.info <- rk.XML.about(
    name = "DSB report",
    author = c(
      person(given = "Stefan", family = "Roediger",
             email = "Stefan.Roediger@b-tu.de", 
             role = c("aut","cre")),
      person(given = "Michal", family = "Burdukiewicz",
             email = "michalburdukiewicz@gmail.com", 
             role = c("aut"))),
    about = list(desc = "GUI interface to generate reports from gH2AX experiments.",
                 version = "0.0.1", 
                 url = "https://github.com/devSJR/foci_rk/blob/master/foci_rk.R")
  )
  
  ## Help page
  plugin.summary <- rk.rkh.summary(
    "Analysis of DSB experiments."
  )
  
  plugin.usage <- rk.rkh.usage(
    "Start the DSB report from the menu and conduct the analysis in the GUI."
  )
  
  ## Dependencies
  dependencies.info <- rk.XML.dependencies(dependencies = list(rkward.min = "0.6.3"), 
					   package = list(
							  c(name = "mdcr", min = "0.0.1"),
							  c(name = "readxl", min = "0.1.0")
							  )
					   )
  
  ## General settings
  # File browser for XLS data.
  XLS_file <- rk.XML.browser("Browse here:", url = TRUE, filter = ".xls")
  
  # Definitions of processing in R
  # Error message handling
  suppress.warnings.chk <- rk.XML.cbox(label = "Show warnings", value = "0", un.value = "-1")
  
  # Definition of plot labels and appearance
  generic.plot.options <- rk.plotOptions()
  in.main <- rk.XML.input(label = "Main", initial = "Zell position")
  in.xlab <- rk.XML.input(label = "Abscissa", initial = "X")
  in.ylab <- rk.XML.input(label = "Ordinate", initial = "Y")
  
  biomarker <- rk.XML.dropdown(label = "Biomarker", 
			       options = list(
					      "FITC" = c(val = "FITC", chk = TRUE, i18n = NULL),
					      "APC" = c(val = "APC", chk = FALSE, i18n = NULL)
					      )) 
  
  # Plot preview
  preview.chk <- rk.XML.preview(label = "Preview")
  generic.plot.options <- rk.plotOptions()
  
  basic.settings <- rk.XML.row(
    rk.XML.col(
		XLS_file,
		preview.chk,
		suppress.warnings.chk
	),
    rk.XML.col(
      in.main,
      in.xlab,
      in.ylab,
      biomarker
      )
  )
  
  full.dialog <- rk.XML.dialog(
    label = "DSB report", basic.settings
  )
  
  JS.calc <- rk.paste.JS(
	echo("options( warn = ", suppress.warnings.chk," )\n"),
	echo("raw_data <- read_aklides(\"", XLS_file,"\")\n")
    )


  JS.print <- rk.paste.JS(
    rk.paste.JS.graph(
#       echo("plot(hexbin(raw_data[, 2], raw_data[, 3], xbins = 16), main = \"", in.main,"\", xlab = \"", in.xlab,"\", ylab = \"", in.ylab,"\")\n")
	echo("plot(Y.coord. ~ X.coord., data = raw_data, col = Spots.n., \n"),
	echo("\tsubset = dye == \"", biomarker,"\", pch = 15, cex = 2, \n"),
	echo("\txlab = \"", in.xlab,"\", ylab = \"", in.ylab,"\", main = paste(\"", in.main,"\", \"", biomarker,"\"))\n"),
	echo("points(Y.coord. ~ X.coord., data = raw_data, col = FociOK.n., subset = dye == \"", biomarker,"\", pch = 19)\n")
      ),
      ite("full", rk.paste.JS(
	echo("\nsummary(raw_data[, 2])\n"), level = 3
	)
      )
  )
  
  ## Plugin skeleton generation
  
  DSBreport_menu <<-  rk.plugin.skeleton(
    about = about.info,
    dependencies = dependencies.info,
    xml = list(dialog = full.dialog),
    js = list(require = c("mdcr", "readxl"),
              calculate = JS.calc,
              doPrintout = JS.print,
              results.header = FALSE),
    rkh = list(plugin.summary, plugin.usage),
    pluginmap = list(
      name = "DSBeport",
      hierarchy = list("analysis", "DSB report")),
    create=c("pmap","xml","js","desc", "rkh"),
    load = TRUE,
    overwrite = TRUE,
    show = TRUE
  )
  
})
