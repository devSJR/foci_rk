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
                 url = "")
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
							  c(name = "hexbin", min = "1.27.1"),
							  c(name = "mdcr", min = "0.0.1"),
							  c(name = "readxl", min = "0.1.0")
							  )
					   )
  
  ## General settings
  
  XLS_file <- rk.XML.browser("Browse here:")
  
  var.select <- rk.XML.varselector(label = "Select data")
  var.data.x <- rk.XML.varslot(label = "Var X", source = var.select, multi = FALSE, classes = "numeric", types = "number", required = FALSE)
  
  # Plot preview
  preview.chk <- rk.XML.preview(label = "Preview")
  generic.plot.options <- rk.plotOptions()
  
  run.DSBreport.chk <- rk.XML.cbox(label = "Start DSB report generation", value = "1", un.value = "0", chk = TRUE)
  
  basic.settings <- rk.XML.row(
    XLS_file,
    run.DSBreport.chk,
    var.select,
    preview.chk,
    var.data.x
  )
  
  full.dialog <- rk.XML.dialog(
    label = "DSB report", basic.settings
  )
  
  JS.calc <- rk.paste.JS(
	echo("raw_data <- read_aklides(\"", XLS_file,"\")\n")
    )


  JS.print <- rk.paste.JS(
    rk.paste.JS.graph(
      	  echo("plot(hexbin(raw_data[, 2], raw_data[, 3], xbins = 16))\n")
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
    js = list(require = c("hexbin", "mdcr", "readxl"),
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
