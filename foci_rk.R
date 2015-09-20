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
                 version = "0.0.1-1", 
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
                                             c(name = "readxl", min = "0.1.0"),
                                             c(name = "plotrix", min = "3.5.12"),
                                             c(name = "reshape2", min = "1.4.1")
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
  
  lower.limit <- rk.XML.spinbox(label = "Lower limit", min = 0, max = 99, initial = 15, precision = 2)
  upper.limit <- rk.XML.spinbox(label = "Upper limit", min = 1, max = 100, initial = 70, precision = 2)
  
  biomarker <- rk.XML.dropdown(label = "Biomarker", 
                               options = list(
                                 "FITC" = c(val = "FITC", chk = TRUE, i18n = NULL),
                                 "APC" = c(val = "APC", chk = FALSE, i18n = NULL)
                               ))
			       
  order_parameter <- rk.XML.dropdown(label = "Order parameter", 
		options = list(
				"DSB" = c(val = "DSB", chk = TRUE, i18n = NULL),
				"Cell area" = c(val = "area", chk = FALSE, i18n = NULL)
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
      rk.XML.row(
        in.main,
        in.xlab,
        in.ylab),
      rk.XML.row(
        biomarker,
        order_parameter),
      rk.XML.row(
        lower.limit,
        upper.limit
      )
    )
  )
  
  full.dialog <- rk.XML.dialog(
    label = "DSB report", basic.settings
  )
  
  JS.calc <- rk.paste.JS(
    echo("options( warn = ", suppress.warnings.chk," )\n"),
    echo("raw_data <- read_aklides(\"", XLS_file,"\")\n"),
    echo("raw_data_purged <- raw_data[\"", lower.limit,"\" <= raw_data$Area.um.. & raw_data$Area.um.. <= \"", upper.limit,"\", ]\n"),
    echo("all_spots <- reshape2::melt(list(\n"),
    echo("\t\tSpots.n. = subset(raw_data, dye == \"", biomarker,"\", Spots.n.)[, 1],\n"),
    echo("\t\tFociOK.n. = subset(raw_data, dye == \"", biomarker,"\", FociOK.n.)[, 1]))\n"),
    echo("res_all_spots <- table(all_spots)\n"),
    echo("all_spots_coordinates <- cbind(xlim = c(0,nrow(res_all_spots)), ylim = c(0, max(res_all_spots)))\n"),
    echo("all_spots_purged <- reshape2::melt(list(\n"),
    echo("\t\tSpots.n. = subset(raw_data_purged, dye == \"", biomarker,"\", Spots.n.)[, 1],\n"),
    echo("\t\tFociOK.n. = subset(raw_data_purged, dye == \"", biomarker,"\", FociOK.n.)[, 1]))\n"),
    echo("res <- data.frame(DSB = raw_data[[\"Spots.n.\"]], area = raw_data[[\"Area.um..\"]], dye = raw_data[[\"dye\"]])\n"),
    echo("res <- res[res$dye == \"", biomarker,"\", ]\n"),
    echo("res <- res[order (res[[\"", order_parameter,"\"]]), ]\n"),
    echo("res_density <- density(subset(raw_data, dye == \"", biomarker,"\", Area.um..)[, 1])\n")
  )
  
  
  JS.print <- rk.paste.JS(
    rk.paste.JS.graph(
      echo("par(mfrow = c(2,3))\n"),
      echo("plot(Y.coord. ~ X.coord., data = raw_data, col = Spots.n., \n"),
      echo("\tsubset = dye == \"", biomarker,"\", pch = 15, cex = 2, \n"),
      echo("\txlab = \"", in.xlab,"\", ylab = \"", in.ylab,"\", main = paste(\"", in.main,"\", \"", biomarker,"\"))\n"),
      echo("points(Y.coord. ~ X.coord., data = raw_data, col = FociOK.n., subset = dye == \"", biomarker,"\", pch = 19)\n\n"),
      echo("plot(table(subset(raw_data, dye == \"", biomarker,"\", Spots.n.)), xlim = all_spots_coordinates[, \"xlim\"],\n"),
      echo("\t\tylim = all_spots_coordinates[, \"ylim\"], xlab = \"Foci per cell\", ylab = \"Counts\", col = rgb(0, 0, 1, alpha = 0.7), lwd = 8)\n"),
      echo("points(table(subset(raw_data, dye == \"", biomarker,"\", FociOK.n.)), col = rgb(0, 1, 0, alpha = 0.9), lwd = 4)\n"),
      echo("\tlegend(\"topright\", c(\"All foci\", \"DSB\"), pch = c(19,19), bty = \"n\", col = c(rgb(0, 0, 1, alpha = 0.7), rgb(0, 1, 0, alpha = 0.9)))\n"),
      echo("plot(res_density, main = \"Cell diameter density\", lwd = 2)\n"),
      echo("\t\tabline(v = c(", lower.limit,", ", upper.limit,"), col = \"grey\")\n"),
      
      echo("pyramid.plot(res$DSB, res$area, labels=NA,top.labels=c(\"DSB\",\"\",\"Cell area\"), gap = 4, unit = c(\"Foci per cell\", \"cm²\"), xlim = c(max(res$DSB), max(res$area)))\n"),
      echo("plot(table(subset(raw_data_purged, dye == \"", biomarker,"\", Spots.n.)), xlim = all_spots_coordinates[, \"xlim\"],\n"),
      echo("\t\tylim = all_spots_coordinates[, \"ylim\"], xlab = \"Foci per cell\", ylab = \"Counts\", col = rgb(0, 0, 1, alpha = 0.7), lwd = 8)\n"),
      echo("points(table(subset(raw_data_purged, dye == \"", biomarker,"\", FociOK.n.)), col = rgb(0, 1, 0, alpha = 0.9), lwd = 4)\n"),
      echo("\tlegend(\"topright\", c(\"All foci\", \"DSB\"), pch = c(19,19), bty = \"n\", col = c(rgb(0, 0, 1, alpha = 0.7), rgb(0, 1, 0, alpha = 0.9)))\n"),
      echo("plot(density(subset(raw_data_purged, dye == \"", biomarker,"\", Area.um..)[, 1]), xlim = range(res_density$x), main = \"Cell diameter density\", lwd = 2)\n")
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
    js = list(require = c("mdcr", "readxl", "plotrix", "reshape2"),
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
    show = FALSE
  )
  
  rk.build.plugin(DSBreport_menu, R.libs="~/R", check = TRUE)
  
})

