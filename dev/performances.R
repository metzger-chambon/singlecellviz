# https://engineering-shiny.org/build-yourself-safety-net.html?q=memor#testing-the-app-load
# https://mastering-shiny.org/performance.html


###################################
# Reactivity

library(shiny)
library(reactlog)

# tell shiny to log all reactivity
reactlog_enable()

# run a shiny app
source('dev/run_dev.R')
port = 9560
run_dev(port)

# once app has closed, display reactlog from shiny
shiny::reactlogShow()

###################################
# profvis: Memory usage and time usage

# Memory: Memory allocated or deallocated (for negative numbers)
# for a given call stack. This is represented in megabytes and
# aggregated over all the call stacks over the code in the given row.
# Time: Time spent in milliseconds. This field is also aggregated over
#all the call stacks executed over the code in the given row.


library(profvis)
source('dev/run_dev.R')
p <- profvis::profvis({
  print(
    run_dev()
  )
})
p

# For comparison : opening a seurat object saved as rds 325 Mb; 1790ms
p_seurat <- profvis::profvis({
  readRDS("../singlecelldatabase/db/seurat/pbmc3k_final.rds")
})
p_seurat

#htmlwidgets::saveWidget(p, "profile.html")

###################################
#shinyloadtest package
#https://mastering-shiny.org/performance.html
###################################
library(shinyloadtest)
source("dev/run_dev.R")
# In Rsession1 do:
port = 11425
run_dev(port = port)
# DO NOT CLOSE THE BROWSER APP

# In Rsession2 do:
name = NULL
shinyloadtest::record_session(paste0("http://127.0.0.1:11425"),
                              output_file = "performances/recording",
                              ifelse(is.null(name), "", paste0("-", name)),
                              ".log")
# Test you app by sumilatung a user, and close only this browser

# In a terminal go to the git folder and run:
cat(paste0(
"cd /Volumes/projects/PCa_SingleCell/utils/git/singlecellviz/performances/ \n",
"/Applications/shinycannon.sh /Volumes/projects/PCa_SingleCell/utils/git/singlecellviz/performances/recording",
ifelse(is.null(name), "", paste0("-", name)),
".log http://127.0.0.1:11425 ",
"--overwrite-output ",
"--workers 5 ", # X workers simulated
"--loaded-duration-minutes 1 ", # X min to keep simulating each worker after all workers have completed one session
"--output-dir ", ifelse(is.null(name), "performances/run", name),
" --start-interval 30000" # X millisec of interval between running workers
))
# In Rsession2 do:
df <- load_runs(ifelse(is.null(name), "performances/run", name))
shinyloadtest_report(df, paste0("performances/run/report", ifelse(is.null(name), "", paste0("-", name)),".html"))
# Might need to fix(shinyloadtest_report) and remove Concurrency Legend part


###################################
# bench : Memory usage and time usage

library(bench)
library(httr2)
wait_for_app_to_start <- function(url) { httr2::request(url) |>
    httr2::req_retry(
      max_seconds = 5,
      backoff = function(attempt) 2 ** attempt
    )
}

measure_mem_usage <- function() {
  result_file <- "~/Desktop/temp/test/test.RDS"
  port <- httpuv::randomPort()
  app_process <- callr::r_bg(
    function(result_file, port) {
      on.exit({
        saveRDS(bench::bench_process_memory(), result_file)
      })

      shiny::runApp(
        appDir = singlecellviz::run_app(options = list(port = port)))

    }, args = list(result_file = result_file, port = port))

  on.exit({
    if (app_process$is_alive()) {
      app_process$kill()
    }
  })

  cat(port, "\n")
  app_url <- paste0("http://127.0.0.1:", port)

  wait_for_app_to_start(app_url)

  utils::browseURL(app_url)

  cat ("Press [enter] to finish the test...")
  line <- readline()

  app_process$interrupt()

  app_process$wait()

  readRDS(result_file)
}


measure_mem_usage()

p <- processx::process$new(
  "Rscript",
  c("-e ", "options('shiny.port'= 2811);singlecellviz::run_app()" )
)
# We wait for the app to be ready
Sys.sleep(2)
# Check that the process is alive
p$is_alive()
# Open the app in our browser just to be sure
browseURL("http://127.0.0.1:21536:2811")

#/Applications/shinycannon.sh shinylogs/recording.log   http:///localhost:21536 --workers 10   --output-dir shinylogs/run1


# Bringing the runs in the R session
shinyload_runs <- shinyloadtest::load_runs(
  "5 workers" = "shinylogs/run1"
)

# Creating a directory to receive the logs
fs::dir_create("shinylogs")
# Performing the session recording inside this new folder
withr::with_dir(
  "shinylogs", {
    # Launch the recording of an app session, using port 1234
    shinyloadtest::record_session(
      "http://127.0.0.1:21536:2811",
      port = 1234
    )
  }
)
