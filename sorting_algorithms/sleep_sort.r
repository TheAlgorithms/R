# Sleep sort in R:

# SleepSort is a (mostly humorous) sorting algorithm that launches one task per
# element, sleeps proportional to the element's value, then outputs the value
# upon waking. The values are observed in order of their completion times.
#
# This implementation targets Windows compatibility by using a PSOCK cluster
# from the 'parallel' package and leveraging file modification times to capture
# the order in which tasks finish.
#
# Notes:
# - Works for numeric vectors. Negative values are supported by offsetting so
#   all sleep times are non-negative (relative to the minimum value).
# - Large values can cause long sleep times; use 'scale' to control delay.
# - This is for educational/demonstration purposes and not recommended for
#   production sorting.

sleep.sort <- function(elements.vec, scale = 0.01, max.workers = NULL) {
  # Handle trivial cases
  n <- length(elements.vec)
  if (n <= 1) return(elements.vec)
  if (!is.numeric(elements.vec)) stop("sleep.sort: elements.vec must be numeric")

  # Shift to make non-negative delays and avoid negative sleep
  min.val <- min(elements.vec)
  offset <- if (min.val < 0) -min.val else 0

  # Cap scale to a reasonable positive number
  if (!is.numeric(scale) || scale <= 0) stop("sleep.sort: 'scale' must be positive numeric")

  # Determine number of workers
  if (is.null(max.workers)) {
    # Conservative default
    max.workers <- max(1L, min(4L, n))
  }

  # Prepare an output directory to record completion times
  outdir <- file.path(tempdir(), paste0("sleep_sort_", as.integer(runif(1) * 1e9)))
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  # Worker function: sleep then write value to a unique file
  worker_fun <- function(v, idx, scale, offset, outdir) {
    delay <- (v + offset) * scale
    # Add a tiny jitter to reduce ties on duplicates finishing at the exact same time
    delay <- delay + (idx %% 13) * 1e-4
    if (delay < 0) delay <- 0
    Sys.sleep(delay)
    out.path <- file.path(outdir, paste0("task_", idx, ".txt"))
    # Write the original value
    write(as.character(v), file = out.path)
    TRUE
  }

  # Attempt parallel execution; on failure, fall back to base sort
  result <- try({
    # Create cluster
    cl <- parallel::makeCluster(max.workers)
    on.exit({try(parallel::stopCluster(cl), silent = TRUE)}, add = TRUE)

    # Export needed objects
    parallel::clusterExport(cl, varlist = c("elements.vec", "scale", "offset", "outdir", "worker_fun"), envir = environment())

    # Schedule one task per element
    parallel::parLapply(cl, seq_along(elements.vec), function(i) {
      v <- elements.vec[i]
      worker_fun(v, i, scale, offset, outdir)
    })

    # All tasks finished; list files and sort by modification time (completion order)
    files <- list.files(outdir, full.names = TRUE, pattern = "^task_\\d+\\.txt$")
    if (length(files) != n) {
      # In rare cases, the filesystem may lag; simple retry loop
      tries <- 0
      while (tries < 50 && length(files) != n) {
        Sys.sleep(0.02)
        files <- list.files(outdir, full.names = TRUE, pattern = "^task_\\d+\\.txt$")
        tries <- tries + 1
      }
    }
    info <- file.info(files)
    ord <- order(info$mtime, na.last = NA)
    as.numeric(vapply(files[ord], function(f) as.numeric(readLines(f, n = 1L)), numeric(1)))
  }, silent = TRUE)

  if (inherits(result, "try-error")) {
    # Fallback: base sort to keep function usable without parallel
    # (This does not simulate sleep behavior.)
    return(sort(elements.vec))
  }

  return(result)
}

# Example:
# sleep.sort(c(3, 1, 4, 2, 5), scale = 0.05)
# Possible output (order of completion):
# [1] 1 2 3 4 5
