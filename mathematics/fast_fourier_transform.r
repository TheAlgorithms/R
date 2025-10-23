# Fast Fourier Transform (Cooley-Tukey recursive implementation)
#
# This implementation accepts a numeric or complex vector and returns
# its discrete Fourier transform as a complex vector. If the input length
# is not a power of two, the vector is zero-padded to the next power of two.
#
# Usage:
#   source('mathematics/fast_fourier_transform.r')
#   x <- c(0,1,2,3)
#   fft_result <- fft_recursive(x)
#   print(fft_result)

next_power_of_two <- function(n) {
  if (n <= 0) return(1)
  p <- 1
  while (p < n) p <- p * 2
  p
}

fft_recursive <- function(x) {
  # Ensure input is complex
  x <- as.complex(x)
  N <- length(x)

  # Pad to next power of two if necessary
  M <- next_power_of_two(N)
  if (M != N) {
    x <- c(x, rep(0+0i, M - N))
    N <- M
  }

  if (N == 1) return(x)

  even <- fft_recursive(x[seq(1, N, by = 2)])
  odd  <- fft_recursive(x[seq(2, N, by = 2)])

  factor <- exp(-2i * pi * (0:(N/2 - 1)) / N)
  T <- factor * odd

  c(even + T, even - T)
}

# Example usage when run directly with Rscript
if (identical(Sys.getenv("R_SCRIPT_NAME"), "") && interactive()) {
  # Running in interactive R session - show sample
  x <- c(0, 1, 2, 3)
  cat("Input:\n")
  print(x)
  cat("FFT result:\n")
  print(fft_recursive(x))
}

# When running via Rscript, users can call: Rscript -e "source('R/mathematics/fast_fourier_transform.r'); print(fft_recursive(c(0,1,2,3)))"
