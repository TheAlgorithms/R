# Fast Fourier Transform (FFT)

This file documents the recursive Cooley-Tukey FFT implementation added to `R/mathematics/fast_fourier_transform.r`.

## Description

The `fft_recursive` function computes the discrete Fourier transform (DFT) of a numeric or complex vector using a divide-and-conquer Cooley-Tukey algorithm. If the input length is not a power of two, it is zero-padded to the next power of two.

## Usage

In an R session:

source('mathematics/fast_fourier_transform.r')
fft_recursive(c(0, 1, 2, 3))

From the command line with Rscript:

Rscript -e "source('R/mathematics/fast_fourier_transform.r'); print(fft_recursive(c(0,1,2,3)))"

## Complexity

Time complexity: O(n log n) for inputs with length a power of two; otherwise dominated by padding to next power of two.

Space complexity: O(n) additional space for recursive calls.

## Notes

- The function returns a complex vector of the same length (after padding) as the input.
- This implementation is primarily educational; production code should prefer the optimized `fft` function available in base R.
