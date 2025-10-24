# Copilot Instructions for TheAlgorithms/R

## General Guidelines

This repository contains implementations of various algorithms in R. All contributions should follow these guidelines to maintain code quality and consistency.

## Code Quality & Functionality

- Ensure that your code is functional and well-structured before submitting
- The code should run without errors in an R environment and produce the expected output
- Follow best practices for efficiency, readability, and maintainability
- Use consistent and meaningful variable names (use `.` or `_` to separate words, e.g., `results.df` for a data frame)

## Adding New Algorithms

When adding a new algorithm:
- **Verify that the algorithm is not already implemented** in the repository (including under a different name)
- **Confirm that the proposed algorithm is a recognized computer-science algorithm**, not a problem-specific adaptation of a general technique (e.g., tuned for LeetCode or other competitive-programming problems)
- Include a brief explanation of the algorithm in the file as comments
- Add an example showcasing its usage (can be commented within the script)
- **Update DIRECTORY.md** to include the new algorithm in the appropriate section

## Modifying Existing Algorithms

When modifying existing algorithms:
- Clearly document the changes in your pull request description
- Ensure that your modifications do not break existing functionality
- If applicable, update or add test cases to validate your changes

## File Naming & Structure Conventions

- **All code file names must use lowercase `.r` extension** (not `.R`)
- Ensure that filenames follow the existing directory structure and naming patterns
- Files should be placed in the appropriate category directory (e.g., `sorting_algorithms/`, `graph_algorithms/`, `mathematics/`)

## Documentation & Comments

- Provide clear and concise documentation in the form of comments within the code
- Add a brief docstring at the beginning of the script explaining:
  - What the algorithm does
  - The expected input and output
  - Any dependencies required

## Testing & Verification

Before submitting a pull request, verify that your code:
- Runs correctly with different test cases
- Does not produce unnecessary warnings or errors
- If applicable, add a test file demonstrating the algorithm's correctness

## Pull Request Review Checklist

When reviewing a pull request:
- Verify that any added algorithms or data structures aren't already implemented elsewhere in the repository (including under a different name)
- Confirm that the proposed algorithm is a recognized computer-science algorithm, not a problem-specific adaptation of a general technique (e.g., tuned for LeetCode or other competitive-programming problems). It is prohibited to add LeetCode problems.
- Check that the extension of all code file names is a lowercase `.r`
- Check that the newly added algorithm is also added to DIRECTORY.md file
- Verify that the code includes appropriate documentation and examples
- Ensure that variable naming follows repository conventions
