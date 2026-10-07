# ggstream 0.1.0.9000

Compatibility with ggplot2 4.0 (S7 classes) and fixes for several long-open issues. `R CMD check --as-cran` is clean apart from notes that come from the package being archived.

* Fixed the CRAN check failure that got the package archived 2025-11-07: `test-geom_stream.R` asserted an exact `class(...)` equality that broke when ggplot2 4.0 added S7 dispatch classes to built plot objects. Now asserts `inherits(..., "ggplot")` (#26).
* Fixed a crash when a fill category's values are all zero: `new_wiggle()`'s mean-based rescale divided by zero, producing `NaN` that then propagated through `stack_densities()`'s cross-group accumulation and emptied the whole plot. An all-zero group now renders as a flat zero band instead (#23, and likely the same mechanism behind #15's "blank chart with no error" report on small-number series).
* Fixed a crash on dense or discrete x values: multiple raw observations snapping to the same interpolation grid point produced duplicate rows that `merge()` then cross-joined, inflating the row count past `n_grid` and breaking the downstream matrix assignment. Duplicate grid points are now aggregated (summed) before the merge (#16, #18).
* Exported `StatStreamDensity` and `StatStreamDensityText` so custom layers can be built on the same `Stat` `geom_stream()`/`geom_stream_label()` use (#25).
* Simplified `new_wiggle()`'s per-group baseline computation, which was leftover multi-stream centring math from an earlier implementation shape; the function is now only ever called on one already-split group at a time, and the old branch was provably a constant 1.5x scale factor that the existing `mean_y`/`mean_y_post` rescale already cancelled exactly -- a code-clarity fix confirmed to produce byte-identical numeric output, not a behavior change.
* Investigated #19 ("y values far too high" vs. an equivalent stacked area chart) in depth. Neither of two independently-derived candidate fixes survived verification against the bundled `blockbusters` dataset -- the actual root cause is still open. Not guessed at further without the reporter's own data.
* #24 (facet_grid/facet_wrap producing visibly wrong output) did not reproduce against current `ggplot2`/`dplyr` on either a `facet_wrap` or a compound two-variable `facet_grid(scales="free")` case -- left alone rather than "fixed" without a reproduction.
* Modernized `mutate_at()`/`vars()` (deprecated since dplyr 1.0.0) to `mutate(across(...))`.
* Fixed a missing `.data$` pronoun prefix on `fill` in `StatStreamDensityText$setup_data`'s `inside_out`/`onset` sorting branches (present in the parallel `StatStreamDensity` code, missing here -- an `R CMD check` NOTE risk under strict non-standard-evaluation checking).
* Removed `decimal_places()`, a dead function defined but never called anywhere in the package.
* Added `URL`/`BugReports` fields to `DESCRIPTION` (#22).
* Regenerated documentation with current `roxygen2` (8.0.0); fixed two pre-existing `@title`/blank-line-continuation blocks that current roxygen2 rejects as multi-paragraph titles.
