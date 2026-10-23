# Visualize FVS Message Frequencies

Visualize FVS Message Frequencies

## Usage

``` r
plot_FVS_messages(out_dir, out_db, stand_df, verbose = FALSE, ...)
```

## Arguments

- out_dir:

  Character string. Location of the output database. Defaults to working
  directory.

- out_db:

  Character string. Name of the FVS output database. Defaults to
  'FVSOut.db', matching the default in
  [`write_FVS_key()`](https://sprachan.github.io/FVSIE-tools/reference/write_FVS_key.md).

- stand_df:

  Dataframe. Stand information for stands run through FVS. Errors will
  only be reported for stands that appear in this dataframe. Requires a
  column named "STAND_ID" (matching STDIDENT passed to write_FVS_files).

- verbose:

  Boolean. If TRUE, unique error messages will be printed to the
  console. Default FALSE.

- ...:

  Optional parameters to pass to plot(). Only used if ggplot is not
  installed.

## Value

If ggplot2 is installed, a ggplot object. Otherwise, a plot on the
current base R plotting device.
