# Get a dataframe of FVS errors/warnings associated with each stand

Get a dataframe of FVS errors/warnings associated with each stand

## Usage

``` r
get_FVS_messages(out_dir = getwd(), out_db, stand_df)
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

## Value

Dataframe. Columns: Stand ID; any additional stand information that is
in `stand_df`; `warncd`, the FVS warning code that can be looked up in
FVS documentation; `warn_type`, which categorizes FVS warning codes into
'Keyword' warnings (codes 1, 4, or 16), 'Forest Bounds' warnings (code
3), 'Insufficient Trees' warnings (code 8), 'Plot Counts' (code 9), 'Hab
Type' (short for habitat type; code 14, 23, and any between 32-35),
'High tree count' (code 40), 'Stocking' (code 41), and 'Other' (any
other codes).
