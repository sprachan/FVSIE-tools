# Convert inside bark to outside bark diameter

For all species except Pinyon pine, rocky mountain maple, cottonwood,
and other hardwood, the bark ratio is not diameter-dependent, so DIB =
k\*DOB where k is the constant species-specific bark ratio.

## Usage

``` r
dob_to_dib(sp, dob)
```

## Arguments

- sp:

  Character. FIA species code, with any leading zeroes.

- dob:

  Numeric. Outside bark diameter.

## Value

Numeric, length matches length of input. The inside bark diameter
calculated using species-specific bark ratio relationships.

## Details

Uses the FVS-IE bark ratio equation (bark ratio = b1+b2/dob) to
calculate inside bark diameter (dib) as bark ratio\*dob
