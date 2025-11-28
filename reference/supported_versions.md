# Get all the supported device-formats in LightLogR

Returns all the supported versions for device-formats in LightLogR.

## Usage

``` r
supported_versions(device = NULL)
```

## Arguments

- device:

  optionally specify a device to only show entries for this devices

## Value

A list of tibbles (or just a tibble if device is provided) containing
the device name, version names, a logical that indicates whether this is
the version that is used by default, and a description.

## Details

This list contains all data formats for each device. These formats can
be used through
[`import_Dataset()`](https://tscnlab.github.io/LightLogR/reference/import_Dataset.md)
with the respective `version`. When there are no entries for a device,
this device has only one known format.

## See also

[import_Dataset](https://tscnlab.github.io/LightLogR/reference/import_Dataset.md),
[supported_devices](https://tscnlab.github.io/LightLogR/reference/supported_devices.md)

## Examples

``` r
supported_versions()
#> $ActLumus
#> # A tibble: 1 × 4
#>   Device   Version Default Description                                          
#>   <chr>    <chr>   <lgl>   <chr>                                                
#> 1 ActLumus initial TRUE    Device format as it was initially implemented in Lig…
#> 
#> $ActTrust
#> # A tibble: 1 × 4
#>   Device   Version Default Description                                          
#>   <chr>    <chr>   <lgl>   <chr>                                                
#> 1 ActTrust initial TRUE    Device format as it was initially implemented in Lig…
#> 
#> $Actiwatch_Spectrum
#> # A tibble: 2 × 4
#>   Device             Version Default Description                                
#>   <chr>              <chr>   <lgl>   <chr>                                      
#> 1 Actiwatch_Spectrum initial TRUE    Device format as it was initially implemen…
#> 2 Actiwatch_Spectrum de      FALSE   This version is for a german file format, …
#> 
#> $Circadian_Eye
#> # A tibble: 1 × 4
#>   Device        Version Default Description                                     
#>   <chr>         <chr>   <lgl>   <chr>                                           
#> 1 Circadian_Eye initial TRUE    Device format as it was initially implemented i…
#> 
#> $Clouclip
#> # A tibble: 1 × 4
#>   Device   Version Default Description                                          
#>   <chr>    <chr>   <lgl>   <chr>                                                
#> 1 Clouclip initial TRUE    Device format as it was initially implemented in Lig…
#> 
#> $DeLux
#> # A tibble: 1 × 4
#>   Device Version Default Description                                            
#>   <chr>  <chr>   <lgl>   <chr>                                                  
#> 1 DeLux  initial TRUE    Device format as it was initially implemented in Light…
#> 
#> $GENEActiv_GGIR
#> # A tibble: 1 × 4
#>   Device         Version Default Description                                    
#>   <chr>          <chr>   <lgl>   <chr>                                          
#> 1 GENEActiv_GGIR initial TRUE    Device format as it was initially implemented …
#> 
#> $Kronowise
#> # A tibble: 1 × 4
#>   Device    Version Default Description                                         
#>   <chr>     <chr>   <lgl>   <chr>                                               
#> 1 Kronowise initial TRUE    Device format as it was initially implemented in Li…
#> 
#> $LIMO
#> # A tibble: 1 × 4
#>   Device Version Default Description                                            
#>   <chr>  <chr>   <lgl>   <chr>                                                  
#> 1 LIMO   initial TRUE    Device format as it was initially implemented in Light…
#> 
#> $LYS
#> # A tibble: 1 × 4
#>   Device Version Default Description                                            
#>   <chr>  <chr>   <lgl>   <chr>                                                  
#> 1 LYS    initial TRUE    Device format as it was initially implemented in Light…
#> 
#> $LiDo
#> # A tibble: 1 × 4
#>   Device Version Default Description                                            
#>   <chr>  <chr>   <lgl>   <chr>                                                  
#> 1 LiDo   initial TRUE    Device format as it was initially implemented in Light…
#> 
#> $LightWatcher
#> # A tibble: 1 × 4
#>   Device       Version Default Description                                      
#>   <chr>        <chr>   <lgl>   <chr>                                            
#> 1 LightWatcher initial TRUE    Device format as it was initially implemented in…
#> 
#> $MiEye
#> # A tibble: 1 × 4
#>   Device Version Default Description                                            
#>   <chr>  <chr>   <lgl>   <chr>                                                  
#> 1 MiEye  initial TRUE    Device format as it was initially implemented in Light…
#> 
#> $MotionWatch8
#> # A tibble: 1 × 4
#>   Device       Version Default Description                                      
#>   <chr>        <chr>   <lgl>   <chr>                                            
#> 1 MotionWatch8 initial TRUE    Device format as it was initially implemented in…
#> 
#> $OcuWEAR
#> # A tibble: 1 × 4
#>   Device  Version Default Description                                           
#>   <chr>   <chr>   <lgl>   <chr>                                                 
#> 1 OcuWEAR initial TRUE    Device format as it was initially implemented in Ligh…
#> 
#> $Speccy
#> # A tibble: 1 × 4
#>   Device Version Default Description                                            
#>   <chr>  <chr>   <lgl>   <chr>                                                  
#> 1 Speccy initial TRUE    Device format as it was initially implemented in Light…
#> 
#> $SpectraWear
#> # A tibble: 1 × 4
#>   Device      Version Default Description                                       
#>   <chr>       <chr>   <lgl>   <chr>                                             
#> 1 SpectraWear initial TRUE    Device format as it was initially implemented in …
#> 
#> $VEET
#> # A tibble: 2 × 4
#>   Device Version Default Description                                            
#>   <chr>  <chr>   <lgl>   <chr>                                                  
#> 1 VEET   initial FALSE   Device format as it was initially implemented in Light…
#> 2 VEET   2.1.7   TRUE    In firmware version 2.1.7 a change was introduced to t…
#> 
#> $nanoLambda
#> # A tibble: 1 × 4
#>   Device     Version Default Description                                        
#>   <chr>      <chr>   <lgl>   <chr>                                              
#> 1 nanoLambda initial TRUE    Device format as it was initially implemented in L…
#> 
```
