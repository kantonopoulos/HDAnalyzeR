# HDAnalyzeR palettes

`hd_palettes()` returns a list of palettes used by the Human Disease
Blood Atlas resource of the Human Protein Atlas (HPA) project.

## Usage

``` r
hd_palettes()
```

## Value

List of HDAnalyzeR palettes.

## Details

Some of the palettes are custom palettes created for the Human Disease
Blood Atlas team. Others are taken from the `ggsci` package, which
provides palettes inspired by scientific journals.

## Examples

``` r
hd_palettes()
#> $sex_basic
#>         F         M 
#> "#C03830" "#317EC2" 
#> 
#> $sex
#>         F         M 
#> "#8a72be" "#A9D0EF" 
#> 
#> $diff_exp
#>  not significant significant down   significant up 
#>           "grey"        "#317EC2"        "#C03830" 
#> 
#> $cancers12
#>       AML       CLL     LYMPH      MYEL       CRC     LUNGC     GLIOM       BRC 
#> "#A6CEE3" "#2271B5" "#08585A" "#66C2A5" "#B89B74" "#ADC74F" "#FFD321" "#E8A29A" 
#>       CVX      ENDC       OVC       PRC 
#> "#9E0142" "#B195AE" "#603479" "#E7662B" 
#> 
#> $secreted
#>                      Secreted to blood                      Secreted in brain 
#>                              "#B30000"                              "#FFDD00" 
#>           Secreted to digestive system   Secreted in male reproductive system 
#>                              "#1280C4"                              "#95D4F5" 
#> Secreted in female reproductive system       Secreted to extracellular matrix 
#>                              "#F8BDD7"                              "#7F6A9C" 
#>              Secreted in other tissues            Secreted - unknown location 
#>                              "#FFD480"                              "#A1A8AA" 
#>             Intracellular and membrane                                Unknown 
#>                              "#F9A266"                               "grey80" 
#> 
#> $specificity
#>        Tissue enriched         Group enriched        Tissue enhanced 
#>              "#e41a1c"              "#FF9D00"              "#984ea3" 
#> Low tissue specificity          not detected  
#>               "grey40"                 "grey" 
#> 
#> $class
#>        Healthy Cardiovascular      Metabolic         Cancer    Psychiatric 
#>      "#B3B3B3"      "#FC8D62"      "#E5C494"      "#8DA0CB"      "#66C2A5" 
#>     Autoimmune      Infection      Pediatric 
#>      "#E78AC3"      "#FFD92F"      "#A6D854" 
#> 
#> $nrc
#>       Cinnabar    Shakespeare   PersianGreen       Chambray        Apricot 
#>      "#E64B35"      "#4DBBD5"      "#00A087"      "#3C5488"      "#F39B7F" 
#> WildBlueYonder     MonteCarlo          Monza    RomanCoffee       Sandrift 
#>      "#8491B4"      "#91D1C2"      "#DC0000"      "#7E6148"      "#B09C85" 
#> 
#> $aaas
#>      Chambray           Red      FunGreen   HoneyFlower          Teal 
#>     "#3B4992"     "#EE0000"     "#008B45"     "#631879"     "#008280" 
#>         Monza ButterflyBush FreshEggplant         Stack       CodGray 
#>     "#BB0021"     "#5F559B"     "#A20056"     "#808180"     "#1B1919" 
#> 
#> $nejm
#>      TallPoppy   DeepCerulean           Zest     Eucalyptus WildBlueYonder 
#>      "#BC3C29"      "#0072B5"      "#E18727"      "#20854E"      "#7876B1" 
#>         Gothic        Salomie     FrenchRose 
#>      "#6F99AD"      "#FFDC91"      "#EE4C97" 
#> 
#> $lancet
#> CongressBlue          Red        Apple    BondiBlue   TrendyPink     MonaLisa 
#>    "#00468B"    "#ED0000"    "#42B540"    "#0099B4"    "#925E9F"    "#FDAF91" 
#>      Carmine       Edward      CodGray 
#>    "#AD002A"    "#ADB6B6"    "#1B1919" 
#> 
#> $jama
#>  Limed Spruce         Anzac      Cerulean Apple Blossom      Acapulco 
#>     "#374E55"     "#DF8F44"     "#00A1D5"     "#B24745"     "#79AF97" 
#>      Kimberly        Makara 
#>     "#6A6599"     "#80796B" 
#> 
#> $bmj
#>      Blue    Yellow      Pink    Purple    Orange     Green      Aqua       Red 
#> "#2A6EBB" "#F0AB00" "#C50084" "#7D5CC6" "#E37222" "#69BE28" "#00B2A9" "#CD202C" 
#>      Grey 
#> "#747678" 
#> 
#> $jco
#>     Lochmara         Corn         Gray ChestnutRose       Danube    RegalBlue 
#>    "#0073C2"    "#EFC000"    "#868686"    "#CD534C"    "#7AA6DC"    "#003C67" 
#>        Olive    MineShaft     WellRead  KashmirBlue 
#>    "#8F7700"    "#3B3B3B"    "#A73030"    "#4A6990" 
#> 
#> $igv
#>      chr1      chr2      chr3      chr4      chr5      chr6      chr7      chr8 
#> "#5050FF" "#CE3D32" "#749B58" "#F0E685" "#466983" "#BA6338" "#5DB1DD" "#802268" 
#>      chr9     chr10     chr11     chr12     chr13     chr14     chr15     chr16 
#> "#6BD76B" "#D595A7" "#924822" "#837B8D" "#C75127" "#D58F5C" "#7A65A5" "#E4AF69" 
#>     chr17     chr18     chr19     chr20     chr21     chr22      chrX      chrY 
#> "#3B1B53" "#CDDEB7" "#612A79" "#AE1F63" "#E7C76F" "#5A655E" "#CC9900" "#99CC00" 
#>     chrUn     chr23     chr24     chr25     chr26     chr27     chr28     chr29 
#> "#A9A9A9" "#CC9900" "#99CC00" "#33CC00" "#00CC33" "#00CC99" "#0099CC" "#0A47FF" 
#>     chr30     chr31     chr32     chr33     chr34     chr35     chr36     chr37 
#> "#4775FF" "#FFC20A" "#FFD147" "#990033" "#991A00" "#996600" "#809900" "#339900" 
#>     chr38     chr39     chr40     chr41     chr42     chr43     chr44     chr45 
#> "#00991A" "#009966" "#008099" "#003399" "#1A0099" "#660099" "#990080" "#D60047" 
#>     chr46     chr47     chr48 
#> "#FF1463" "#00D68F" "#14FFB1" 
#> 
#> $uchicago
#>     Maroon   DarkGray     Yellow LightGreen       Blue     Orange        Red 
#>  "#800000"  "#767676"  "#FFA319"  "#8A9045"  "#155F83"  "#C16622"  "#8F3931" 
#>  DarkGreen     Violet 
#>  "#58593F"  "#350E20" 
#> 
```
