This file provides the documentation to the main function used to create the movie. There are several subsidiary functions, but their inputs are defined in the call to this main function.

Notes:

1. Either `T` and `F` or `TRUE` and `FALSE` can be used, but the default entries in this code are using `T` and `F` because it is shorter.

2. The function `rep(x,n)` repeats `x` (number, character, string, boolean, etc.) `n` times to form a vector and it is used for more concise assignment in some examples.

3. Most plot and text functions in `R` have a `cex` parameter that stands for character expansion. One can make text bigger by increasing the `cex`. If you want to increase text in part of the movie look at the variables with "cex" in the name.

------
## `makeParetoMovie()`

**_Inputs_**:

_Input Data File Variables_

**fname**: Name of input file as a string/character. File is assumed to be in the current working directory.

**datSep**: Separator for the "generations" in the input data file. This is the single character on a line that is used to break the input file for the Pareto Front at different time periods. Default `datSep='#'`.

_Problem Objectives Variables_

**objs**: Vector column number objectives for x-axis, y-axis, z-axis, color, size, and transparency (ordered). Default `objs=c(1,2,3,4,5,6)` meaning the the objectives will be plotted in order.

**mnmx**: Vector of whether objectives are minimization or maximization corresponding to the same order as the columns in the input file. Default `mnmx=rep('max',6)` because the code assumes all objectives are maximization. If any objective was minimized then substitute `min`. Minimization objectives will be multiplied by -1 (assumed they were below 0).

**objNames**: Names of the objectives. Default `objNames=NULL` assigns names 'Obj1', 'Obj2',...,'Obj6'. Can be a vector of six character strings and the names will be assigned in the order of the input columns.

_Ideal Point Definition & Inclusion in Plot_

**ideal**: Ideal point. Default `ideal=NULL` means that there is no ideal point and the maximum value of the objectives is used.

**idealAdd**: Add ideal point to plot. Defalult `idealAdd = F` for no ideal point. Will use maximum of objectives if ideal not specified. Size is not used.

**idealPch**: Plot symbol for the ideal. Default `idealPch = 8` for a star.

**idealMult**: Multiplier for the ideal point pch. Default `idealMult = 10`.

_Color Objective Variables_

**colN**: Number of colors used in the color bar: Default `colN=15`. Only used when `colPal=NULL`.

**colPal**: Color palette. Default `colPal = NULL` uses a red-orange-green-blue scale with the number of colors specified by `nCols`. A color palette can also be specified using built-in palettes or user-defined palettes.

_Size Objective Variables for Scaling_

**sizeMin**: Minimum size for points. Default `sizeMin = 0.1` so points where the size scaling is less than 0.1 will be assigned a value of 0.1. This controls how the small size objective points will be visualized (or not visualized).

**sizePow**: Power used when scaling the size objective. Default `sizePow = 1/3` meaning that the point radius is proportional to the third root of the objective. Setting `sizePow = 1` means that the radius would be proportional to the objective.

_Transparency Objective Variables for Scaling_

**transMin**: Minimum transparency. Default `transMin = 0.05` for 95% transparency (5% opacity).

**transMax**: Maximum transparency. Default `transMax = 1` for 0% transparency (100% opacity).

_Points Variables_

**pchOutline**: Should the plot symbol have an outline. Default `pchOutline = T` to select points with outlines.

**pchType**: Type of plot symbol. Default `pchType = 'circle'` for circular/spherical points. Other options include 'square', 'triangle', and 'diamond'.

**pchMult**: Multiplyer of the plot symbols. Default `pchMult = 5`. Increase value for larger points.

_Animation Variables_

**aniH**: Animation height in pixels. Default `aniH = 800`. Increase value for larger vertical dimension.

**aniW**: Animation width in pixels. Default `aniW = 800`. Increase value for larger horizontal dimension.

**aniName**: Animation name. Default `aniName = 'ParetoFront.mp4'`. Must use a format consistent with the `saveVideo()` function.

**aniInit**: Interval between frames in the file in seconds. Default `aniInt = 0.1`. Increase value to slow-down the video.

**aniFramEx**: Number of extra frames added when the rotation is at its extents. Default `aniFramEx = 10`. Increase value to increase length of "pause" when at the extent of rotation.

**aniRot**: Parameter for whether all or only the last Pareto front should be rotated. Default `aniRot = 'last'` meaning only the last Pareto front will rotate. Option 'all' will rotate all of the Pareto fronts.

**aniRotFrames**: Number of frames for the non-rotated Pareto fronts. Default `aniRotFrames = 20`. Increase value to give more time per non-rotated Pareto front.

**aniNotRotAng**: Plot angles (theta, phi in `scatter3D`) in degrees for the non-rotated plots. Default `aniNonRotAng = 40`.

_General Plot Layout Variables_

**ptMain**: Title of plot. Default `ptMain = 'Pareto Front'`. Change to a character string.

**ptMainCex**: Chararacter expansion fo the title. Default `ptMainCex = 2`. Increase value to make plot title larger.

**ptMainLine**: Line at which to write the title `ptMain`. Default `ptMainLine = -7`. Decreasing values moves the title closer to the plot.

**ptAngle**: Angles, in degrees, to be swept through when rotating (theta and phi in `scatter3d()`). Default `ptAngle = seq(9,81,1)`.

**ptTick**: Controlling if the tick marks on the axes are plotted. Default `ptTick = T` to plot the tick marks.

**ptLabTick**: Controlling if tick tables are plotted. Default `ptLabTick = F` to not plot the tick mark labels.

**ptCexLab**: Character expansion for the axis labels. Default `ptCexLab = 1`. Increase value to make labels larger.

_Point Size Legend_

**legSizeVals**: Size values in size legend. Default `legSizeVals = NULL` uses three standard size values for the legend.

**legLoc**: Location of the size legend within the plot for the legend. Default `legLoc=c(0.5,0.7)`.

**legCol**: Color of the size legend points. Default `legCol = 'black'`.

**legCex**: Character expansion for size legend. Default `legCex = 2`. Larger values make the text larger.

**legyinsp**: Verical separation of entries in size legend. Default `legyinsp = 2`. Increasing the value will increase the spacing.

**legHoriz**: Controls if the size legend is horizontal. Default `legHoriz = F`.

_Color Bar Legend_

**cbLen**: Length of the color bar within the subplot for color bars. Default `cbLen = 0.6`. Increasing the value will increase the length of the color bar.

**cbx**: Horizontal (x) location of the color bars within the subplot. Default `cbx = 0.2`.

**cby**: Vertical (y) location of the first color bar within the subplot. Default `cby = 0.7`.

**cbdy**: Vertical spacing between color bars within the subplot. Defafult `cbdy = -0.1`.

**cblwd**: Line weight (thickness) of the color bar. Default `cblwd = 10`. Increasing the value increases the thickness.

**cbout**: Controls whether an outline box is drawn around the color bar. Default `cbout = F` (no color bar).

**cbDig**: Number of digits for the color bar display. Default `cbDig = 2`.

**cbCex**: Character expansion for the color bar. Default `cbCex = 1.5`. Increasing the values increaes the size of the text.

_Transparency Legend_

**tspCex**: Character expansion for the transparency values. Default `tspCex = 1.5`. Increasing the values increaes the size of the text.

**tspLevs**: Transparency values for the legend in terms of the orignial transparency objective. Default `tspLevs = NULL` uses default values based on quantiles.
