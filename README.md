# ParetoFrontMovie
Code to create a movie of the Pareto front, as it evolves over time, in R for up to six objectives. The objectives are three spatial dimensions, size, color, and transparency. The input file is assumed to be the Pareto front at different stages of the optimization, where the stages are separated by a given character.

The following R libraries are required:
  •plot3D (for making a 3-d plot using `scatter3D`)
  •grDevices (for some colors and transparency)
  •animation (for making the movie)
  •phytools (for color bars)

Depending on the machine you might need to install FFmpeg or avconv. If these are not installed when `saveVideo()` is called it can generate an error.

The general code is based on the function `makeParetoMovie()`. The first step within the function is reading-in a data set and formatting it into a data frame. After this, the size, color, and transparency objectives are converted into size, color, and transparency of points. The last portion is a large loop within `saveVideo()` that plots the data many times. There are options for rotating the plots as well. An example of the function call is given in `paretoFrontMovie6d.R`.

The code is still in beta test. Feedback on which parameters are not working or other functions/options to add are welcome.

Some example movies are in the .mp4 files, but you must download these files to view them because GitHub will not display them.

**This code is offered under the MIT License. See LICENSE for more details. Please cite this repository as (year of latest commit, commit ID, and date retrieved should be substituted into the format):**

Whealton, C.A. and J. Zatarain (\<year>). "ParetoFrontMovie-Code to create a movie of the Pareto Front in R for up to six objectives". GitHub repository. https://github.com/calvinwhealton/ParetoFrontMovie \<commit ID>, retrieved \<date>.
