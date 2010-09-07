
<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);

?>

<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
	
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
    <LINK href="style.css" rel="stylesheet" type="text/css"> 

  </head>

<body>


<h1>rangeMapper - A package for easy generation of biodiversity (species richness) or life-history traits maps using species geographical  ranges </h1>

<h2>  The rangeMapper project:  </h2> 

<ul class="tick">
<li> A suite of tools for easy generation of biodiversity (species richness) or life-history traits maps and, in general, maps of any variable associated with a species or population. </li>
<li> Performs range maps interpolation with a pre-defined grid, then computes at each grid cell a chosen statistical model. </li>
<li> Can be extended with any statistical model (from a simple average to mixed-effect and phylogenetic models) available in one of the existing R packages.</li>
<li> The resulting raster maps are stored in a rangeMapper project file (a pre-customized SQLite database) and can thus be displayed and/or manipulated at any latter stage.  </li>
<li> Is built on the framework provided by the sp, maptools and rgdal packages using sqlite support to store data. </li>
<li> Has a user-friendly platform-independent graphical user interface.  </li>
</ul>
 

 
<h2> Package vignette(s) :</h2>
<ul class="bull"> 
<li><a href="rangeMapper.pdf">Graphical user interface, quick guide.</a>  </li> 
</ul>

<h2> Download and install:</h2>
<ul class="bull"> 
<li><a href="https://r-forge.r-project.org/R/?group_id=856">Download page</a>  </li> 
<li>To install this package directly within R type: install.packages("rangeMapper", repos="http://R-Forge.R-project.org")  </li> 
</ul>
 

<h2> The main GUI</h2>
<img src="gui.png"  class="center" />


<h2> Examples:</h2>
<ul class="bull"> 
<li> Mammalian species richness </li> 
	<h3> The geographical range data of mammals of the world (5227 species) is available online at <a href="http://www.iucnredlist.org/technical-documents/spatial-data"> IUCN </a>. The rangeMapper project was created using a 1.5deg canvas. 
	</h3>
<img src="map1d.png"  class="center" />
</ul>
 









<hr class="a">

<h2> The project summary page you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"> here. </h2>



</body>
</html>

















