
<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

?>

<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
	
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
	<LINK href="style.css" rel="stylesheet" type="text/css">

  </head>

<body>


rangeMapper is a suite of tools for easy generation of biodiversity (species richness) or life-history traits maps and, in general, maps of any variable associated with a species or population. rangeMapper performs range maps interpolation with a pre-defined grid, then computes at each grid cell a chosen statistical model. rangeMapper can be easily extended with any statistical model (from a simple average to mixed-effect and phylogenetic models) available in one of the existing R packages. The resulting raster maps are stored in a rangeMapper project file (a pre-customized SQLite database) and can thus be displayed and/or manipulated at any latter stage. rangeMapper is built on the framework provided by the sp, maptools and rgdal packages using sqlite support to store data. rangeMapper comes with a user-friendly platform-independent tcltk graphical user interface.


<p> <strong>Package vignette(s) :</strong> 
<ul>
<li> <a href="rangeMapper.pdf">Graphical user interface</a> 
</ul>




<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>

















