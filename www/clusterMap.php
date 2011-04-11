
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

r-forge
 
<a href="http://www2.clustrmaps.com/counter/maps.php?url=http://rangemapper.r-forge.r-project.org/" id="clustrMapsLink"><img src="http://www2.clustrmaps.com/counter/index2.php?url=http://rangemapper.r-forge.r-project.org/" style="border:0px;" alt="Locations of visitors to this page" title="Locations of visitors to this page" id="clustrMapsImg" onerror="this.onerror=null; this.src='http://clustrmaps.com/images/clustrmaps-back-soon.jpg'; document.getElementById('clustrMapsLink').href='http://clustrmaps.com';" />
</a>


CRAN

<a href="http://www2.clustrmaps.com/counter/maps.php?url=http://cran.r-project.org/web/packages/rangeMapper/index.html" id="clustrMapsLink"><img src="http://www2.clustrmaps.com/counter/index2.php?url=http://cran.r-project.org/web/packages/rangeMapper/index.html" style="border:0px;" alt="Locations of visitors to this page" title="Locations of visitors to this page" id="clustrMapsImg" onerror="this.onerror=null; this.src='http://clustrmaps.com/images/clustrmaps-back-soon.jpg'; document.getElementById('clustrMapsLink').href='http://clustrmaps.com';" />
</a>



</body>
</html>

















