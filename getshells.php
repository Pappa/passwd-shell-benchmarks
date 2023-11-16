#!/usr/bin/env php
# first cut at a php cli implementation

<?php
$shellcount = array();
$fh = fopen("passwd", "r");
if ($fh) {
    while (($line = fgets($fh)) !== false) {
        // process the line read.
        $tmp = substr($line, strrpos($line, ":") + 1);
        if (array_key_exists($tmp, $shellcount)) {
            $shellcount[$tmp] += 1;
        } else
            $shellcount[$tmp] = 1;
    }
    fclose($fh);

    foreach($shellcount as $key => $value) {
        $key = rtrim($key);
#        printf("%s\t%d\n", $key, $value);
	echo "$key: ";
	echo "$value\n";
    }
}
?> 

