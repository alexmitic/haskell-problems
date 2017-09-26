<?php
function b($n) {
    $list = array();
    $list[0] = 1;
    
    for ($m = 1; $m <= $n; $m++) { 
        $list[$m] = 0;
        for ($k=0; $k <= $m - 1; $k++) { 
            $list[$m] = $list[$m] - binom($m + 1, $k) * $list[$k];
        }
        $list[$m] = $list[$m]/($m + 1);
    }
    return $list[$n];
}

function binom($n, $k) {
    $r = 1;
    for ($i = 1; $i <= $k; $i++) { 
        $r = $r * ($n - $i + 1) / $i;
    }
    return $r;
}
?>