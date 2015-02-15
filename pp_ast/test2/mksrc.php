include "core.h"
include stdio.h

<?php


for($i = 1; $i <= 2000; $i++) {
    mkprog($i);
}

function mkprog($i) {
?>
    S<?=$i?> class()
<?php for($k = 1; $k <= rand(1, 5); $k++) { ?>
    S<?=$i?> :> S<?=$i?>child<?=$k?> (a:int)
<?php } $k1 = $k - 1;?>
  Trait<?=$i?> trait { trait<?=$i?>:()=>int }
  <?php for($k = 1; $k <= rand(1, $k1); $k++) { ?>
    Trait<?=$i?> :> S<?=$i?>child<?=$k?> { trait<?=$i?>():int = { return @a } }
  <?php } ?>
  <?php $ii = rand(1, $i); if($ii < $i) { ?>
    Trait<?=$ii?> :> S<?=$i?>child1 { trait<?=$ii?>():int = { return @a } }
  <?php } ?>
<?php
}
?>

main():int = {
  s:S1child1(22)

  printf("test %d\n", s:Trait1=>trait1())

  printf("ok\n")
  return 0
}
