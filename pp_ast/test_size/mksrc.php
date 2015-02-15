<?php


for($i = 0; $i < 2000; $i++) {
    mkprog($i);
}

function mkprog($i) {
?>


<?php for($k = 1; $k < rand(1, 5); $k++) { ?>
    S<?=$i?> <: S<?=$i?>child<?=$k?> class(a:int)
<?php } ?>

  Trait<?=$i?> trait { trait<?=$i?>:()->int }
  <?php for($k = 1; $k < rand(1, 5); $k++) { ?>

    S<?=$i?>child<?=$k?> <: Trait<?=$i?> { trait<?=$i?>():int = { return @a->int } }

  <?php } ?>

  <?php $ii = rand(0, $i); $jj = rand(0, $i); ?>
    S<?=$jj?>child1 <: Trait<?=$ii?> { trait<?=$ii?>():int = { return @a->int } }
<?php
}

