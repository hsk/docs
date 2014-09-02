#!/usr/local/bin/php
<?php
$name = "あなたぁ";
$fp = fsockopen("localhost", 9801, $errno, $errstr, 30);
if (!$fp) {echo "$errstr ($errno)<br />\n"; exit(0); }

$out = "NOTIFY SSTP/1.1\r\n";
$out .= "Sender: test\r\n";
$out .= "Charset: UTF-8\r\n";

$data = array(
  "\\0\\s2おおー、コミットしたとは。えらい！褒めて使わす\\e",
  "\\0\\s1{$name}素敵ーw\\e",
  "\\0\\s3{$name}。がんばりすぎじゃなーい？\\e",
  "\\0\\s4はぁ、すごすぎ\\e",
  "\\0\\s5いいかんじだね！\\e",
  "\\0\\s6ほほー、やるなー\\e",
  "\\0\\s7ちょっと！すごすぎでしょ！\\e",
  "\\0\\s8もう、なんというか、、、。しごとできるー\\e",
  "\\0\\s9おいおい、ちゃんと、チェックしてるんだろうな\\e",
  "\\0\\s1え、なに？私のおかげで、さぎょうがはかどる？照れるなぁ。\\e",
  "\\0\\s0いい調子！いい調子！\\e",
);

$a = rand(0,count($data)-1);

$out .= "Script: ".$data[$a];

$out .= "\r\n";
fwrite($fp, $out);
fclose($fp);
