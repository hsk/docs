<?php
if(count($argv) < 2) {
  echo "usase a.php mlfilename\n";
  exit(1);
}
$src = file_get_contents($argv[1]);
function lex($src) {
  $datas = array();
  $tok = "";
  $src = preg_replace_callback("/.*\\(\\*/", function($m) { return $m[0].strlen($m[0])."."; }, $src);
  $no = false;
  preg_replace_callback("/(\\(\\*|\\*\\)|\"(\\\\.|[^\"])*\"|[0-9]+\\.|.)/s", function($m)use(&$no, &$dts,&$datas,&$tok){
    if($m[1] == "(*") $no = true;
    if($m[1] == "(*" || $m[1] == "*)") {
      if($tok != "") $datas[] = $tok;
      $datas[] = $m[1];
      $tok = "";
    } else if($no) {
      $datas[] = $m[1];
      $no = false;
    } else {
      $tok .= $m[1];
    }
    return "";
  }, $src);
  if($tok != "") $datas[] = $tok;
  $datas[] = "";
  return $datas;
}

function parse($tokens) {
  $dts = "";
  while(true) {
    $token = array_shift($tokens);
    switch($token) {
    case "": return preg_replace("/^\\s+/s", "", $dts);
    case "(*":
      $no = array_shift($tokens);
      $token = parse_comment($tokens);
      $token = preg_replace("/^ */", "", $token);
      $token = preg_replace("/\\*\\)\$/", "", $token);
      $token = preg_replace("/^".str_repeat(" ", $no+1)."/m", "", $token);
      $dts.= "\n".$token."\n";
      break;
    case "*)": throw new Exception("error");
    default:
      $token = preg_replace("/^/m", "    ", $token);
      $dts .= $token;
    }
  }
}
function parse_comment(&$tokens) {
  $dts = "";
  while(true) {
    $token = array_shift($tokens);
    switch($token) {
    case "": throw new Exception("error found eof in comment");
    case "(*":
      array_shift($tokens);
      $dts .= "(*" . parse_comment($tokens); break;
    case "*)": return $dts . $token;
    default:
      $dts .= $token;
    }
  }
}

$tokens = lex($src);
try {
echo(parse($tokens));
} catch(Exception $e) {
  echo "error\n";
}
