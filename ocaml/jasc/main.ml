open JData;;
open ExtString;;
open ExtList;;
open IO.BigEndian;;

let _ =
  let fp = open_in_bin "a_org.txt" in
  let a = JReader.parse_class (IO.input_channel fp) in
  close_in fp;
  Format.printf "@.";
  Format.printf "%a\n" JData.pp_jclass a;
  Format.printf "**************** write@.";
  let fp = open_out_bin "a.class" in
  JWriter.encode_class (IO.output_channel fp) a;
  close_out fp;

  Format.printf "**************** read@.";
  let fp = open_in_bin "a.class" in
  let a = JReader.parse_class (IO.input_channel fp) in
  close_in fp;
  Format.printf "@.";
  Format.printf "%a@." JData.pp_jclass a;

  Format.printf "**************** methods@.";
  let methods = a.cmethods in
  List.iter (fun m ->
    let code = JCode.get_code m in
    let jcode = JCodeReader.parse_code a.constants code in
    Format.printf "jcode=%a@." JCode.pp_jcode jcode;
    let ctx = JWriter.new_ctx (IO.output_string ()) a.constants in
    JCodeWriter.encode_code ctx jcode;
    let ocode = IO.close_out ctx.ch in
    Format.printf "src code=%S@." code;
    Format.printf "dst code=%S@." ocode;
    assert (code = ocode);
  ) methods;
  Format.printf "**************** main end@.";
