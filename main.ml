let limit = ref 1000

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
  if e = e' then e else
  iter (n - 1) e'

let lexbuf outchan binchan l = (* バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  let prog = 
    (RegAlloc.f
      (Simm.f
        (Virtual.f
            (Closure.f
              (iter !limit
                  (Alpha.f
                    (KNormal.f
                        (Typing.f
                          (Parser.exp Lexer.token l)))))))))
  in 
    Emit.f outchan prog(*;
    EmitBinary.f binchan prog*)

let string s = lexbuf stdout stdout (Lexing.from_string s) (* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)

let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  let binchan = open_out (f ^ ".bin") in
  try
    lexbuf outchan binchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
    close_out binchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let file2 f =   
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ "-1.log") in
  try
    Syntax.print_syntax outchan (snd(Parser.exp Lexer.token (Lexing.from_channel inchan))) 0;
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file2 f);ignore(file f))
    !files
