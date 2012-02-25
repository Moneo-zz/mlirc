open Unix 

let server = Sys.argv.(1)
let nick=Sys.argv.(2)
let addr = (gethostbyname server).h_addr_list.(0)
let s = socket PF_INET SOCK_STREAM 0 
let _ = clear_nonblock s;
connect s  (ADDR_INET (addr,6667))
let cin = in_channel_of_descr s
let cout = out_channel_of_descr s


open Printf
module M = Mutex

let m = M.create ()

let print_command s =
  M.lock m;
  fprintf cout "%s\r\n" s;
  M.unlock m



let rec read_stuff ()=
    let open Str in 
         let s = (input_line cin) in
	  let s = String.sub s 0 (String.length s -1) in
	 let a::b = split (regexp " ") s in 
         if a="PING" then (
           print_command (String.concat " " ("PONG"::b));
	   print_endline "PONG" );
         Display.add_row s;
         read_stuff ()
let _ = Graphics.open_graph ":0 600x700"
	
let _ = print_endline "Client: connected" 
let _ = Thread.create read_stuff () 
let _ = while true do 
   let s = read_line () in 
   print_command s
  done
