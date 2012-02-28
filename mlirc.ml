(*==================================================*)
(*             Parsing user input                   *)

type user_input = (*two types of inputs *)
	Command of string * string
	|Message of string

(*regexps for parsing command *)

let command = Str.regexp "/[^ ]*"
let space = Str.regexp " "

let strip_leading s = (*removes the first char*)
	String.sub s 1 (String.length s -1) 

let parse_user s = 
	let l = Str.split space s in 
		try 
			let a::b =  l in
				if Str.string_match command a 0 then 
					Command (strip_leading a,String.concat " " b)
				else
					Message s
  with _ -> Message s
  
(*==================================================*)
(*              Parsing server input                *)

type server_input =
	Ping of string 
	|Other of string

let cr = Str.regexp "\r"

let parse_server s = 
	let s = Str.global_replace cr "" s in
	let l =  Str.split space s in
		try
			let a::b = l in 
				if a = "PING" then 
					Ping (String.concat " " b) 
				else 
					Other s
		with _ -> Other s

(*==================================================*)
(*              Connecting to server                *)

let connect () = 
	let open Unix in 
		let server = Sys.argv.(1) in
		let port=int_of_string Sys.argv.(2) in 
		let addr = (gethostbyname server).h_addr_list.(0) in
		open_connection (ADDR_INET (addr,port))


(*==================================================*)
(*          Thread safe output functions            *)

open Printf
module M = Mutex

let m = M.create ()
(* thread safe writing on the connection socket *)

let print_command oc command param =
  M.lock m;
  fprintf oc "%s %s \r\n" command param  ;
  flush oc;
  M.unlock m

let print_message oc chan message = 
match chan with
	Some chan ->   
		M.lock m;
		fprintf oc "PRIVMSG %s :%s \r\n" chan message;
		flush oc;
		M.unlock m
	|None -> print_endline "You need to join a channel first !"
(*==================================================*)
(*                  Main loops                      *)


let rec read_stuff ic oc = (*read stuff from the socket *)
	let open Str in 
		 match parse_server (input_line ic) with 
			Ping s -> 
				print_command oc "PONG" s;
				read_stuff ic oc
			|Other s -> Display.add_row s;
				read_stuff ic oc

let init oc = 
	print_endline "please enter your nickname:" ;
	let nick = read_line () in 
		print_command oc "USER" "ocaml * * :school" ;
		print_command oc "NICK" nick
	
let rec print_stuff oc chan=
	match parse_user (read_line () ) with 
		Command (c,p) when c = "JOIN"-> 
			print_command oc c p ;
			print_stuff oc (Some p)
		|Command (c,p) -> 	
			print_command oc c p;
			print_stuff oc chan
		|Message m -> 
			print_message oc chan m ;
			 Display.add_row (sprintf "You sent: %s" m);
			print_stuff oc chan

let _ = 
	let ic,oc = connect () in 
		Display.init ();
		Thread.create (read_stuff ic) oc;
		init oc;
		print_endline "Client: connected" ; 
		print_stuff oc None
