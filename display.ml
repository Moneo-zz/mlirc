open Graphics

let row_height = 15
let offset=20


type row = {content:string;
	    mutable rank:int;
	   }


let clear_row r = 
 set_color white;
 fill_rect offset (row_height*r.rank) 1500 row_height;
 set_color black


let print_row r =
  clear_row r;
  moveto offset (row_height*r.rank);
  draw_string r.content



let rows=ref []

let number_of_rows = 60

let rec incr_rows res = function
	[] -> res
	|a::b when a.rank < number_of_rows-> 
			begin
				a.rank <- a.rank+1;
				 incr_rows (a::res) b
			end
	|a::b -> incr_rows res b


let add_row s= 
	rows:={content=s;rank=1}::(incr_rows [] !rows);
	List.iter print_row !rows
let init () = 
  Graphics.open_graph ":0 500x700"
