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

let incr_row r  =
   r.rank <- (r.rank + 1)



let rows = ref []
let add_row s= 
	List.iter incr_row !rows;
	rows:={content=s;rank=1}::!rows;
	List.iter print_row !rows

