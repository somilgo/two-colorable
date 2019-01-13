let width = 1000
let length = 1000

let () = 
	Random.self_init ();
	Graphics.open_graph " 1000x1000+50-0";
	Graphics.set_color Graphics.black;
	Graphics.fill_rect 0 0 width length; 
	let random_color = (Random.int 255, Random.int 255, Random.int 255) in 
	Tree.create_tree (width/2) 0 0.0 10 random_color;
	let _ = read_line () in 
	Draw.render_tree width length "output.bmp"
