let max_angle = 30.0

let thickness (level : int) = 
	level * 2

let branch_length (level : int) = 
	(Random.float 50.0) +. (float level *. 15.0)

let color_change = 10

let split_probability = 95

let clamp_color (c : int) : int = 
	if c > 255 then 255 else
	(if c < 0 then 0 else c)

let mutate (color : int * int * int) : int * int * int = 
	let r, g, b = color in
	let rand_dr = Random.int color_change + 25 in 
	let rand_dg = Random.int color_change + 25 in 
	let rand_db = Random.int color_change + 25 in
	let r_sign = Random.int 2 in 
	let g_sign = Random.int 2 in 
	let b_sign = Random.int 2 in 
	let dr = if r_sign = 1 then -rand_dr else rand_dr in
	let dg = if g_sign = 1 then -rand_dg else rand_dg in 
	let db = if b_sign = 1 then -rand_db else rand_db in 
	(clamp_color (r + dr), clamp_color (g + dg), clamp_color (b + db))

let line_endpoint (x : int) (y : int) (theta : float) (length : float) : int * int = 
	let pi = 4.0 *. atan 1.0 in 
	let theta_rad = (theta) /. (360.0) *. 2.0 *. pi in 
	let abs_dx = abs_float (length *. (sin theta_rad)) in 
	let dx = if theta < 0.0 then -. abs_dx else abs_dx in 
	let abs_dy = abs_float (length *. (cos theta_rad)) in 
	let dy = if (abs_float theta) > 90.0 then -. abs_dy else abs_dy in 
	(x + (int_of_float dx), y + (int_of_float dy)) 

let create_branch (x : int) (y : int) (theta : float) (level : int) (color : int*int*int) =
	Graphics.moveto x y; 
	Graphics.set_line_width (thickness level); 
	let r, g, b = color in 
	Graphics.set_color (Graphics.rgb r g b); 
	let length = branch_length level in
	let x', y' = line_endpoint x y theta length in
	Graphics.lineto x' y'; 
	(x', y')

let rec create_tree (x : int) (y : int) (theta : float) (num_levels : int) (color : int * int * int) : unit = 
	if num_levels = 0 then () else 
	let x', y' = create_branch x y theta num_levels color in 
	let branch_angle1 = -. (Random.float max_angle) in 
	let branch_angle2 = Random.float max_angle in 
	if Random.int 100 < split_probability then 
	(create_tree x' y' (branch_angle1 +. theta) (num_levels-1) (mutate color);
	create_tree x' y' (branch_angle2 +. theta) (num_levels-1) (mutate color)) else ()