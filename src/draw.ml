open Images;;

let draw_rect' x0 y0 w h = 
   let (a,b) = Graphics.current_point() in
   let () = Graphics.set_color Graphics.blue 
   and x1 = x0+w and y1 = y0+h 
   in
     Graphics.moveto x0 y0; 
     Graphics.lineto x0 y1; Graphics.lineto x1 y1;
     Graphics.lineto x1 y0; Graphics.lineto x0 y0; 
     Graphics.moveto a b;;

let example_drawing (_ : unit) =
  let width  = int_of_string Sys.argv.(1)
  and length = int_of_string Sys.argv.(2)
  and name   = Sys.argv.(3)
  and black = {Color.Rgb.r = 0; g=0; b=0; }
  and blue = {Color.Rgb.r = 0; g=0; b=255; }
  and white = {Color.Rgb.r = 255; g=255; b=255; } in
  Graphics.open_graph "";
  let () = draw_rect' 10 10 (width/2) (length /2) in 
  let color_arr = Graphics.dump_image (Graphics.get_image 0 0 width length) in 
  let image = Rgb24.make width length black in
  for i = 0 to width-1 do
     for j = 0 to (length) - 1 do
        let pix_color = color_arr. (j). (i) in
        if pix_color = Graphics.blue then 
          (Rgb24.set image i j blue;)
        else Rgb24.set image i j white;
     done;
  done;
  Bmp.save name [] (Images.Rgb24 image)