open Thistlethwaite_module;;
open Utils_module;;
open Graphics;;

let rect_size = 50;;
let window_width = 500;;
let window_height = 500;;

let draw_cube (cube: Rubiks_cube.rubiks_cube) =
  for row = 0 to 2 do 
    for col = 0 to 2 do 
      let r,g,b = Utils.rgb_of_color (cube#get_facette_color UP row col) in
      set_color (rgb r g b);
      fill_rect ((col * rect_size) + 225) ((row * rect_size) + 375) rect_size rect_size;
      set_color (rgb 0 0 0);
      draw_rect ((col * rect_size) + 225) ((row * rect_size) + 375) rect_size rect_size;
    done;
  done;

  for row = 0 to 2 do 
    for col = 0 to 2 do 
      let r,g,b = Utils.rgb_of_color (cube#get_facette_color LEFT row col) in
      set_color (rgb r g b);
      fill_rect ((col * rect_size) + 75) ((row * rect_size) + 225) rect_size rect_size;
      set_color (rgb 0 0 0);
      draw_rect ((col * rect_size) + 75) ((row * rect_size) + 225) rect_size rect_size;
    done;
  done;

  for row = 0 to 2 do 
    for col = 0 to 2 do 
      let r,g,b = Utils.rgb_of_color (cube#get_facette_color FRONT row col) in
      set_color (rgb r g b);
      fill_rect ((col * rect_size) + (75 * 3)) ((row * rect_size) + 225) rect_size rect_size;
      set_color (rgb 0 0 0);
      draw_rect ((col * rect_size) + (75 * 3)) ((row * rect_size) + 225) rect_size rect_size;
    done;
  done;

  for row = 0 to 2 do 
    for col = 0 to 2 do 
      let r,g,b = Utils.rgb_of_color (cube#get_facette_color RIGHT row col) in
      set_color (rgb r g b);
      fill_rect ((col * rect_size) + (75 * 5)) ((row * rect_size) + 225) rect_size rect_size;
      set_color (rgb 0 0 0);
      draw_rect ((col * rect_size) + (75 * 5)) ((row * rect_size) + 225) rect_size rect_size;
    done;
  done;

  for row = 0 to 2 do 
    for col = 0 to 2 do 
      let r,g,b = Utils.rgb_of_color (cube#get_facette_color BACK row col) in
      set_color (rgb r g b);
      fill_rect ((col * rect_size) + (75 * 7)) ((row * rect_size) + 225) rect_size rect_size;
      set_color (rgb 0 0 0);
      draw_rect ((col * rect_size) + (75 * 7)) ((row * rect_size) + 225) rect_size rect_size;
    done;
  done;


  for row = 0 to 2 do 
    for col = 0 to 2 do 
      let r,g,b = Utils.rgb_of_color (cube#get_facette_color DOWN row col) in
      set_color (rgb r g b);
      fill_rect ((col * rect_size) + 225) ((row * rect_size) + 75) rect_size rect_size;
      set_color (rgb 0 0 0);
      draw_rect ((col * rect_size) + 225) ((row * rect_size) + 75) rect_size rect_size;
    done;
  done;
;;