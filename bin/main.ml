open Tgl4

let () =

  GLFW.init ();
  at_exit GLFW.terminate;

  let window = GLFW.createWindow ~width:640 ~height:480 ~title:"Hello World" () in

  GLFW.makeContextCurrent ~window:(Some window);
  GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:4;
  GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:5;

  let geometry_id, _ = Cube.create_geometry () in
  let program_id, _ = Cube.create_program "450" in

  while not (GLFW.windowShouldClose ~window:window) do

    Gl.clear_color 0. 0. 0. 1.;
    Gl.clear Gl.color_buffer_bit;
  
    Gl.use_program program_id;
    Gl.bind_vertex_array geometry_id;
    Gl.draw_elements Gl.triangles 36 Gl.unsigned_int (`Offset 0);
    Gl.bind_vertex_array 0;
    Gl.use_program 0;
  
    GLFW.swapBuffers ~window:window;
  
    GLFW.pollEvents ()
  done

