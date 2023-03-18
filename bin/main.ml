open Tgl4

let () =
  (* Initialize the library *)
  GLFW.init ();
  at_exit GLFW.terminate;
  (* Create a windowed mode window and its OpenGL context *)
  let window = GLFW.createWindow ~width:640 ~height:480 ~title:"Hello World" () in
  (* Make the window's context current *)
  GLFW.makeContextCurrent ~window:(Some window);
  (* Loop until the user closes the window *)
  while not (GLFW.windowShouldClose ~window:window) do
    (* Render here *)
    Gl.clear_color 1. 0. 0. 1.;
    Gl.clear Gl.color_buffer_bit;
    (* Swap front and back buffers *)
    GLFW.swapBuffers ~window:window;
    (* Poll for and process events *)
    GLFW.pollEvents ()
  done
