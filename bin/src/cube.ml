open Tgl4
open Result

let str = Printf.sprintf

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f -> f a; Int32.to_int a.{0};;

let set_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f i -> a.{0} <- Int32.of_int i; f a;;

let get_string len f =
  let a = bigarray_create Bigarray.char len in
  f a; Gl.string_of_bigarray a;;


let points = [|
  -0.5, 0.5, -0.5;
  0.5, 0.5, -0.5;
  -0.5, -0.5, -0.5;
  0.5, -0.5, -0.5;

  -0.5, 0.5, 0.5;
  0.5, 0.5, 0.5;
  -0.5, -0.5, 0.5;
  0.5, -0.5, 0.5;
  |];;

let vertices = bigarray_create Bigarray.float32 (Array.length points * 6);;

let () =
  let i = ref 0 in
  Array.iter (fun (x,y,z) ->
    vertices.{!i} <- x; incr i;
    vertices.{!i} <- y; incr i;
    vertices.{!i} <- z; incr i;
    vertices.{!i} <- x; incr i;
    vertices.{!i} <- y; incr i;
    vertices.{!i} <- z; incr i;
  ) points;;


let faces = [|
  0,1,2;
  1,2,3;

  0,2,4;
  2,4,6;

  4,5,6;
  5,6,7;

  1,3,5;
  3,5,7;

  0,1,4;
  1,4,5;

  2,3,6;
  3,6,7;
  |];;

let indices = bigarray_create Bigarray.int32 (Array.length faces * 3);;

let () =
  let i = ref 0 in
  Array.iter (fun (x,y,z) ->
    indices.{!i} <- Int32.of_int x; incr i;
    indices.{!i} <- Int32.of_int y; incr i;
    indices.{!i} <- Int32.of_int z; incr i;
  ) faces;;

let vertex_shader v = str "
  #version %s core
  in vec3 vertex;
  void main()
  {
    v_color = vec4(color, 1.0);
    gl_Position = vec4(vertex, 1.0);
  }" v

let fragment_shader v = str "
  #version %s core
  out vec4 color;
  void main() { color = vec4(1.0); }" v

let create_buffer buffer =
  let id = get_int (Gl.gen_buffers 1) in
  let bytes = Gl.bigarray_byte_size buffer in
  Gl.bind_buffer Gl.array_buffer id;
  Gl.buffer_data Gl.array_buffer bytes (Some buffer) Gl.static_draw;
  id;;

let delete_buffer buffer_id =
  set_int (Gl.delete_buffers 1) buffer_id;;

let create_geometry () =
  let geometry_id = get_int (Gl.gen_vertex_arrays 1) in
  let ibo = create_buffer indices in
  let vbo = create_buffer vertices in
  let bind_attrib id loc dim typ =
    Gl.bind_buffer Gl.array_buffer id;
    Gl.enable_vertex_attrib_array loc;
    Gl.vertex_attrib_pointer loc dim typ false 0 (`Offset 0);
  in
  Gl.bind_vertex_array geometry_id;
  Gl.bind_buffer Gl.element_array_buffer ibo;
  bind_attrib vbo 0 3 Gl.float;
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.bind_buffer Gl.element_array_buffer 0;
  (geometry_id, [ibo; vbo]);;

let delete_geometry geometry_id buffers_ids =
  set_int (Gl.delete_vertex_arrays 1) geometry_id;
  List.iter delete_buffer buffers_ids;;

let compile_shader src shader_type =
  let get_shader shader_id e = get_int (Gl.get_shaderiv shader_id e) in
  let shader_id = Gl.create_shader shader_type in
  Gl.shader_source shader_id src;
  Gl.compile_shader shader_id;
  (shader_id, get_shader shader_id Gl.compile_status);;

let create_program glsl_v =
  let vertex_id, _ = compile_shader (vertex_shader glsl_v) Gl.vertex_shader in
  let fragment_id, _ = compile_shader (fragment_shader glsl_v) Gl.fragment_shader in
  let program_id = Gl.create_program () in
  let get_program program_id e = get_int (Gl.get_programiv program_id e) in
  Gl.attach_shader program_id vertex_id; Gl.delete_shader vertex_id;
  Gl.attach_shader program_id fragment_id; Gl.delete_shader fragment_id;
  Gl.bind_attrib_location program_id 0 "vertex";
  Gl.link_program program_id;
  (program_id, get_program program_id Gl.link_status);;

let delete_program program_id =
  Gl.delete_program program_id; Ok ();;