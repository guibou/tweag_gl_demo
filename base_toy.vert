#version 440

void main()
{
  vec2 pos = vec2(gl_VertexID / 2, mod(gl_VertexID, 2));

  gl_Position = vec4(pos * 2 - 1, 0, 1);
}
