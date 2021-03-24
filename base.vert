#version 440

layout(location=0) in vec3 pos;
layout(location=0) uniform mat4 trans;

out vec3 pos_vert;

void main()
{
  gl_Position = trans * vec4(pos, 1);
  pos_vert = pos;
}
