#version 440

layout(location=0) uniform mat4 trans;

layout(location=0) in vec3 pos;


void main()
{
  gl_Position = trans * vec4(pos * 5, 1);
}
