#version 440

in vec3 pos_vert;
out vec4 color;

void main()
{
    vec3 norm = normalize(cross(dFdx(pos_vert), dFdy(pos_vert)));

    color = vec4(abs(norm), 1);
}
