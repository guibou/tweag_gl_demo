#version 440

out vec4 color;

layout(location=0) uniform vec2 iRes;
layout(location=1) uniform float iTime;
layout(location=2) uniform vec2 iMouse;

void main()
{
    if(length(gl_FragCoord.xy - iMouse) < 500 + 100 * cos(iTime))
    {
       color = vec4(gl_FragCoord.xy / iRes, 1, 1);
    }
    else
    {
       color = vec4(gl_FragCoord.yx / iRes, 0, 1);
    }
}
