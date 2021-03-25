Intro
======

- Introduction to real time computer graphics / GPU programming

Difference GPU / CPU
====================

- Multiples "core" (10496 on RTX 3090!)
- Designed for SIMD (decode one instruction, schedule many executions)
- Hardware accelerated operations

Hardware Operations
===================

- Texture loading / decoding
- Tesselation
- Rope / Z buffer
- "Raytrace" ?

SIMD
====

- Describe simple operation
- Schedule its application on multiples data

Graphical Pipeline
===================

```
vertex (attribute) -> [Transformations] -> vertex (attribute+position) -> [Primitive assembly] -> primitive -> {Tesselation} -> fragments -> [Lighting] -> pixel color
```

Objets
======

- Buffers (with data)
- Textures
- VAO
- Shader

Vertex shader
===============

- generic attributes -> generic attributes + position

```glsl
in vec3 position;
in vec3 normal;

uniform mat4 transform;

out vec3 normal_vs;
out float distanceFromOrigin;

int main()
{
   gl_Position = transform * position;
   normal_vs = normal;
   distanceFromOrigin = dist(vec3(0,0,0), position);
}
```

Fragment Shader
===============

- fragment with interpolated attributes -> color

```glsl
in normal_vs;

out vec3 color;

uniform sample1D image;

void main()
{
    color = texture(image, distanceFromOrigin);
}
```

Geometry Shader
===============

I won't show an example

- Assemble vertex into primitives.
