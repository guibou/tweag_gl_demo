Intro
======

- Introduction to real time computer graphics / GPU programming

Difference GPU / CPU
====================

- Multiples "core" (10496 on RTX 3090!)
- Designed for SIMD (decode one instruction, schedule many executions)
- Hardware accelerated operations for graphics

Hardware Operations
===================

- Texture loading / decoding / filtering
- Rasterization
- Rope / Z buffer
- "Raytrace" ?

SIMD
====

- Describe simple operation
- Schedule its application on multiples data

GPU API
=======

- (Graphics) OpenGL, DirectX, Vulkan
- (Compute) OpenCL, Cuda

Graphical Pipeline
===================

```
vertex (attribute) -> [Transformations]
   -> vertex (attribute+position) -> [Primitive assembly]
      -> primitive -> {Rasterization}
        -> fragments -> [Lighting]
          -> pixel color
```

Objets
======

- Buffers (with data)
- Textures
- Shader
- VAO
- Framebuffer

```
// Setup the object

// Setup the current state

// Enable a shader pipeline

draw TRIANGLES 4000
```

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

Shaders
========

- * Vertex
- Tesselation 1
- Tesselation 2
- Geometry
- * Fragment

- Compute

Demo / code time
================
