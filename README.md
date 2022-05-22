An experiment to make writing Vec3 code in zig easier by building a comptime
infix(!) math DSL.

Ideas:
* expand this in to a proper library with support for vec3/vec4/mat4
* build an embedded, compile-time APL-like language for ndarray shenanigans?
    * something like [`numpy.einsum`](https://numpy.org/doc/stable/reference/generated/numpy.einsum.html)?
* maybe support GPU code? Compile to OpenGL compute shader or something?

```zig
const E = CompileExpression("(x*y)+x*3-y");
const x = Vec3{ 1, 2, 3 };
const y = Vec3{ 3, 4, 5 };
const r = E.eval(.{ .x = x, .y = y });
```
