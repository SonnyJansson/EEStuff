#version 430 core
layout (location = 0) in vec3 vPos;

out vec4 color;

void main()
{
//    gl_Position = vec4(vPos, 1.0)
    gl_Position = vec4(0, 0, 0, 1.0)
    color = vec4(1, 0, 0, 1)
}