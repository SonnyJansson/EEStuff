#version 330 core
layout (location = 0) in vec3 vPos;

out vec3 color;

void main() {
    gl_Position = vec4(vPos, 1.0)
    color = vec4(0, 0, 0, 0.7)
}