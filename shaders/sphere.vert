#version 430 core

layout(location = 0) in vec3 vPosition;

out vec4 color;

void main()
{
   gl_Position = vec4(vPosition, 1.0);
   color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}