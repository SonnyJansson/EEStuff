#version 430 core

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec2 uvCoord;

out vec2 uv;

// uniform mat4 transform;
uniform mat4 modelMatrix;
uniform mat4 scaleMatrix;
uniform mat4 viewMatrix;
uniform mat4 projectionMatrix;

void main()
{
   gl_Position = projectionMatrix * viewMatrix * scaleMatrix * modelMatrix * vec4(vPosition, 1.0);
   // color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
   // color = vec4(uvCoord.x, uvCoord.y, 1.0, 1.0);
   uv = uvCoord;
}
