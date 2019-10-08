#version 430 core

in vec2 uv;
out vec4 fColor;

uniform sampler2D tex_00;

void main()
{
   fColor = texture(tex_00, uv);
   // vec4 texColor = texture(tex_00, uv);
   // if(texColor.a < 0.1)
   //   discard;
   // fColor = texColor;
}