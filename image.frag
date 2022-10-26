#version 430 core

in vec2 fTexCoord;

out vec4 FragColor;

uniform sampler2D uImageTexture;

void main() { // vec4(1.0, 0.0, 1.0, 1.0);
    FragColor = texture(uImageTexture, fTexCoord);
}
