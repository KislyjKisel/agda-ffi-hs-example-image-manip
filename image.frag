#version 430 core

uniform sampler2D uImageTexture;

in vec2 fTexCoord;

out vec4 FragColor;

void main() {
    FragColor = texture(uImageTexture, fTexCoord);
}
