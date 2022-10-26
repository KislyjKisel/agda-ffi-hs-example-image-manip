#version 430 core

layout(location = 0) in vec2 vPosition;
layout(location = 1) in vec2 vTexCoord;

out vec2 fTexCoord;

uniform vec2 uOffset;
uniform vec2 uScale;

void main() {
    gl_Position = vec4(uOffset + uScale * vPosition, 0.0, 1.0);
    fTexCoord = vTexCoord;
}
