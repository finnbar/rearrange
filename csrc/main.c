#include <stdio.h>

int input = 0;
int intermediate = 0;
int output = 0;

int* inputCell() {
    return &input;
}

int* outputCell() {
    return &output;
}

int* intermediateCell() {
    return &intermediate;
}

void cinput() {
    int x = scanf("%d", &input);
}

void coutput() {
    printf("in: %d, inter: %d, out: %d\n", input, intermediate, output);
}