#include <stdio.h>

int input = 0;
int input2 = 0;
int intermediate = 0;
int output = 0;
int output2 = 0;

int* inputCell() {
    return &input;
}

int* outputCell() {
    return &output;
}

int* intermediateCell() {
    return &intermediate;
}

int* inputCell2() {
    return &input2;
}

int* outputCell2() {
    return &output2;
}

void cinput() {
    int x = scanf("%d", &input);
}

void coutput() {
    printf("in: %d, inter: %d, out: %d, in2: %d, out2: %d\n", input, intermediate, output, input2, output2);
}