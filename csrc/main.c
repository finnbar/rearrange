#include <stdio.h>

int input = 0;
int input2 = 0;
int output = 0;
int output2 = 0;
int output3 = 0;

int* inputCell() {
    return &input;
}

int* inputCell2() {
    return &input2;
}

int* outputCell() {
    return &output;
}

int* outputCell2() {
    return &output2;
}

int* outputCell3() {
    return &output3;
}

void cinput() {
    printf("Enter first input:\n");
    int x = scanf("%d", &input);
    printf("Enter second input:\n");
    x = scanf("%d", &input2);
}