#include "stdio.h"

// Code by Aleksandar Mitic and Oskar Nehlin
// 
// Works for n < 141 and is has worst case complexity 1 * o(n)

int main(void) {
    
    int n;

    scanf("%d", &n);

    if (n < 21) {
        printf("single %d\n", n);
    } else {
        int i = 20;
        int added = 0;
        int arr[6];
        char toPrint = 1;

        while (i > 0) {

            if (n == 0) {
                break;
            } else if (added == 6) {
                toPrint = 0;
                break;
            }

            if (n >= 3*i) {
                arr[added] = 3;
                arr[added + 1] = i;
                n -= 3*i;
                added += 2;
            } else if (n >= 2*i) {
                arr[added] = 2;
                arr[added + 1] = i;
                n -= 2*i;
                added += 2;
            } else {
                arr[added] = 1;
                arr[added + 1] = i;
                n -= i;
                added += 2;
            }

            if (n <= i) {
                i = n;
            }
        }

        if (toPrint == 0) {
            printf("%s\n", "impossible");
        } else {
            for (int i = 0; i < added; i += 2) {
                if (arr[i] == 3) {
                    printf("triple %d\n", arr[i + 1]);
                } else if (arr[i] == 2) {
                    printf("double %d\n", arr[i + 1]);
                } else {
                    printf("single %d\n", arr[i + 1]);
                }
            }
        }
    }

    return 0;
}