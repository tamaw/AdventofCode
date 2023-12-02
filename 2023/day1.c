#include <stdio.h>
#include <stdlib.h>
#define BUFF_SIZE 100
#define MAX_NUMS 1000

int main(void)
{
  FILE* fp = fopen("day1input.txt", "r");
  char* line = malloc(BUFF_SIZE * sizeof(char));
  int nums[MAX_NUMS];
  int sum = 0;

  int n;
  for(n = 0; (line = fgets(line, BUFF_SIZE, fp)); n++) {
    int c, n0 = -1, n1 = -1;

    for(int i = 0; (c = line[i]) != '\n'; i++) {
      if(c >= '0' && c <= '9') {
        if(n0 == -1) n0 = c;
        else n1 = c;
      }
    }

    // no second value found
    if(n1 == -1) n1 = n0;

    char* nvalue = (char[]){n0, n1, '\0'};
    nums[n] = atoi(nvalue);
    printf("n0: %c, n1: %c, n: %d\n", n0, n1, nums[n]);
  }

  for(int i = 0; i < n; i++)
    sum += nums[i];

  printf("Sum: %d\n", sum);

  return 0;
}