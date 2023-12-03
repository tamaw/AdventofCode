#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define BUFF_SIZE 100
#define MAX_NUMS 1000

int part1(FILE* fp, int* nums)
{
  char* line = malloc(BUFF_SIZE * sizeof(char));

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
  }

  return n;
}

int part2(FILE* fp, int* nums)
{
  const char* words[] = {"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};
  int wordLengths[9];
  char* line = malloc(BUFF_SIZE * sizeof(char));

  // precompute word legnths
  for(int w = 0; w < sizeof(words) / sizeof(words[0]); w++)
  {
    wordLengths[w] = strlen(words[w]);
  }

  int n;
  for(n = 0; (line = fgets(line, BUFF_SIZE, fp)); n++) {
    int letterCount[] = {0,0,0,0,0,0,0,0,0,0};
    int n0 = -1, n1 = -1;
    char c;

    for(int i = 0; (c = line[i]) != '\n'; i++) {
      char c = line[i];

      // number
      if(c >= '0' && c <= '9') {
        if(n0 == -1) n0 = c;
        else n1 = c;
      }

      // word search
      for(int j = 0; j < 9; j++) {

        if(c != words[j][letterCount[j]]) {
          letterCount[j] = 0;
        }

        if(c == words[j][letterCount[j]]) {
          if(++letterCount[j] == wordLengths[j]) // match
          {
            if(n0 == -1) n0 = j+'1';
            else n1 = j+'1';
            letterCount[j] = 0;
          }
        }

      }
    }

     // missing second value
     if(n1 == -1) n1 = n0;

     // concat and convert
     char* nvalue = (char[]){n0, n1, '\0'};
     nums[n] = atoi(nvalue);

  }

  return n;
}

int main(void)
{
  FILE* fp = fopen("day1input.txt", "r");
  int nums[MAX_NUMS];
  int sum = 0;

  // int n = part1(fp, nums);
  int n = part2(fp, nums);

  for(int i = 0; i < n; i++)
    sum += nums[i];

  printf("Sum: %d\n", sum);

  return 0;
}