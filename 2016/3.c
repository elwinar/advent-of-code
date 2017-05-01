#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

#define BUFFER_SIZE 255

bool triangle(int, int, int);

void main()
{
	int fd = open("./3.input", O_RDONLY);

	int sides[3];
	char c;
	int triangles = 0;
	int n;

	do {
		for (int i = 0; i < 3; i++) {
			sides[i] = 0;
		}

		for(int i = 0; i < 3; i++) {
			while (true) {
				n = read(fd, &c, 1);
				if (n == 0) {
					goto end;
				}

				if(c == ' ' || c == '\n') {
					if (sides[i] != 0) {
						break;
					} else {
						continue;
					}
				}

				sides[i] = sides[i]*10 + (c - '0');
			}
		}

		triangles += triangle(sides[0], sides[1], sides[2]);
	} while(true);
end:

	printf("%d", triangles);
}

bool triangle(int a, int b, int c)
{
	return !(a+b <= c || b+c <= a || a+c <= b);
}
