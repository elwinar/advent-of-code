#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NORTH 0
#define EAST 1
#define SOUTH 2
#define WEST 3

#define RIGHT 'R'
#define LEFT 'L'

void main()
{
	char plan[] = "R3, L5, R2, L1, L2, R5, L2, R2, L2, L2, L1, R2, L2, R4, R4, R1, L2, L3, R3, L1, R2, L2, L4, R4, R5, L3, R3, L3, L3, R4, R5, L3, R3, L5, L1, L2, R2, L1, R3, R1, L1, R187, L1, R2, R47, L5, L1, L2, R4, R3, L3, R3, R4, R1, R3, L1, L4, L1, R2, L1, R4, R5, L1, R77, L5, L4, R3, L2, R4, R5, R5, L2, L2, R2, R5, L2, R194, R5, L2, R4, L5, L4, L2, R5, L3, L2, L5, R5, R2, L3, R3, R1, L4, R2, L1, R5, L1, R5, L1, L1, R3, L1, R5, R2, R5, R5, L4, L5, L5, L5, R3, L2, L5, L4, R3, R1, R1, R4, L2, L4, R5, R5, R4, L2, L2, R5, R5, L5, L2, R4, R4, L4, R1, L3, R1, L1, L1, L1, L4, R5, R4, L4, L4, R5, R3, L2, L2, R3, R1, R4, L3, R1, L4, R3, L3, L2, R2, R2, R2, L1, L4, R3, R2, R2, L3, R2, L3, L2, R4, L2, R3, L4, R5, R4, R1, R5, R3";

	int azimuth = NORTH;
	int distances[4] = {0, 0, 0, 0};

	int i = 0;
	do {
		// Skip spaces, commas, etc.
		if (plan[i] != LEFT && plan[i] != RIGHT && plan[i] != '\0') {
			i++;
			continue;
		}

		// Update the azmuth.
		if (plan[i] == RIGHT) {
			azimuth += 1;
		} else {
			azimuth += 3;
		}
		azimuth = azimuth % 4;
		i++;

		// Read the distance.
		int distance = 0;
		while (plan[i] >= '0' && plan[i] <= '9') {
			distance = distance * 10 + plan[i] - '0';
			i++;
		}

		distances[azimuth] += distance;
	} while (i < strlen(plan));

	printf("%d", abs(distances[NORTH] - distances[SOUTH]) + abs(distances[EAST] - distances[WEST]));
}
