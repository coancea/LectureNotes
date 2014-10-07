#ifndef GRAPHIO_H
#define GRAPHIO_H

#include <iostream>
#include <sstream>
#include <fstream>
#include <string.h>

#define FLOAT_INF 1000.0f

class Graph
{
	public:
		int N; ///< Number of vertices
		float* distMatrix; ///< NxN distance matrix. Unconnected vertices will have a FLOAT_INF entry.

		/** Initialize empty graph */
		Graph(): N(0) {}

		/** Copy constructor */
		Graph(Graph &g);

		/** Destructor */
		~Graph();

		/** Read distance matrix into graph */
		void readFromFile(const char* fName);

		/** Write distance matrix to file */
		void writeToFile(const char* fName);

};


#endif
