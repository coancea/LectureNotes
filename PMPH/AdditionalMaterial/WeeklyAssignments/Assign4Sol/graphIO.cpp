#include "graphIO.h"
#include <stdlib.h>
#include <cstdlib>
#include <string>

Graph::Graph(Graph &g): N(g.N) {
	distMatrix = (float*)malloc(N*N*sizeof(float));
	memcpy(distMatrix, g.distMatrix, N*N*sizeof(float));
}

Graph::~Graph()
{
	if(N>0) free(distMatrix);

}

void Graph::writeToFile(const char* fName)
{
	std::ofstream outfile (fName);

	outfile.precision(1);
	for(int i=0;i<N;i++){
		for(int j=0;j<N;j++){
			float val = distMatrix[i*N+j];
			if(val>=FLOAT_INF)
				outfile<<"_ ";
			else
				outfile<<distMatrix[i*N+j]<<" ";
		}
		outfile<<'\n';
	}

	outfile.close();
}

void Graph::readFromFile(const char* fName)
{
	if(N!=0) 
		delete distMatrix;

	// First pass. Get number of vertices
	N = 0;
	std::ifstream infile(fName);
	std::string line;
	while (std::getline(infile, line))
	{
		if(line.find("v ")==0) N++;
	}
	infile.close();

	// Allocate matrix data
	distMatrix = (float*)malloc( N*N*sizeof(float) );
	for(int i=0;i<N*N;i++) 
		distMatrix[i] = FLOAT_INF;
	for(int i=0;i<N;i++) 
		distMatrix[i*N+i] = 0.0f;

	// Second pass. Read edges
	infile.open(fName);
	while (std::getline(infile, line))
	{
		if(line.find("e ")==0){
			std::istringstream iss(line);
			std::string e;
			int v1, v2;
			if (!(iss >> e >> v1 >> v2)) { std::cerr<<"Error parsing "<<line<<std::endl; continue; }
			if(v1<0 || v1>=N || v2<0 || v2>=N) { std::cerr<<"Error, out of bounds "<<line<<std::endl; continue; }

			distMatrix[v1*N + v2] = 1.0f;
		}
	}
	infile.close();
}
