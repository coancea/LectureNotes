CXX        = nvcc

SOURCES_CPP =ScanMainGen.cu
HELPERS     =ScanHost.cu.h ScanKernels.cu.h SparseMatVctMult.cu.h MsspProblem.cu.h ExclScan.cu.h
EXECUTABLE  =assignm2


default: compile

.cu.o: $(SOURCES_CPP) $(HELPERS)
	$(CXX) -c $@ $<


compile: $(EXECUTABLE)
$(EXECUTABLE): 
	$(CXX) -o $(EXECUTABLE) $(SOURCES_CPP)


run: $(EXECUTABLE)
	./$(EXECUTABLE)

clean:
	rm -f $(EXECUTABLE)

