CXX        = nvcc

SRC1 = scan-main.cu
SRC2 = mssp-main.cu
SRC3 = spMV-Mul-main.cu
SOURCES_CPP = scan-main.cu mssp-main.cu spMV-Mul-main.cu
HELPERS     =scan-host.cu.h scan-kernels.cu.h mssp-kernels.cu.h spMV-Mul-kernels.cu.h
EXECUTABLE1 =assignm2-task1
EXECUTABLE2 =assignm2-task2
EXECUTABLE3 =assignm2-task3

default: compile run

.cu.o: $(SOURCES_CPP) $(HELPERS)
	$(CXX) -c $@ $<


compile: $(EXECUTABLE1) $(EXECUTABLE2) $(EXECUTABLE3)

$(EXECUTABLE1): $(SRC1) $(HELPERS)
	$(CXX) -o $(EXECUTABLE1) $(SRC1)

$(EXECUTABLE2): $(SRC2) $(HELPERS)
	$(CXX) -o $(EXECUTABLE2) $(SRC2)

$(EXECUTABLE3): $(SRC3) $(HELPERS)
	$(CXX) -o $(EXECUTABLE3) $(SRC3)


run: $(EXECUTABLE1) $(EXECUTABLE1)
	./$(EXECUTABLE1)
	./$(EXECUTABLE2)
	./$(EXECUTABLE3)

clean:
	rm -f $(EXECUTABLE1) $(EXECUTABLE2)

