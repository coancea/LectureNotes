CXX        = nvcc -O3

SRC1 = transpose-main.cu
SRC2 = mmm-main.cu
SOURCES_CPP = transpose-main.cu mmm-main.cu
HELPERS     = transpose-host.cu.h transpose-kernels.cu.h mmm-kernels.cu.h
EXECUTABLE1 =assignm3-task1
EXECUTABLE2 =assignm3-task2

default: compile run

.cu.o: $(SOURCES_CPP) $(HELPERS)
	$(CXX) -c $@ $<


compile: $(EXECUTABLE1) $(EXECUTABLE2)

$(EXECUTABLE1): $(SRC1) $(HELPERS)
	$(CXX) -o $(EXECUTABLE1) $(SRC1)

$(EXECUTABLE2): $(SRC2) $(HELPERS)
	$(CXX) -o $(EXECUTABLE2) $(SRC2)

run: $(EXECUTABLE1) $(EXECUTABLE2)
	./$(EXECUTABLE1)
	./$(EXECUTABLE2)

clean:
	rm -f $(EXECUTABLE1) $(EXECUTABLE2)

