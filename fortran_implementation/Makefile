# Makefile to compile create_cubic_node_opt3.f90 

# Compiler
FC = ftn

# Compiler flags
FFLAGS = -Wall -O3

# Target executable
TARGET = opt3

# Source files
SRC = create_cubic_node_opt3.f90

# Object files
OBJ = $(SRC:.f90=.o)

# Default rule: compile and link the program
all: $(TARGET)

# Rule to link the object file into the executable
$(TARGET): $(OBJ)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJ)

# Rule to compile the source file into an object file
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

# Clean rule to remove the executable and object files
clean:
	rm -f $(TARGET) $(OBJ)

# Phony targets
.PHONY: all clean

