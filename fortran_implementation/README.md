```markdown
# Cubic Lattice Growth

Ilkhom Abdurakhmanov  
28/06/2024

This is the source code for growing a cubic lattice using Fortran.

## Source Code

The source code is  `create_cubic_node_opt3.f90`. It is converted from create_node_dict.py which is developed by 
Max Galettis and located in
the parent folder.

## Compilation

To compile the source code, use the provided Makefile. In your terminal, run:

make
```

This will execute the following commands:

```sh
ftn -Wall -O3 -c create_cubic_node_opt3.f90
ftn -Wall -O3 -o opt3 create_cubic_node_opt3.o
```

The resulting executable will be named `opt3`.

## Running the Program

You can run the program with the following command:

```sh
./opt3 <X> <Y> <Z>
```

Replace `<X>`, `<Y>`, and `<Z>` with the desired dimensions for the cubic lattice.

### Example

To grow a lattice with dimensions 2x2x2, run:

```sh
./opt3 2 2 2
```

The output will display the points and their neighbors in the lattice. For example:

```
Grew lattice with 8 points.
Box contents:
point: (0, 0, 0) neighbours: {(1, 0, 0), (0, 1, 0), (0, 0, 1)}
point: (1, 0, 0) neighbours: {(1, 1, 0), (1, 0, 1), (0, 0, 0)}
point: (0, 1, 0) neighbours: {(1, 1, 0), (0, 1, 1), (0, 0, 0)}
point: (0, 0, 1) neighbours: {(1, 0, 1), (0, 1, 1), (0, 0, 0)}
point: (1, 1, 0) neighbours: {(1, 1, 1), (0, 1, 0), (1, 0, 0)}
point: (1, 0, 1) neighbours: {(1, 1, 1), (0, 0, 1), (1, 0, 0)}
point: (0, 1, 1) neighbours: {(1, 1, 1), (0, 0, 1), (0, 1, 0)}
point: (1, 1, 1) neighbours: {(0, 1, 1), (1, 0, 1), (1, 1, 0)}
```

### Performance analysis

Comparison of compute time between Fortran and Python implementations as a function of box size.
![Performance](box.png "Performance, Fortran vs Python")

Comparison of maximum resident set size (max RSS) memory requirements between Fortran and Python implementations as a function of box size.
![Memory](memory.png "Memory requirement, Fortran vs Python")

