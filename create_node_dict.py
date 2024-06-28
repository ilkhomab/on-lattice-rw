#create_node_dict.py
# Max Galettis
#14/03/2024


#only do these:
#tetra hedron
#cube
#octahedron
#octahedron + 2*cube #no doesnt work, you get intersections
###########################################################dodecahedrons do not form a lattice, last 2 weeks have been for nothing ahhhhhhhhhhhhhhhhhhh


#################################IT WAS ALL A LIE, DONT USE THE PLATONIC SOLIDS
#octahedron has central height 2, but side height of sqrt(2)
#trunctated dodecahedron

#wait, i can probably compute it for the lattice keeping the 4th dimension constant, since those direction vectors each have a complement
#first i have to check that getting to the path via other directions is possible...



import pickle
from Tree_Node import TreeNode #import the treenode class form the file tree_node.py in the same directory
import os
import time
import numpy as np #needed for the sqrt(5)
import sys


#tetrahedron, dictionary of sets
tetrahedron_lattice_perturbations = {
    0: {
        (1,1,1, 1),
        (1,-1,-1, 1),
        (-1,1,-1, 1),
        (-1,-1,1, 1)
        },
    1: {
        (-1,-1,-1, -1),
        (-1,1,1, -1),
        (1,-1,1, -1),
        (1,1,-1, -1)
        }
    } #0 is a, 1 is b, it always switches...

#cube, set
cube_lattice_perturbations = {
    (1,0,0),
    (-1,0,0),
    (0,1,0),
    (0,-1,0),
    (0,0,1),
    (0,0,-1)
    }

#octahedron, set
octahedron_lattice_perturbations = {
    (1,1,1),
    (1,1,-1),
    (1,-1,1),
    (1,-1,-1),
    (-1,1,1),
    (-1,1,-1),
    (-1,-1,1),
    (-1,-1,-1)
    }


'''These dont work
#dodecahedron, dictionary of sets
#[1,phi], i should change this to np.array(1,phi) sort of thing... that way they can be summed faster?
dodecahedron_lattice_perturbations = {
    0: {
        ((0,0),(1,0),(0,1), 1),
        ((0,0),(1,0),(0,-1), 1),
        ((0,0),(-1,0),(0,1), 1),
        ((0,0),(-1,0),(0,-1), 1),
        ((1,0),(0,1),(0,0), 1),
        ((1,0),(0,-1),(0,0), 1),
        ((-1,0),(0,1),(0,0), 1),
        ((-1,0),(0,-1),(0,0), 1),
        ((0,1),(0,0),(1,0), 1),
        ((0,1),(0,0),(-1,0), 1),
        ((0,-1),(0,0),(1,0), 1),
        ((0,-1),(0,0),(-1,0), 1)
        },
    1: {
        ((0,0),(0,1),(1,0), -1),
        ((0,0),(0,1),(-1,0), -1),
        ((0,0),(0,-1),(1,0), -1),
        ((0,0),(0,-1),(-1,0), -1),
        ((0,1),(1,0),(0,0), -1),
        ((0,1),(-1,0),(0,0), -1),
        ((0,-1),(1,0),(0,0), -1),
        ((0,-1),(-1,0),(0,0), -1),
        ((1,0),(0,0),(0,1), -1),
        ((1,0),(0,0),(0,-1), -1),
        ((-1,0),(0,0),(0,1), -1),
        ((-1,0),(0,0),(0,-1), -1)
        }
    } #0 is a, 1 is b

#icosahedron, dictionary of sets of tuples with tuples and ints
#note, 1/phi + 1 = phi, and hence we can write [1,1] instead of [0,0,1] where values correspond to [1/phi, 1]
#[1/phi, 1], phi=[1,1]
icosahedron_lattice_perturbations = {
    0: {
        ((0,1),(0,1),(0,1), 1),
        ((0,1),(0,1),(0,-1), 1),
        ((0,1),(0,-1),(0,1), 1),
        ((0,1),(0,-1),(0,-1), 1),
        ((0,-1),(0,1),(0,1), 1),
        ((0,-1),(0,1),(0,-1), 1),
        ((0,-1),(0,-1),(0,1), 1),
        ((0,-1),(0,-1),(0,-1), 1),
        ((0,0),(1,0),(1,1), 1),
        ((0,0),(1,0),(-1,-1), 1),
        ((0,0),(-1,0),(1,1), 1),
        ((0,0),(-1,0),(-1,-1), 1),
        ((1,0),(1,1),(0,0), 1),
        ((1,0),(-1,-1),(0,0), 1),
        ((-1,0),(1,1),(0,0), 1),
        ((-1,0),(-1,-1),(0,0), 1),
        ((1,1),(0,0),(1,0), 1),
        ((1,1),(0,0),(-1,0), 1),
        ((-1,-1),(0,0),(1,0), 1),
        ((-1,-1),(0,0),(-1,0), 1)
        },
    1: {
        ((0,1),(0,1),(0,1), -1),
        ((0,1),(0,1),(0,-1), -1),
        ((0,1),(0,-1),(0,1), -1),
        ((0,1),(0,-1),(0,-1), -1),
        ((0,-1),(0,1),(0,1), -1),
        ((0,-1),(0,1),(0,-1), -1),
        ((0,-1),(0,-1),(0,1), -1),
        ((0,-1),(0,-1),(0,-1), -1),
        ((0,0),(1,1),(1,0), -1),
        ((0,0),(1,1),(-1,0), -1),
        ((0,0),(-1,-1),(1,0), -1),
        ((0,0),(-1,-1),(-1,0), -1),
        ((1,1),(1,0),(0,0), -1),
        ((1,1),(-1,0),(0,0), -1),
        ((-1,-1),(1,0),(0,0), -1),
        ((-1,-1),(-1,0),(0,0), -1),
        ((1,0),(0,0),(1,1), -1),
        ((1,0),(0,0),(-1,-1), -1),
        ((-1,0),(0,0),(1,1), -1),
        ((-1,0),(0,0),(-1,-1), -1)
        }
    } #0 is a, 1 is b
'''


###generate box/lattice
def tetrahedron_generate_box(X,Y,Z, folder=''):
    file_name = f'tetrahedron_{X}by{Y}by{Z}.lat'
    #'' in os.path.join is current directory
    file_path = os.path.join(folder, file_name)

    #initialise the box/lattice
    box = {}
    fresh_points = {(0,0,0,0)} #set of tuples
    #grow the lattice
    while True:
        #add freshpoints to box and find futurepoints
        future_points = set()
        for fresh_point in fresh_points:
            neighbours = {
                (fresh_point[0] + perturbation[0], 
                fresh_point[1] + perturbation[1], 
                fresh_point[2] + perturbation[2], 
                fresh_point[3] + perturbation[3]) 
                for perturbation in tetrahedron_lattice_perturbations[fresh_point[3]]
            }
            valid_neighbours = set()
            for neighbour in neighbours:
                if (
                    0<=neighbour[0]<X and
                    0<=neighbour[1]<Y and
                    0<=neighbour[2]<Z
                    ):
                    valid_neighbours.add(neighbour)
            box[fresh_point] = TreeNode(fresh_point,valid_neighbours)
            future_points.update(valid_neighbours)
        #update values for the next iteration of the while loop
        fresh_points = future_points - set(box)
        if not future_points: break #empty sets return false

    #record the lattice, number of nodes (its kind of like the effective volume and used for doing density of neurons calcs)
    with open(os.path.join(folder, 'corresponding_nodes.txt'), 'a') as f:
        f.write(f'{file_name}, {len(box)}\n')

    # Pickle the box
    with open(file_path, 'wb') as f:
        pickle.dump(box, f)


def cube_generate_box(X,Y,Z, folder=''):
    file_name = f'cube_{X}by{Y}by{Z}.lat'
    #'' in os.path.join is current directory
    file_path = os.path.join(folder, file_name)

    #initialise the box/lattice

    box = {}
    fresh_points = {(0,0,0)} #set of tuples
    #grow the lattice
    while True:
        #add freshpoints to box and find futurepoints
        future_points = set()
        for fresh_point in fresh_points:
            neighbours = {
                (fresh_point[0] + perturbation[0], 
                fresh_point[1] + perturbation[1], 
                fresh_point[2] + perturbation[2]) 
                for perturbation in cube_lattice_perturbations
            }
            valid_neighbours = set()
            for neighbour in neighbours:
                if (
                    0<=neighbour[0]<X and
                    0<=neighbour[1]<Y and
                    0<=neighbour[2]<Z
                    ):
                    valid_neighbours.add(neighbour)
            box[fresh_point] = TreeNode(fresh_point,valid_neighbours)
            future_points.update(valid_neighbours)
        #update values for the next iteration of the while loop
        fresh_points = future_points - set(box)
        if not future_points: break #empty sets return false

#    print(f'number of points:',len(set(box)))
    
#    #record the lattice, number of nodes (its kind of like the effective volume and used for doing density of neurons calcs)
#    with open(os.path.join(folder, 'corresponding_nodes.txt'), 'a') as f:
#        f.write(f'{file_name}, {len(box)}\n')

#    # Pickle the box
#    with open(file_path, 'wb') as f:
#        pickle.dump(box, f)


def octahedron_generate_box(X,Y,Z, folder=''):
    file_name = f'octahedron_{X}by{Y}by{Z}.lat'
    #'' in os.path.join is current directory
    file_path = os.path.join(folder, file_name)

    #initialise the box/lattice

    box = {}
    fresh_points = {(0,0,0)} #set of tuples
    #grow the lattice
    while True:
        #add freshpoints to box and find futurepoints
        future_points = set()
        for fresh_point in fresh_points:
            neighbours = {
                (fresh_point[0] + perturbation[0], 
                fresh_point[1] + perturbation[1], 
                fresh_point[2] + perturbation[2]) 
                for perturbation in octahedron_lattice_perturbations
            }
            valid_neighbours = set()
            for neighbour in neighbours:
                if (
                    0<=neighbour[0]<X and
                    0<=neighbour[1]<Y and
                    0<=neighbour[2]<Z
                    ):
                    valid_neighbours.add(neighbour)
            box[fresh_point] = TreeNode(fresh_point,valid_neighbours)
            future_points.update(valid_neighbours)
        #update values for the next iteration of the while loop
        fresh_points = future_points - set(box)
        if not future_points: break #empty sets return false
    
    #record the lattice, number of nodes (its kind of like the effective volume and used for doing density of neurons calcs)
    with open(os.path.join(folder, 'corresponding_nodes.txt'), 'a') as f:
        f.write(f'{file_name}, {len(box)}\n')

    # Pickle the box
    with open(file_path, 'wb') as f:
        pickle.dump(box, f)


'''These dont work
def dodecahedron_generate_box(X,Y,Z, folder=''):
    file_name = f'dodecahedron_{X}by{Y}by{Z}.lat'
    #'' in os.path.join is current directory
    file_path = os.path.join(folder, file_name)

    phi = (np.sqrt(5)+1)/2 #golden ratio, or  1 + 1/goldenratio

    #initialise the box/lattice
    box = {}
    fresh_points = {((0,0),(0,0),(0,0),0)} #set of tuples
    #grow the lattice
    while True:
        #add freshpoints to box and find futurepoints
        future_points = set()
        for fresh_point in fresh_points:
            neighbours = {
                ((fresh_point[0][0] + perturbation[0][0], fresh_point[0][1] + perturbation[0][1]), 
                (fresh_point[1][0] + perturbation[1][0], fresh_point[1][1] + perturbation[1][1]), 
                (fresh_point[2][0] + perturbation[2][0], fresh_point[2][1] + perturbation[2][1]), 
                fresh_point[3] + perturbation[3]) 
                for perturbation in dodecahedron_lattice_perturbations[fresh_point[3]]
            }
            valid_neighbours = set()
            for neighbour in neighbours:
                if (
                    0<=neighbour[0][0] + phi*neighbour[0][1]<X and
                    0<=neighbour[1][0] + phi*neighbour[1][1]<Y and
                    0<=neighbour[2][0] + phi*neighbour[2][1]<Z
                    ):
                    valid_neighbours.add(neighbour)
            box[fresh_point] = TreeNode(fresh_point,valid_neighbours)
            future_points.update(valid_neighbours)
        #update values for the next iteration of the while loop
        fresh_points = future_points - set(box)
        if not future_points: break #empty sets return false
    
    # Pickle the box
    with open(file_path, 'wb') as f:
        pickle.dump(box, f)


def icosahedron_generate_box(X,Y,Z, folder=''):
    file_name = f'icosahedron_{X}by{Y}by{Z}.lat'
    #'' in os.path.join is current directory
    file_path = os.path.join(folder, file_name)

    #phi = (1 + np.sqrt(5))/2 #the golden ratio
    #phi - 1 was closer than 1/phi to the real value so thats what we will use
    #actually, phi - 0.9999999999999999 was closer, but that was only visually
    one_on_phi = (np.sqrt(5)-1)/2 #so turns out the golden ratio is actually so special that 1+ 1/phi = phi, so changing the table

    #initialise the box/lattice
    box = {}
    fresh_points = {((0,0),(0,0),(0,0),0)} #set of tuples
    #grow the lattice
    while True:
        #add freshpoints to box and find futurepoints
        future_points = set()
        for fresh_point in fresh_points:
            neighbours = {
                ((fresh_point[0][0] + perturbation[0][0], fresh_point[0][1] + perturbation[0][1]),
                (fresh_point[1][0] + perturbation[1][0], fresh_point[1][1] + perturbation[1][1]),
                (fresh_point[2][0] + perturbation[2][0], fresh_point[2][1] + perturbation[2][1]),
                fresh_point[3] + perturbation[3]) 
                for perturbation in icosahedron_lattice_perturbations[fresh_point[3]]
            }
            valid_neighbours = set()
            for neighbour in neighbours:
                if (
                    0<=one_on_phi*neighbour[0][0] + neighbour[0][1]<X and
                    0<=one_on_phi*neighbour[1][0] + neighbour[1][1]<Y and
                    0<=one_on_phi*neighbour[2][0] + neighbour[2][1]<Z
                    ):
                    valid_neighbours.add(neighbour)
            box[fresh_point] = TreeNode(fresh_point,valid_neighbours)
            future_points.update(valid_neighbours)
        #update values for the next iteration of the while loop
        fresh_points = future_points - set(box)
        if not future_points: break #empty sets return false
    
    # Pickle the box
    with open(file_path, 'wb') as f:
        pickle.dump(box, f)


#only use the 0th set, dont change chirality, the complements for the directions exist in its own set
def mad_test_icosahedron_generate_box(X,Y,Z, folder=''):
    file_name = f'mad_icosahedron_{X}by{Y}by{Z}.lat'
    #'' in os.path.join is current directory
    file_path = os.path.join(folder, file_name)

    #phi = (1 + np.sqrt(5))/2 #the golden ratio
    #phi - 1 was closer than 1/phi to the real value so thats what we will use
    #actually, phi - 0.9999999999999999 was closer, but that was only visually
    one_on_phi = (np.sqrt(5)-1)/2 #so turns out the golden ratio is actually so special that 1+ 1/phi = phi, so changing the table

    #initialise the box/lattice
    box = {}
    fresh_points = {((0,0),(0,0),(0,0),0)} #set of tuples
    #grow the lattice
    while True:
        #add freshpoints to box and find futurepoints
        future_points = set()
        for fresh_point in fresh_points:
            neighbours = {
                ((fresh_point[0][0] + perturbation[0][0], fresh_point[0][1] + perturbation[0][1]),
                (fresh_point[1][0] + perturbation[1][0], fresh_point[1][1] + perturbation[1][1]),
                (fresh_point[2][0] + perturbation[2][0], fresh_point[2][1] + perturbation[2][1]),
                0) 
                for perturbation in icosahedron_lattice_perturbations[0]
            }
            valid_neighbours = set()
            for neighbour in neighbours:
                if (
                    0<=one_on_phi*neighbour[0][0] + neighbour[0][1]<X and
                    0<=one_on_phi*neighbour[1][0] + neighbour[1][1]<Y and
                    0<=one_on_phi*neighbour[2][0] + neighbour[2][1]<Z
                    ):
                    valid_neighbours.add(neighbour)
            #if fresh_point in box: print(fresh_point)
            print(valid_neighbours)
            box[fresh_point] = TreeNode(fresh_point,valid_neighbours)
            future_points.update(valid_neighbours)
        #update values for the next iteration of the while loop
        fresh_points = future_points - set(box)
        if not future_points: break #empty sets return false
    
    # Pickle the box
    with open(file_path, 'wb') as f:
        pickle.dump(box, f)
'''


def main():

    if len(sys.argv) != 4:
        print(f"Usage: {sys.argv[0]} <X> <Y> <Z>")
        sys.exit(1)

#    X=200
#    Y=200
#    Z=200

    # Access the arguments
    X = int(sys.argv[1])
    Y = int(sys.argv[2])
    Z = int(sys.argv[3])

    folder='lattices' #'' is current directory
    #box is counted computationally, 27 means 27 positions, values 0 to 26, hence 0<=x<X is used
    #158/159 is about 1mil points in the octahedral lattice (2 points per unit cell, each unit cell has a volume of 2^3)
    
    start_time = time.process_time()#time.perf_counter()

    #tetrahedron_generate_box(X,Y,Z,folder=folder)
    cube_generate_box(X,Y,Z,folder=folder)
    #octahedron_generate_box(X,Y,Z, folder=folder)

    end_time = time.process_time() #time.perf_counter()
    elapsed_time = end_time - start_time
    print(X,Y,Z,elapsed_time)
    

if __name__ == '__main__':
    main()

