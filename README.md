# on-lattice-rw

11/06/2024 Max Galettis
# recommended reading order
1. intersect_branch_to_optimise_wk8.py refering to dependancies as necessary
2. everything else


## dependancies
### create_node_dict.py 
generates a lattice using predefined edge vectors that characterise the lattice shape. The lattice is generated over a prescribed finite portion of the positive R3 quadrant by specifying the maximum ordinate values in each axis of R3. This results in a .lat file named after the shape of the lattice and the maximum ordinate values of R3. The lattice is built of TreeNode objects.

Please for your sake, skip the parts that I have marked wont work

### Neuron.py 
class used for information about a path of points on a lattice(of TreeNode type)
### Tree_Node.py 
class used for information about points on a lattice

## the simulation scripts:
each time step, grow each neuron(typically randomly initialised on the lattice) along each leading head. (with restriciton of moving at one lattice ppint per timestep, moving only to lattice points yet to reach their occupancy limit and not occupied by the neuron that is in the process of growing)

### intersect_branch_to_optimise_wk8.py 
whenever there is a (detectable) intersection(by the construction of the lattice is only ever found at node points, never on the edge of a lattice) the neuron will try to branch(create a second leading head from the same position), max occupancy for neurons on the lattice is 2

This code was cleaned for the purpose of sharing, most outdated and not to standard parts were cut on the 11/06/2024. These not to standard parts were however left in the non_intersect_alternative_growth_rule.py sim. please if understanding the code, read this one first, then move to alternative grwoth rule to see how things change.

### non_intersect_branch_alternative_growth_rule.py 
changes the branching condition of the above code and restricts max occupancy for each node on the lattice from neurons to 1

results are generated as desired, could be a csv or a list of cable lengths, or something else that we are yet to want.



### run_multiple_times(_alternative).py 
runs the respective simulation script more than once with incremental run names each time.