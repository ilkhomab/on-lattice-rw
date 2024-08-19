module lattice_growth_optimized
    implicit none
    integer :: X, Y
    integer :: max_points
    type :: TreeNode
        integer :: point(2)
        integer :: neighbours(4,2)
        integer :: neighbours_coord(4)
        integer :: num_neighbours
    end type TreeNode
    type(TreeNode), allocatable :: box(:)
    integer :: num_points
    integer :: cube_lattice_perturbations(4, 2)
    integer, allocatable :: point_exists(:,:)

! ANSI colours for the output:
    character(len=*), parameter :: black = char(27) // '[30m'
    character(len=*), parameter :: red = char(27) // '[31m'
    character(len=*), parameter :: green = char(27) // '[32m'
    character(len=*), parameter :: yellow = char(27) // '[33m'
    character(len=*), parameter :: blue = char(27) // '[34m'
    character(len=*), parameter :: magenta = char(27) // '[35m'
    character(len=*), parameter :: cyan = char(27) // '[36m'
    character(len=*), parameter :: white = char(27) // '[37m'
    character(len=*), parameter :: reset = char(27) // '[0m'


    integer :: num_neurons_in_sim, num_neurons
    type :: point2D
      integer :: point(2)
    end type point2D
    type(point2D), allocatable :: starting_points(:)

    type :: Neuron
!      integer :: neuron_id
!      type(point2D) :: root  
      integer :: num_points, num_future_points, num_fresh_points
      integer, allocatable :: points(:)
      type(point2D), allocatable :: fresh_points(:)
      type(point2D), allocatable :: future_points(:)
!      type(point2D), allocatable :: branch_points(:)
!      type(point2D), allocatable :: child_points(:)
!      type(point2D), allocatable :: cables(:)
!      type(point2D), allocatable :: intersection_points(:)
!      
    end type Neuron 

    type(Neuron), allocatable :: neurons(:)

    integer, allocatable :: neuron_positions(:,:,:)



    integer, allocatable :: is_occupied(:,:), indbox(:,:), was_occupied(:)


contains

    subroutine initialize_cube_lattice_perturbations()
        cube_lattice_perturbations = reshape([ &
            1, 0, -1, 0, &
            0, 1, 0, -1], &
            shape(cube_lattice_perturbations))
    end subroutine initialize_cube_lattice_perturbations

    subroutine grow_lattice()
        implicit none
        integer :: i, j
        integer, allocatable :: fresh_points(:,:)
        integer, allocatable :: future_points(:,:)
        integer :: fresh_point(2), neighbour(2)
        integer :: num_fresh_points, num_future_points
        logical :: valid
    
        ! Initialize variables
        num_points = 0
        allocate(fresh_points(max_points, 2))
        fresh_points(1,:) = [0, 0]
        num_fresh_points = 1
    
        allocate(box(max_points))
        allocate(point_exists(0:X-1, 0:Y-1))
        point_exists = 0
        point_exists(0, 0) = 1

        allocate(future_points(max_points, 2))
    
        ! Grow the lattice
        do while (num_fresh_points > 0)
            num_future_points = 0
            do i = 1, num_fresh_points
                fresh_point = fresh_points(i,:)
                ! Find neighbours
                do j = 1, 4 
                    neighbour = fresh_point + cube_lattice_perturbations(j,:)
                    ! Check if the neighbour is valid
                    valid = .true.
                    if (any(neighbour < 0) .or. &
                        neighbour(1) >= X .or. &
                        neighbour(2) >= Y) then
                        valid = .false.
                    end if
                    ! If valid, add to future points
                    if (valid) then
                        if ( point_exists(neighbour(1), neighbour(2)) == 0) then
                          point_exists(neighbour(1), neighbour(2)) = 1
                          num_future_points = num_future_points + 1
                          future_points(num_future_points,:) = neighbour
                        endif
                    end if
                end do
                call add_point_to_box(fresh_point, cube_lattice_perturbations)
            end do
            num_fresh_points = num_future_points
            fresh_points(1:num_future_points, :) = future_points(1:num_future_points, :)
        end do
        deallocate(future_points)
        deallocate(fresh_points)
!        deallocate(point_exists)

    end subroutine grow_lattice

    subroutine add_point_to_box(point, perturbations)
        integer, intent(in) :: point(2)
        integer, intent(in) :: perturbations(4,2)
        integer :: i, j, neighbour(2)
        logical :: valid

        num_points = num_points + 1
        box(num_points)%point = point
        indbox(box(num_points)%point(1),box(num_points)%point(2)) = num_points


        j = 0
        do i = 1, 4 
            neighbour = point + perturbations(i,:)
            valid = .true.
            if (any(neighbour < 0) .or. &
                neighbour(1) >= X .or. &
                neighbour(2) >= Y) then
                valid = .false.
            end if
            if (valid) then
                j = j + 1
                box(num_points)%neighbours(j,:) = neighbour
            end if
        end do
        ! number of valid neighbours of a point
        box(num_points)%num_neighbours = j 
    end subroutine add_point_to_box

    character(len=10) function int2char(i)
        integer, intent(in) :: i

        write(int2char,'(I10)') i
    end function int2char

    subroutine print_rectangle()
        integer :: i, j

        write(*,"(A,A,A)") cyan, "Rectangle contents:", reset
        do i = 1, num_points
            ! Print point coordinates in red
            write(*, "(A, ' (', A, ', ', A, ') ', A, ' {')", advance='no') &
                red // "point:" // reset, green // trim(adjustl(int2char(box(i)%point(1)))), &
                trim(adjustl(int2char(box(i)%point(2)))), blue // "neighbours:" // reset

            ! Print neighbours in green
            do j = 1, box(i)%num_neighbours
                if (j > 1) write(*, "(A)", advance='no') ", "
                write(*, "( '(', A, ', ', A, ')')", advance='no') &
                    green // trim(adjustl(int2char(box(i)%neighbours(j, 1)))), &
                    trim(adjustl(int2char(box(i)%neighbours(j, 2)))) // reset
            end do

            write(*,"('}')")
        end do
    end subroutine print_rectangle

    subroutine get_neighbour_coords()
        integer :: i, j

        do i = 1, num_points
           do j = 1, box(i)%num_neighbours
              box(i)%neighbours_coord(j) = indbox(box(i)%neighbours(j, 1),box(i)%neighbours(j, 2))
           end do
        end do

    end subroutine get_neighbour_coords 
   

    subroutine print_starting_points()
        integer :: i,itmp,j

        write(*,"(A,A,A,A)") cyan, "Starting points for ",trim(adjustl(int2char(num_neurons_in_sim))), " neurons:"// reset
        do i = 1, num_neurons_in_sim 
            ! Print point coordinates in red
            write(*, "(A, ' (', A, ', ', A, ')')", advance='no') &
                red // "starting point:" // reset, green // trim(adjustl(int2char(starting_points(i)%point(1)))), &
                trim(adjustl(int2char(starting_points(i)%point(2)))) // reset
            itmp=indbox(starting_points(i)%point(1),starting_points(i)%point(2))
            write(*,"(A,A,A,A,A,A,A,A,A,A,A,A)") blue//'. In 2D domain it has a unique number ' //reset,&
            green// trim(adjustl(int2char(itmp))),&
            '.'//reset // blue // ' Its neighbours unique numbers: '//reset,&
            green,(trim(adjustl(int2char(box(itmp)%neighbours_coord(j)))),' ',j=1,box(itmp)%num_neighbours)
            write(*,"(A)") reset
            write(*,*)
        end do
    end subroutine print_starting_points

    subroutine generate_starting_positions()
        integer :: i, j, temp
        integer :: array(1:num_points)  ! auxiliary array 
        real :: random_value

        ! Check if num_neurons_in_sim is less than or equal to num_points
        if (num_neurons_in_sim > num_points) then
            print *, "Error: num_neurons_in_sim should be less than or equal to num_points"
            stop
        endif

        allocate(starting_points(1:num_neurons_in_sim))

        ! Initialize the array with values from 1 to auxiliary array 
        do i = 1, num_points 
            array(i) = i
        end do

        ! Seed the random number generator
        call random_seed()

        ! Fisher-Yates shuffle algorithm (shuffling only the first N elements)
        do i = 1, num_neurons_in_sim
            call random_number(random_value)
            j = int(random_value * (num_points-i+1)) + i
            ! Swap elements
            temp = array(i)
            array(i) = array(j)
            array(j) = temp
            ! Store the selected unique number
            starting_points(i)%point(1:2) = box(array(i))%point(1:2)
        end do

    end subroutine generate_starting_positions

    subroutine random_choice(n,array,choice)
        integer, intent(inout) :: n
        integer, intent(inout), dimension(1:n) :: array
        integer, intent(out) :: choice
        integer :: itmp,i,i0
        real :: random_value
        ! Seed the random number generator
        call random_seed()
        ! Generate a random number between 0 and 1
        call random_number(random_value)
        ! Scale it to the range [1, n]
        itmp = int(random_value * n) + 1
        choice = array(itmp)
 ! removing the direction since it is now a future point
        n=n-1
        i0=0
        do i=1,n
          if(i/=itmp) then
            i0=i0+1
            array(i0)=array(i)
          endif
        enddo
    end subroutine random_choice

    subroutine remove_element_in_place(array, index_to_remove, size)
        implicit none
        ! Input/output parameters
        integer, intent(inout) :: array(1:size)         ! Array (input/output)
        integer, intent(in) :: index_to_remove     ! Index of the element to remove
        integer, intent(inout) :: size             ! Size of the array (input/output)

        ! Local variables
        integer :: i                                ! Loop index

        ! Decrease the size of the array by one
        size = size - 1
        ! Shift elements to remove the specified element
        do i = index_to_remove, size
            array(i) = array(i+1)
        end do

    end subroutine remove_element_in_place

    subroutine array_difference(A,size_A,B,size_B,C,size_C)
      implicit none
      integer, intent(in) :: size_A
      integer, intent(in) :: A(1:size_A)
      integer, intent(in) :: size_B
      integer, intent(in) :: B(1:size_B)
      integer, intent(inout) :: size_C
      integer, intent(out) :: C(1:size_C)
      
      integer :: i

      size_C=0
      do i = 1, size_A
        if (any (A(i) == B)) then
          cycle
        endif
        size_C = size_C + 1
        C(size_C) = A(i)
      enddo
      
    end subroutine array_difference

    subroutine branching_check(point,A,size_A,hit)
      implicit none
      integer, intent(in) :: point
      integer, intent(in) :: size_A
      integer, intent(in) :: A(1:size_A)
      logical, intent(out) :: hit
      
      integer :: i

     
      if (any (point == A)) then
        hit = .true.
      else
        hit = .false.
      endif
      
    end subroutine branching_check

end module lattice_growth_optimized

program main
    use lattice_growth_optimized
    implicit none
    integer :: iargc, n
    integer, allocatable :: cmd_args(:)
    character(len=8) :: cmd_arg
    real :: start_time, end_time, elapsed_time

    integer :: code_time, i, j, time_steps, idirection
    integer :: direction(1:4)
    integer :: choice, ind, k, l 
    integer :: nrn_pos
    integer :: direction_to_move
    real :: rand
    integer, allocatable, dimension(:) :: neuron_future_points
    integer, allocatable, dimension(:,:) :: head_positions 
    integer, allocatable, dimension(:,:) :: start_point, stop_point 
    logical, allocatable, dimension(:,:) :: neuron_status
    integer :: num_future_points, num_future_points_tmp, indtmp, num_neurons_tmp
    integer :: max_branches
    logical :: hit
    integer, allocatable, dimension(:) :: number_of_branches
    character(len=22) :: filename

    ! Get the number of command-line arguments
    iargc = command_argument_count()

    ! Check if the correct number of arguments are provided
    if (iargc /= 4) then
        write(*,"(A,A,A,A)")magenta,  'Usage: ./exe X Y num_neurons_in_sim, time_steps', reset
        stop
    end if

    ! Allocate array to hold command-line arguments
    allocate(cmd_args(iargc))

    ! Read command-line arguments
    do n = 1, iargc
        call get_command_argument(n, cmd_arg)
        read(cmd_arg, *) cmd_args(n)
    end do

    ! Assign X, Y from command-line arguments
    X = cmd_args(1)
    Y = cmd_args(2)
    num_neurons_in_sim = cmd_args(3)
    time_steps = cmd_args(4)

    max_points = X * Y !2147483647 !X * Y * Z

    if(max_points>2147483647) then
      write(*,"(A,A,A)")magenta, 'max_points > 2147483647 which is beyond int4 range. Please enter smaller values of X, Y, Z', reset
      stop
    endif

    allocate(indbox(0:X-1,0:Y-1))

    ! Get the starting time
    call cpu_time(start_time)

    call initialize_cube_lattice_perturbations()
    call grow_lattice()

    ! Get the ending time
    call cpu_time(end_time)

    ! Calculate elapsed time
    elapsed_time = end_time - start_time

    ! Find out the coordinates of the neighbours of the points
    call get_neighbour_coords()

    ! Output results (for demonstration purposes)
    !write(*,"(A,A,I10,A,A)")magenta, 'Grew lattice with ', num_points, ' points.', reset
    write(*,"(2I8,I10,F14.8,A)")X, Y, num_points, elapsed_time

    call print_rectangle()
    ! Generate starting positions
    call generate_starting_positions()

    ! Print starting points of neurons
    call print_starting_points()

    max_branches=10000!2147483647
    num_neurons=num_neurons_in_sim
    
    !allocate(neurons(1:max_neurons))

    allocate(neuron_positions(1:time_steps,1:num_neurons,1:max_branches))
    allocate(neuron_future_points(1:4))
    allocate(head_positions(1:num_neurons,1:max_branches))
    allocate(neuron_status(1:num_neurons,1:max_branches))
    allocate(start_point(1:num_neurons,1:max_branches))
    allocate(stop_point(1:num_neurons,1:max_branches))
    allocate(number_of_branches(1:num_neurons))


    call random_seed()

    code_time = 1
    ! Initialise neuron positions
    do i = 1, num_neurons
      indtmp = indbox(starting_points(i)%point(1),starting_points(i)%point(2))
      neuron_positions(1,i,1) = indtmp
      head_positions(i,1) = indtmp
      neuron_status(i,1)=.true.
      start_point(i,1) = 1
      stop_point(i,1) = time_steps
      number_of_branches(i) = 1
    enddo

    

    ! Grow the neurons in the lattice
    do code_time=2, time_steps 

      num_neurons_tmp=num_neurons
    ! Moving each neuron
      do i=1,num_neurons

        do k=1,number_of_branches(i)
          if(.not.neuron_status(i,k)) cycle

    ! Current position of the neuron
          nrn_pos = neuron_positions(code_time-1,i,k)
    ! Initially num_future_points is set to be the maximum number of possible neighbours
          num_future_points = box(nrn_pos)%num_neighbours
    ! Making sure that the neuron is not moving to its own body which includes all of its branches as well
          neuron_future_points(1:num_future_points)=box(nrn_pos)%neighbours_coord(1:num_future_points)
          do l=1,number_of_branches(i)
            num_future_points_tmp = num_future_points 
            call array_difference(neuron_future_points(1:num_future_points),num_future_points,&
                              neuron_positions(start_point(i,l):code_time-1,i,l),code_time-start_point(i,l),&
                              neuron_future_points(1:num_future_points),num_future_points_tmp)
            num_future_points = num_future_points_tmp
          enddo

    ! Making sure that the neuron is not moving to the position occupied by other neurons
          do j=1,num_neurons
            call array_difference(neuron_future_points(1:num_future_points),num_future_points,&
                              head_positions(j,1:number_of_branches(j)),number_of_branches(j),&
                              neuron_future_points(1:num_future_points),num_future_points_tmp)
            num_future_points = num_future_points_tmp
          enddo

          select case (num_future_points)
            case(0)
    ! Terminate the neuron journey as it has nowhere to go
            neuron_status(i,k) = .false.
            stop_point(i,k) = code_time - 1
            cycle

            case(1)
    ! Make the neuron to move to that only available point
            direction_to_move=neuron_future_points(1)        

            case default
    ! Choose the random position out of the available num_future_points positions

            call random_number(rand)
            direction_to_move = neuron_future_points(int(rand * num_future_points) + 1)

          end select

          neuron_positions(code_time, i, k) = direction_to_move

          head_positions(i,k) = direction_to_move

    ! Now we need to introduce the branching condition. Currently we choose the following branching 
    ! condition:
    ! Whenever the neuron head moves into the path (body) of any of the other neurons it branches. 
    ! That point is a branching point where a new neuron branch starts its random walk.
    ! To impose this branching condition we are introducing the j loop which runs through all other neurons.

          do j=1, num_neurons
            if(j==i) cycle  ! Branching does not occur if a neuron moves into its own path
          
            do l=1,number_of_branches(j)
    ! branching_check subroutine checks if point 'direction_to_move' hits the path (body) of neuron j
              call branching_check(direction_to_move,neuron_positions(start_point(j,l):code_time-1,j,l),&
                               code_time-start_point(j,l),hit)
              if(hit)then
print*,'branching'
                number_of_branches(i) = number_of_branches(i) + 1
                neuron_positions(code_time,i,number_of_branches(i)) = direction_to_move
                start_point(i,number_of_branches(i)) = code_time 
                stop_point(i,number_of_branches(i)) = time_steps
                neuron_status(i,number_of_branches(i))=.true.
                head_positions(i,number_of_branches(i)) = direction_to_move
                exit
              endif
            enddo

          enddo


        enddo  


      enddo

    enddo

! Printing results:

    open(unit=30, file='branches.txt', status='replace')
    do i=1,num_neurons
      write(filename, '("neuron_path_", A)') trim(adjustl(int2char(i)))
      open(unit=i + 100, file=filename, status='replace')
      do code_time=1,time_steps
        write(i + 100,"(1A10)",advance='no')trim(adjustl(int2char(code_time)))
        do k=1,number_of_branches(i)
          if(code_time >= start_point(i,k).and.code_time <= stop_point(i,k)) then
            write(i+100,"(2A5)",advance='no')trim(adjustl(int2char(box(neuron_positions(code_time, i, k))%point(1)))),&
                                        trim(adjustl(int2char(box(neuron_positions(code_time, i, k))%point(2))))
          else
            write(i+100,"(2A5)",advance='no')'NaN','NaN'
          endif
        enddo
        write(i+100,*)
      enddo
      write(30,"(3A)")trim(adjustl(int2char(i))),'   ',trim(adjustl(int2char(number_of_branches(i))))
    enddo
    

    do i=1,num_neurons
      do k=1,number_of_branches(i)
        write(filename, '("start_stop_", A,"_",A)') trim(adjustl(int2char(i))),trim(adjustl(int2char(k))) 
        open(unit=88, file=filename, status='replace')
        write(88, '(A,"   ",A)') trim(adjustl(int2char(start_point(i,k)))),&
        trim(adjustl(int2char(stop_point(i,k))))
        close(88)
        write(filename, '("neuron_branch_", A,"_",A)') trim(adjustl(int2char(i))),trim(adjustl(int2char(k))) 
        open(unit=88, file=filename, status='replace')
        do code_time=start_point(i,k),stop_point(i,k)
          write(88,'(A,"   ",A)')trim(adjustl(int2char(box(neuron_positions(code_time, i, k))%point(1)))),&
                                        trim(adjustl(int2char(box(neuron_positions(code_time, i, k))%point(2))))
        enddo
        close(88)
      enddo
    enddo

    time_steps=maxval(stop_point) 

    print*,'time_steps=',time_steps
    open(unit=88, file='time_steps',status='replace')
    write(88,*)time_steps
    close(88)

!$omp parallel do default(none) &
!$omp private(filename, code_time, i, k) &
!$omp shared(time_steps, X, Y, num_neurons, number_of_branches, start_point, stop_point)
    do code_time=1,time_steps
      write(filename, '("plot_frame_", A)') trim(adjustl(int2char(code_time)))
      open(unit=code_time+100, file=filename, status='replace')
      write(code_time+100,'(A)') 'set terminal pngcairo size 800,800'
      write(code_time+100,'(A)') 'set size square'
      write(code_time+100,'(A)') 'set xrange [0:'//trim(adjustl(int2char(X)))//']'
      write(code_time+100,'(A)') 'set yrange [0:'//trim(adjustl(int2char(Y)))//']'
      write(code_time+100,'(A)') 'set grid'
      write(code_time+100,'(A)') 'set title "Neuron random walk simulation"'
      write(code_time+100,'(A)') "set output 'frame_"//trim(adjustl(int2char(code_time)))//".png'"
      write(code_time+100,'(A)',advance='no') "plot "
      do i=1,num_neurons
        do k=1,number_of_branches(i)
          if(code_time < start_point(i,k)) then
            cycle
          else
            if(code_time > stop_point(i,k)) then
              write(code_time+100,'(A)',advance='no') "'< cat neuron_branch_"&
              //trim(adjustl(int2char(i)))//"_"//trim(adjustl(int2char(k)))&
              //"' using 1:2 with lines lc "//trim(adjustl(int2char(i)))//" notitle,"
              write(code_time+100,'(A)',advance='no') "'< tail -n 1 neuron_branch_"&
              //trim(adjustl(int2char(i)))//"_"//trim(adjustl(int2char(k)))&
              //"' using 1:2 with points lc "&
              //trim(adjustl(int2char(i)))//" pointtype 7 notitle,"
            else
              write(code_time+100,'(A)',advance='no') "'< head -n "&
              //trim(adjustl(int2char(code_time-start_point(i,k)+1)))&
              //" neuron_branch_"//trim(adjustl(int2char(i)))//"_"&
              //trim(adjustl(int2char(k)))&
              //"' using 1:2 with lines lc "//trim(adjustl(int2char(i)))//" notitle,"
              write(code_time+100,'(A)',advance='no') "'< head -n "&
              //trim(adjustl(int2char(code_time-start_point(i,k)+1)))&
              //" neuron_branch_"//trim(adjustl(int2char(i)))//"_"//trim(adjustl(int2char(k)))//&
              " | tail -n 1 '"&
              //" using 1:2 with points lc "//trim(adjustl(int2char(i)))//" pointtype 7 notitle,"
            endif
          endif
        enddo
      enddo
      close(code_time+100)
    enddo
!$omp end parallel do

    stop

!    allocate(neurons(1:num_neurons_in_sim))
!    do i = 1, num_neurons_in_sim 
!      !allocate(neurons(i)%points(1:min(2**(time_steps+1)-1,max_points)))
!      allocate(neurons(i)%points(1:max_points))
!      allocate(neurons(i)%fresh_points(1:max_points))
!      allocate(neurons(i)%future_points(1:max_points))
!      neurons(i)%num_fresh_points=1
!      neurons(i)%fresh_points(neurons(i)%num_fresh_points)=starting_points(i)
!    enddo
!
!    allocate(is_occupied(0:X-1,0:Y-1))
!    allocate(was_occupied(1:max_points))
!    is_occupied(0:X-1,0:Y-1)=0
!    was_occupied(1:max_points)=0
!
!    code_time=0
!
!
!    do i = 1, num_neurons_in_sim
!      neurons(i)%num_points=0
!    enddo
!
!    ! Grow the neurons in the lattice
!    do code_time=1, time_steps 
!        do i = 1, num_neurons_in_sim 
!            neurons(i)%num_future_points=0
!            do j = 1, neurons(i)%num_fresh_points 
!! Get the index of fresh_points in the box array
!                ind=indbox(neurons(i)%fresh_points(j)%point(1),neurons(i)%fresh_points(j)%point(2))
!                idirection=0
!                do k=1,box(ind)%num_neighbours
!! Check if the neighbour point is occupied by another neuron
!                    if(is_occupied(box(ind)%neighbours(k,1),box(ind)%neighbours(k,2))==0) then
!                        idirection=idirection+1
!                        direction(idirection)=indbox(box(ind)%neighbours(k,1),box(ind)%neighbours(k,2))
!!                        is_occupied(box(ind)%neighbours(k,1),box(ind)%neighbours(k,2))=1
!                    endif
!                enddo
!                if(idirection/=0) then
!                  call random_choice(idirection,direction(1:idirection),choice)
!if(neurons(i)%num_future_points >= max_points) exit
!                  neurons(i)%num_future_points=neurons(i)%num_future_points+1
!                  neurons(i)%future_points(neurons(i)%num_future_points)%point(1)=box(choice)%point(1)
!                  neurons(i)%future_points(neurons(i)%num_future_points)%point(2)=box(choice)%point(2)
!                endif
!! Branching condition
!print*, 'before branching',idirection,was_occupied(ind)
!                if(idirection/=0 .and. was_occupied(ind)==1) then
!                  call random_choice(idirection,direction(1:idirection),choice)
!if(neurons(i)%num_future_points >= max_points) exit
!                  neurons(i)%num_future_points=neurons(i)%num_future_points+1
!                  !neurons(i)%points(neurons(i)%num_points)=choice
!print*, 'branching'
!                  neurons(i)%future_points(neurons(i)%num_future_points)%point(1)=box(choice)%point(1)
!                  neurons(i)%future_points(neurons(i)%num_future_points)%point(2)=box(choice)%point(2)
!                endif
!
!            end do
!! migrate_fresh_points
!            do j = 1, neurons(i)%num_fresh_points 
!if(neurons(i)%num_points >= max_points) exit
!              neurons(i)%num_points=neurons(i)%num_points+1
!              neurons(i)%points(neurons(i)%num_points)=indbox(neurons(i)%fresh_points(j)%point(1), &
!                                                              neurons(i)%fresh_points(j)%point(2))
!              is_occupied(neurons(i)%fresh_points(j)%point(1),neurons(i)%fresh_points(j)%point(2))=1
!              was_occupied(indbox(neurons(i)%fresh_points(j)%point(1),neurons(i)%fresh_points(j)%point(2)))=1
!            enddo
!! migrate_future_points
!            do j=1, neurons(i)%num_future_points
!              neurons(i)%fresh_points(j)=neurons(i)%future_points(j)
!            enddo
!            neurons(i)%num_fresh_points=neurons(i)%num_future_points
!            print*,'neurons(i)%num_future_points',neurons(i)%num_future_points
!        end do
!        is_occupied(:,:)=0
!        if (mod(code_time, 10) == 0) print *, 'time:', code_time
!    end do
!
!    do i=1, neurons(1)%num_points
!      print*,neurons(1)%points(i)
!    enddo
!
!    print*,''
!    print*,''
!    
!!    do i=1, neurons(2)%num_points
!!      print*,neurons(2)%points(i)
!!    enddo
!
!    do i=1, neurons(1)%num_points
!      write(33,*)box(neurons(1)%points(i))%point(1),box(neurons(1)%points(i))%point(2)
!    enddo

!    write(33,*)
!    write(33,*)
!    
!    do i=1, neurons(2)%num_points
!      write(33,*)box(neurons(2)%points(i))%point(1),box(neurons(2)%points(i))%point(2)
!    enddo
!
!    write(33,*)
!    write(33,*)
!    
!    do i=1, neurons(3)%num_points
!      write(33,*)box(neurons(3)%points(i))%point(1),box(neurons(3)%points(i))%point(2)
!    enddo
!
!    write(33,*)
!    write(33,*)
!    
!    do i=1, neurons(4)%num_points
!      write(33,*)box(neurons(4)%points(i))%point(1),box(neurons(4)%points(i))%point(2)
!    enddo


end program main

