module lattice_growth_optimized
    implicit none
    integer :: X, Y, Z
    integer :: max_points
    type :: TreeNode
        integer :: point(3)
        integer, allocatable :: neighbours(:,:)
    end type TreeNode
    type(TreeNode), allocatable :: box(:)
    integer :: num_points
    integer :: cube_lattice_perturbations(6, 3)
    integer, allocatable :: point_exists(:,:,:)

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

contains

    subroutine initialize_cube_lattice_perturbations()
        cube_lattice_perturbations = reshape([ &
            1, 0, 0, -1, 0, 0, &
            0, 1, 0,  0, -1, 0, &
            0, 0, 1,  0,  0, -1], &
            shape(cube_lattice_perturbations))
    end subroutine initialize_cube_lattice_perturbations

    subroutine grow_lattice()
        implicit none
        integer :: i, j
        integer, allocatable :: fresh_points(:,:)
        integer, allocatable :: future_points(:,:)
        integer :: fresh_point(3), neighbour(3)
        integer :: num_fresh_points, num_future_points
        logical :: valid
    
        ! Initialize variables
        num_points = 0
        allocate(fresh_points(max_points, 3))
        fresh_points(1,:) = [0, 0, 0]
        num_fresh_points = 1
    
        allocate(box(max_points))
        allocate(point_exists(0:X-1, 0:Y-1, 0:Z-1))
        point_exists = 0
        point_exists(0, 0, 0) = 1
    
        ! Grow the lattice
        do while (num_fresh_points > 0 .and. num_points < max_points)
            num_future_points = 0
            allocate(future_points(max_points, 3))
            do i = 1, num_fresh_points
                fresh_point = fresh_points(i,:)
                ! Find neighbours
                do j = 1, size(cube_lattice_perturbations, 1)
                    neighbour = fresh_point + cube_lattice_perturbations(j,:)
                    ! Check if the neighbour is valid
                    valid = .true.
                    if (any(neighbour < 0) .or. &
                        neighbour(1) >= X .or. &
                        neighbour(2) >= Y .or. &
                        neighbour(3) >= Z) then
                        valid = .false.
                    end if
                    ! If valid, add to future points
                    if (valid .and. point_exists(neighbour(1), neighbour(2), neighbour(3)) == 0) then
                        point_exists(neighbour(1), neighbour(2), neighbour(3)) = 1
                        num_future_points = num_future_points + 1
                        future_points(num_future_points,:) = neighbour
                    end if
                end do
                call add_point_to_box(fresh_point, cube_lattice_perturbations)
            end do
            num_fresh_points = num_future_points
            fresh_points(1:num_future_points, :) = future_points(1:num_future_points, :)
            deallocate(future_points)
        end do
        deallocate(fresh_points)
    end subroutine grow_lattice

    subroutine add_point_to_box(point, perturbations)
        integer, intent(in) :: point(3)
        integer, intent(in) :: perturbations(6,3)
        integer :: num_neighbours, i, j, neighbour(3)
        logical :: valid

        num_points = num_points + 1
        box(num_points)%point = point

        ! Count valid neighbours
        num_neighbours = 0
        do i = 1, size(perturbations, 1)
            neighbour = point + perturbations(i,:)
            valid = .true.
            if (any(neighbour < 0) .or. &
                neighbour(1) >= X .or. &
                neighbour(2) >= Y .or. &
                neighbour(3) >= Z) then
                valid = .false.
            end if
            if (valid) then
                num_neighbours = num_neighbours + 1
            end if
        end do

        ! Allocate and assign valid neighbours
        allocate(box(num_points)%neighbours(num_neighbours, 3))
        j = 0
        do i = 1, size(perturbations, 1)
            neighbour = point + perturbations(i,:)
            valid = .true.
            if (any(neighbour < 0) .or. &
                neighbour(1) >= X .or. &
                neighbour(2) >= Y .or. &
                neighbour(3) >= Z) then
                valid = .false.
            end if
            if (valid) then
                j = j + 1
                box(num_points)%neighbours(j,:) = neighbour
            end if
        end do
    end subroutine add_point_to_box

    character(len=3) function int2char(i)
        integer, intent(in) :: i

        write(int2char,'(I3)') i
    end function int2char

    subroutine print_box()
        integer :: i, j

        write(*,"(A,A,A)") cyan, "Box contents:", reset
        do i = 1, num_points
            ! Print point coordinates in red
            write(*, "(A, ' (', A, ', ', A, ', ', A, ') ', A, ' {')", advance='no') &
                red // "point:" // reset, green // trim(adjustl(int2char(box(i)%point(1)))), &
                trim(adjustl(int2char(box(i)%point(2)))), &
                trim(adjustl(int2char(box(i)%point(3)))) // reset, blue // "neighbours:" // reset

            ! Print neighbours in green
            do j = 1, size(box(i)%neighbours, 1)
                if (j > 1) write(*, "(A)", advance='no') ", "
                write(*, "( '(', A, ', ', A, ', ', A, ')')", advance='no') &
                    green // trim(adjustl(int2char(box(i)%neighbours(j, 1)))), &
                    trim(adjustl(int2char(box(i)%neighbours(j, 2)))), &
                    trim(adjustl(int2char(box(i)%neighbours(j, 3)))) // reset
            end do

            write(*,"('}')")
        end do
    end subroutine print_box

end module lattice_growth_optimized

program main
    use lattice_growth_optimized
    implicit none
    integer :: iargc, n
    integer, allocatable :: cmd_args(:)
    character(len=8) :: cmd_arg

    ! Get the number of command-line arguments
    iargc = command_argument_count()

    ! Check if the correct number of arguments are provided
    if (iargc /= 3) then
        write(*,"(A,A,A)")magenta,  'Usage: ./exe X Y Z', reset
        stop
    end if

    ! Allocate array to hold command-line arguments
    allocate(cmd_args(iargc))

    ! Read command-line arguments
    do n = 1, iargc
        call get_command_argument(n, cmd_arg)
        read(cmd_arg, *) cmd_args(n)
    end do

    ! Assign X, Y, Z from command-line arguments
    X = cmd_args(1)
    Y = cmd_args(2)
    Z = cmd_args(3)

    max_points = X * Y * Z !2147483647 !X * Y * Z

    if(max_points>2147483647) then
      write(*,"(A,A,A)")magenta, 'max_point > 2147483647 which is beyond int4 range. Please enter smaller values of X, Y, Z', reset
      stop
    endif

    call initialize_cube_lattice_perturbations()
    call grow_lattice()

    ! Output results (for demonstration purposes)
    write(*,"(A,A,I10,A,A)")magenta, 'Grew lattice with ', num_points, ' points.', reset

    !call print_box()

end program main

