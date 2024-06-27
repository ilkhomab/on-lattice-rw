module lattice_growth
    implicit none
    integer, parameter :: X = 30, Y = 30, Z = 30 ! Example dimensions
    integer, parameter :: max_points = X * Y * Z !2147483647 !X * Y * Z
    type :: TreeNode
        integer :: point(3)
        integer, allocatable :: neighbours(:,:)
    end type TreeNode
    type(TreeNode), allocatable :: box(:)
    integer :: num_points
    integer :: cube_lattice_perturbations(6, 3)

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
        allocate(fresh_points(1, 3))
        fresh_points(1,:) = [0, 0, 0]
        num_fresh_points = 1

        allocate(box(max_points))

        ! Grow the lattice
        do while (num_fresh_points > 0)
            num_future_points = 0
            if (allocated(future_points)) deallocate(future_points)
            allocate(future_points(0, 3))
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
                    if (valid .and. .not. is_point_in_box(neighbour)) then
                        call add_point(future_points, num_future_points, neighbour)
                    end if
                end do
                ! Add fresh point to the box with its valid neighbours
                call add_point_to_box(fresh_point, cube_lattice_perturbations)
            end do
            ! Update fresh points
            deallocate(fresh_points)
            if (num_future_points > 0) then
                allocate(fresh_points(num_future_points, 3))
                num_fresh_points = 0
                do i = 1, num_future_points
                    if (.not. is_point_in_box(future_points(i,:))) then
                        num_fresh_points = num_fresh_points + 1
                        fresh_points(num_fresh_points,:) = future_points(i,:)
                    end if
                end do
            else
                num_fresh_points = 0
            end if
        end do
        if (allocated(fresh_points)) deallocate(fresh_points)
        if (allocated(future_points)) deallocate(future_points)
    end subroutine grow_lattice

    logical function is_point_in_box(point)
        integer, intent(in) :: point(3)
        integer :: i

        is_point_in_box = .false.
        do i = 1, num_points
            if (all(box(i)%point == point)) then
                is_point_in_box = .true.
                return
            end if
        end do
    end function is_point_in_box

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

    subroutine add_point(array, size, point)
        integer, allocatable, intent(inout) :: array(:,:)
        integer, intent(inout) :: size
        integer, intent(in) :: point(3)
        integer, allocatable :: temp(:,:)
        integer :: i
        logical :: exists

        ! Check if the point already exists in the array
        exists = .false.
        do i = 1, size
            if (all(array(i,:) == point)) then
                exists = .true.
                exit
            end if
        end do

        ! Add the point if it doesn't exist
        if (.not. exists) then
            size = size + 1
            allocate(temp(size, 3))
            if (size > 1) then
                temp(1:size-1,:) = array
                deallocate(array)
            end if
            temp(size,:) = point
            array = temp
        end if
    end subroutine add_point

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

end module lattice_growth

program main
    use lattice_growth
    implicit none

    call initialize_cube_lattice_perturbations()
    call grow_lattice()

    ! Output results (for demonstration purposes)
    write(*,"(A,A,I10,A,A)")magenta, 'Grew lattice with ', num_points, ' points.', reset

    !call print_box()

end program main

