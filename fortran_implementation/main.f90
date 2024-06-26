module precision_type
    integer, parameter :: real_type = selected_real_kind(8)
    integer, parameter :: int_type = selected_int_kind(8)
end module precision_type

module box_parameters 
    use precision_type
    implicit none

! This type is used to declare a point in 3D space with x, y and z coordinates
    type :: point3D_type
        integer :: x, y, z  
    end type point3D_type

! This type is used to declare a point in 3D space with x, y and z coordinates 
! and 3 neighbours which in turn have x, y and z coordinates
    type :: point_type
        integer :: x, y, z
        type(point3D_type), dimension(6) :: neighbour
    end type point_type

! hash array contains unique hashes representing points
    integer, allocatable, dimension(:) :: hash

! 6 perturbation points 
  type(point3D_type), dimension(6), parameter :: cube_lattice_perturbations = [ &
       point3D_type( 1,  0,  0), &
       point3D_type(-1,  0,  0), &
       point3D_type( 0,  1,  0), &
       point3D_type( 0, -1,  0), &
       point3D_type( 0,  0,  1), &
       point3D_type( 0,  0, -1)  ]

! The box size:
    integer, parameter :: xmax = 2, ymax = 2, zmax = 2 

    contains

! This function calculates a hash of a point. Large prime numbers are choosen to avoid hash collisions
    function get_hash(ix,iy,iz)
        integer, intent(in) :: ix, iy, iz
        integer, parameter :: prime1 = 73856093, prime2 = 19349663, prime3 = 83492791    
        integer :: get_hash
    
        get_hash = IEOR(IEOR(ix * prime1, iy * prime2), iz * prime3)
    
    end function get_hash

end module box_parameters

program main
!    use precision_type
    use box_parameters
    implicit none
    integer(kind=int_type) :: max_points
    type(point_type), allocatable, dimension(:) :: point
    type(point_type), allocatable, dimension(:) :: fresh_point
    type(point_type), allocatable, dimension(:) :: box 
    type(point3D_type), allocatable, dimension(:) :: future_points
    integer, allocatable, dimension(:) :: future_points_hashes
    type(point3D_type), allocatable, dimension(:) :: new_point
    type(point3D_type), dimension(1:6) :: neighbours
    integer :: number_of_fresh_points, number_of_valid_neighbours
    integer :: tmp_hash 
    integer :: index
    integer :: i0, i, j, k, h, h0
!
    ! ANSI escape code for red color
    character(len=*), parameter :: red = char(27) // '[31m'
    character(len=*), parameter :: green = char(27) // '[32m'
    character(len=*), parameter :: blue = char(27) // '[34m'
    ! ANSI escape code to reset color
    character(len=*), parameter :: reset = char(27) // '[0m'

    max_points=2147483647  ! 2^31 - 1, the maximum int4 range

    allocate(point(1:max_points))
    allocate(box(1:max_points))
    allocate(fresh_point(1:max_points))
    allocate(hash(1:max_points))

    allocate(future_points(1:max_points))
    allocate(future_points_hashes(1:max_points))
    

    fresh_point(1)%x=0
    fresh_point(1)%y=0
    fresh_point(1)%z=0

    number_of_fresh_points=1
    !add the fresh point to the box
    !box(number_of_fresh_points)=fresh_point(1)
    !add a hash of the fresh point to the array of hashes
    hash(number_of_fresh_points)=0
    i0=0
    do while(i0 <= max_points .and. number_of_fresh_points /= 0)
      number_of_valid_neighbours=0
      do j = 1, number_of_fresh_points
        write(*, '(A, A, 3I2, A)') red, ' FRESH POINT=', fresh_point(j)%x, fresh_point(j)%y, fresh_point(j)%z, reset
        do k = 1, 6  ! run through the 6 neigbours and check if they are valid neighbours
          neighbours(k)%x = fresh_point(j)%x+cube_lattice_perturbations(k)%x 
          neighbours(k)%y = fresh_point(j)%y+cube_lattice_perturbations(k)%y 
          neighbours(k)%z = fresh_point(j)%z+cube_lattice_perturbations(k)%z 
          ! check if the neighbour points are in the domain
          if ((neighbours(k)%x >= 0 .and. neighbours(k)%x < xmax) .and. &
             &(neighbours(k)%y >= 0 .and. neighbours(k)%y < ymax) .and. &
             &(neighbours(k)%z >= 0 .and. neighbours(k)%z < zmax)) then
            number_of_valid_neighbours=number_of_valid_neighbours+1
            future_points(number_of_valid_neighbours)=neighbours(k)
            future_points_hashes(number_of_valid_neighbours)=get_hash(future_points(number_of_valid_neighbours)%x,&
                                                                     &future_points(number_of_valid_neighbours)%y,&
                                                                     &future_points(number_of_valid_neighbours)%z)
          endif
        enddo ! do k = 1, 6
      enddo ! do j = 1, number_of_fresh_points
      number_of_fresh_points=0
      do h=1,number_of_valid_neighbours
! the following if condition checks if future_points_hashes(h) is not in the hash array
        if (any(future_points_hashes(h) == hash(1:i0))) then
! this is not a fresh point
          cycle
        else
          number_of_fresh_points=number_of_fresh_points+1
          i0=i0+number_of_fresh_points
! this is a fresh point
          fresh_point(number_of_fresh_points)%x=future_points(h)%x  
          fresh_point(number_of_fresh_points)%y=future_points(h)%y  
          fresh_point(number_of_fresh_points)%z=future_points(h)%z  
          hash(i0) = future_points_hashes(h)
          write(*,'(A, A, I10, A, 3I2, A)')green, 'HASH:', future_points_hashes(h), '  fresh point', & 
                                       fresh_point(number_of_fresh_points)%x, &
                                       fresh_point(number_of_fresh_points)%y, &
                                       fresh_point(number_of_fresh_points)%z, reset
        endif
      enddo
    enddo ! while(i0 <= max_points)

!            box(i0+j)%neighbour(number_of_valid_neighbours)%x=neighbours(k)%x
!            box(i0+j)%neighbour(number_of_valid_neighbours)%y=neighbours(k)%y
!            box(i0+j)%neighbour(number_of_valid_neighbours)%z=neighbours(k)%z
!          box(i0)%x = fresh_point(j)%x 
!          box(i0)%y = fresh_point(j)%y 
!          box(i0)%z = fresh_point(j)%z 

      ! Pause execution for 5 seconds
      !CALL SYSTEM("sleep 5")
do i=1,24
write(*, '(A, I10,A, A, I1, A, I1, A, I1, A, 6(A, I1, A, I1, A, I1), A)') &
    'hash:', hash(i), ', ', &
    'point: (', box(i)%x, ',', box(i)%y, ',', box(i)%z, '), neighbours:', '(', &
    box(i)%neighbour(1)%x, ',', box(i)%neighbour(1)%y, ',', box(i)%neighbour(1)%z, '), (', &
    box(i)%neighbour(2)%x, ',', box(i)%neighbour(2)%y, ',', box(i)%neighbour(2)%z, '), (', &
    box(i)%neighbour(3)%x, ',', box(i)%neighbour(3)%y, ',', box(i)%neighbour(3)%z, '), (', &
    box(i)%neighbour(4)%x, ',', box(i)%neighbour(4)%y, ',', box(i)%neighbour(4)%z, '), (', &
    box(i)%neighbour(5)%x, ',', box(i)%neighbour(5)%y, ',', box(i)%neighbour(5)%z, '), (', &
    box(i)%neighbour(6)%x, ',', box(i)%neighbour(6)%y, ',', box(i)%neighbour(6)%z, ')'
enddo


!    point(1)%neighbour(1)%x = 2 
!    point(1)%neighbour(1)%y = 0 
!    point(1)%neighbour(1)%z = 1 
!    
!    point(1)%x=1
!    point(1)%y=0
!    point(1)%z=1
!
!    hash(1)=get_hash(point(1)%x,point(1)%y,point(1)%z)
!
!    point(2)%x=0
!    point(2)%y=0
!    point(2)%z=0
!
!    hash(2)=get_hash(point(2)%x,point(2)%y,point(2)%z)
!
!    point(3)%x=0
!    point(3)%y=1
!    point(3)%z=0
!        
!    hash(3)=get_hash(point(3)%x,point(3)%y,point(3)%z)
!
!    allocate(new_point(1:1))
!
!    new_point(1)%x=0
!    new_point(1)%y=1
!    new_point(1)%z=0
!
!    tmp_hash=get_hash(new_point(1)%x,new_point(1)%y,new_point(1)%z)
!
!! the following if condition checks if tmp_hash is already in the hash array
!    if (any(tmp_hash == hash)) then
!      print*,'hash exists'
!      !! check if that hash represents an existing point or a new point
!      !! Find the index of the element
!      !index = minval(minloc(hashes, mask = hashes == tmp_hash))    
!      !print*,'and its index is', index
!      !print*,'now it is necessary to check if that hash represents an existing point or a new point'
!      !if(new_points(1)%coordinates(1)==points(index)%coordinates(1).and.&
!      !&  new_points(1)%coordinates(2)==points(index)%coordinates(2).and.&
!      !&  new_points(1)%coordinates(3)==points(index)%coordinates(3)) then
!      !  print*,'it is an existing point'
!      !else
!      !  print*,'it is a new point'
!      !endif
!    else
!      print*,'hash does not exist'
!    endif

end program main
