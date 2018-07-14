program main
    use const_mod
    use mpi
    implicit none

    include "mkl.fi"

    ! -- MPI Variables --
    ! - MPI - 
    ! Error
    integer :: ierror
    ! Abort Error
    integer :: abort_ierror
    ! Core Rank
    integer :: my_rank
    ! Number of Cores
    integer :: num_cores
    ! Status
    integer :: my_status(mpi_status_size)
    ! Master Rank
    integer, parameter :: master = 0
    ! Wall Time
    real(kind=dp) :: t0_wall
    real(kind=dp) :: tf_wall

    ! -- User Variables --
    ! - User Input -
    character(len=8) :: arg

    ! -- Program Variables --
    ! counters
    integer :: i,j
    ! size of matrix
    integer :: N, M
    ! divisor
    integer :: D
    ! for job assignment table
    integer :: quotient, remainder
    integer, allocatable, dimension(:) :: job_table
    ! rows of full matrix
    integer, dimension(1:2) :: rows
    ! vector x (LHS)
    real(kind=dp), allocatable, dimension(:) :: x
    ! vector y (RHS)
    real(kind=dp), allocatable, dimension(:) :: y_local
    real(kind=dp), allocatable, dimension(:) :: y_final
    ! matrix columns
    real(kind=dp), allocatable, dimension(:,:) :: A
    ! displacement for variable gather
    integer, allocatable, dimension(:) :: displs

    ! -- BLAS Argument Placeholders --
    character :: trans
    real(kind=dp) :: alpha, beta

    ! --------------------------------------------------
    ! Statement Block
    ! --------------------------------------------------

    ! -- Initialize MPI --
    call mpi_init(ierror)
    ! Get rank
    call mpi_comm_rank(mpi_comm_world,my_rank,ierror)
    ! Get number of cores
    call mpi_comm_size(mpi_comm_world,num_cores,ierror)
    ! MPI Wall Time Start
    t0_wall = mpi_wtime()

    ! -- debug: mpi rolecall --
    !write(*,*) 'I am core', my_rank, 'of ', num_cores

    ! -- get user input --
    ! number of rows
    call getarg(1, arg)
    read(arg,'(I8)') N
    ! number of cols
    call getarg(2, arg)
    read(arg,'(I8)') M
    ! divisor of cols to cores
    call getarg(3, arg)
    read(arg,'(I8)') D

    ! all: allocate assignment table by rank
    allocate(job_table(0:num_cores-1))
    
    ! -- all: create assignment table --
    quotient = M / D
    remainder = mod(M,D)
    ! fill assignment table with quotient
    job_table(:) = quotient
    ! add one to first r elements
    job_table(0:remainder-1) = job_table(0:remainder-1) + 1
    ! get rows
    rows(2) = sum(job_table(0:my_rank))
    rows(1) = rows(2) - job_table(my_rank) + 1

    ! debug out
    !write(*,*) 'Core ', my_rank, 'is doing rows ', rows(1), ' to ', rows(2)

    ! all: allocate LHS vector
    allocate(x(1:N))

    ! -- master: read vector x in, broadcast --
    if(my_rank == master) then
        ! read in actual vector later, for now, generate it
        do i = 1,N
            x(i) = real(i,dp)
        end do
        ! debug out
        !write(*,*) 'master sending out x = ', x
    end if
    call mpi_barrier(mpi_comm_world,ierror)
    call mpi_bcast(x, N, mpi_double_precision, master, mpi_comm_world,ierror)

    ! debug out
    !write(*,*) 'Core ', my_rank, 'received x = ', x

    ! all: allocate matrix rows
    allocate(A(rows(1):rows(2),M))

    ! -- all: generate matrix columns --
    do j = 1,M
        do i = rows(1),rows(2)
            A(i,j) = i + j
        end do
    end do

    ! debug out
    !write(*,*) 'Core ', my_rank, 'A = ', A

    ! all: allocate local y
    allocate(y_local(rows(1):rows(2)))

    ! -- all: reduced matrix-vector product --
    ! use BLAS here!
    ! y_local(:) = my_rank
    trans = 'n'
    alpha = real(1.0,dp)
    beta = real(0.0,dp)
    call dgemv (trans, job_table(my_rank), M, alpha, A, job_table(my_rank), x, 1, beta, y_local, 1)

    ! debug out
    !write(*,*) 'Core ', my_rank, 'y_local = ', y_local

    ! master: allocate final y and build displs
    if (my_rank == master) then
        allocate(y_final(1:N))
        allocate(displs(0:num_cores-1))
        displs(0) = 0
        do i = 1,num_cores-1
            displs(i) = sum(job_table(0:i-1))
        end do

        ! debug out
        !write(*,*) 'displs = ', displs
        !write(*,*) 'job_table = ', job_table
    end if

    ! all: gather into final y
    call mpi_barrier(mpi_comm_world,ierror)
    call mpi_gatherv(y_local, job_table(my_rank), mpi_double_precision, &
                y_final, job_table, displs, & 
                mpi_double_precision, master, mpi_comm_world, ierror)

    ! mpi wall time finish
    tf_wall = mpi_wtime()
        
    ! all: deallocate runtime arrays
    deallocate(job_table)
    deallocate(x)
    deallocate(A)
    deallocate(y_local)

    ! -- master: print resultant vector and time elapsed --
    if (my_rank == master) then
        !write(*,*) 'y_final = ', y_final

        write(*,*) 'walltime = ', tf_wall-t0_wall

        ! deallocate final y and displs
        deallocate(y_final)
        deallocate(displs)
    end if

    ! -- finalize mpi --
    ! finalize mpi
    call mpi_finalize(ierror)

end program main
