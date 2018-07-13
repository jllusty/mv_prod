program main
    use const_mod
    use mpi
    implicit none

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
    ! size of matrix
    integer :: N, M
    ! master only
    integer, allocatable :: quotient, remainder
    integer, allocatable, dimension(:) :: job_table
    ! all cores
    integer, allocatable, dimension(:) :: rows

    ! -- Initialize MPI --
    call mpi_init(ierror)
    ! Get rank
    call mpi_comm_rank(mpi_comm_world,my_rank,ierror)
    ! Get number of cores
    call mpi_comm_size(mpi_comm_world,num_cores,ierror)
    ! MPI Wall Time Start
    t0_wall = mpi_wtime()

    ! -- debug: mpi rolecall --
    write(*,*) 'I am core', my_rank, 'of ', num_cores

    ! -- get user input --
    call getarg(1, arg)
    read(arg,'(I8)') N
    call getarg(2, arg)
    read(arg,'(I8)') M

    ! -- master: distribute number of columns for each row --
    if (my_rank == master) then
        ! allocate assignment variables
        allocate(quotient,remainder)
        ! allocate assignment table
        allocate(job_table(1:num_cores))

        quotient = M / 4
        remainder = mod(M,4)
        ! fill assignment table with quotient
        job_table = reshape((/quotient/),shape(job_table))
        ! add one to first r elements
        job_table(1:remainder) = job_table(1:remainder) + 1

        write(*,*) 'job_table = ', job_table

        ! deallocate assignment variables
        deallocate(quotient,remainder)
    end if

    ! scatter assignment table

    ! broadcast vector x

    ! -- master: gather resultant vector --
    if (my_rank == master) then
        ! deallocate assignment table
        deallocate(job_table)
    end if
    

    ! -- finalize mpi --
    ! mpi wall time finish
    tf_wall = mpi_wtime()
    ! finalize mpi
    call mpi_finalize(ierror)

end program main
