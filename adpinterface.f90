program main
    use adpsolver
    use adpio
    implicit none
    
    type(adpmodel) :: solver  ! adpmodel object

    integer :: nargs  ! Number of command line arguments
    character*80 :: arg  ! Command line argument
    character*80 :: opt  ! Option from command line argument
    character*80 :: val  ! Value from command line argument

    character*80 :: infile  ! Input file name
    character*80 :: outfile  ! Output file name

    integer :: err  ! Error flag

    ! Get command line arguments
    nargs = command_argument_count()
    if (nargs .gt. 0) then
        call get_command_argument(1, infile)
    else
        write(*, *) "An input file must be specified."
        call exit(1)
    end if

    ! Initialize solver
    write(*, *) "Initializing solver..."
    call solver%init()

    ! Read the input file
    write(*, *) "Reading input file '", trim(infile), "'..."
    err = readInput(solver, infile)
    if (err .ne. 0) then
        write(*, *) "Error reading input file, aborting!"
        call exit(2)
    end if

    ! Calculate a solution
    write(*, *) "Solving..."
    err = solver%solve()
    if (err .ne. 0) then
        write(*, *) "Solution error, aborting!"
        call exit(3)
    end if

    ! Determine the output file name
    if (nargs .gt. 1) then
        call get_command_argument(2, outfile)
    else
        outfile = trim(solver%jobname) // ".out"
    end if

    ! Write the output
    write(*, *) "Writing output to file '", trim(outfile), "'..."
    err = writeOutput(solver, outfile)
    if (err .ne. 0) then
        write(*, *) "Error generating output file!"
        call exit(4)
    end if
    
    call solver%dealloc()

end program main
