module adpio
    use adpsolver
    implicit none
    
    integer, parameter :: ioUnit = 10  ! IO unit for files

contains
    !!! Read and parse the input file
    !!! 
    !!! Input:
    !!!     model = adpmodel object
    !!!     filename = Name of input file to read/parse
    !!!
    !!! Return:
    !!!     stat = Error status (0 = success,
    !!!                          1 = error opening input file,
    !!!                          2 = error parsing card(s))
    integer function readInput(model, filename) result(stat)
        class(adpmodel), intent(inout) :: model
        character*80, intent(in) :: filename  ! Name of input file

        character*80 :: card
        character*80 :: id

        integer :: err  ! I/O error flag

        ! Initialize the error flags
        err = 0
        stat = 0

        ! Open the file for reading
        open(unit = ioUnit, file = filename, status = 'old', &
            &  action = 'read', iostat = err)
        if (err .ne. 0) then
            write(*, '(A)') "ERROR: The input file could not be read."
            write(*, '(A, A)') "    Filename = ", trim(filename)
            write(*, '(A, I3)') "    I/O Error = ", err
            close(ioUnit)
            stat = 1
            return
        end if

        ! Begin processing input cards
        read(ioUnit, '(A)', iostat = err) card
        do while (.not. is_iostat_end(err))
            ! Remove all whitespace from card
            card = removeWhitespace(card)
            
            if (card(1:1) .eq. '*') then
                write(*, '(A, 1X, A)') "Found card:", trim(card)
                err = parseCard(model, card)
                if (err .ne. 0) then
                    stat = 2
                end if
            else if (len(trim(card)) .gt. 1) then
                write(*, '(A, 1X, A)') "Found comment:", trim(card)
            end if

            ! Read the next card
            read(ioUnit, '(A)', iostat = err) card
        end do

        if (stat .eq. 2) then
            write(*, *) "Error: Some input cards were not processed."
        end if

        close(unit = ioUnit)

    end function readInput

    
    !!! Parse an input card and extract all parameter data
    !!!
    !!! Input:
    !!!     model = adpmodel object
    !!!     card = Input card to parse and extract data from
    !!!
    !!! Return:
    !!!     stat = Error status flag (0 = success,
    !!!                               1 = I/O error parsing card,
    !!!                               2 = error parsing words on card,
    !!!                               3 = invalid word parameter name,
    !!!                               4 = invalid card ID)
    integer function parseCard(model, card) result(stat)
        class(adpmodel), intent(inout) :: model
        character*80, intent(in) :: card  ! Card string

        character*80 :: id  ! Card ID (first word on card)
        integer :: nWords  ! Number of words on card (excludes ID)
        character*80 :: word  ! List of words on card (excludes ID)

        integer :: i  ! Loop control variable
        integer :: ind  ! End index of last word
        integer :: err  ! Error flag

        character*80 :: paramName  ! Parameter name
        character*80 :: paramValue  ! Parameter value

        ! Initialize the error flags
        err = 0
        stat = 0

        ! Get the card ID (first word on card)
        read(card, *, iostat = err) id
        if (err .ne. 0) then
            write(*, *) "Unknown error parsing card ID:", trim(card)
            stat = 1
            return
        end if

        ! Get the number of remaining words
        nwords = count(transfer(card, 'A', len(trim(card))) == ",")

        ! Get the words
        ind = index(card, trim(id)) + len(trim(id)) + 1  ! Add 1 for comma
        do i = 1, nWords
            ! Read the next word from the card
            read(card(ind:80), *, iostat = err) word
            if (err .ne. 0) then
                write(*, '(A, I2, A, A)') "Unknown error parsing word ", &
                    &   i, ":", trim(card)
                stat = 2
                return
            end if

            ! Get the starting index of the next word (add 1 for comma)
            ind = index(card, trim(word)) + len(trim(word)) + 1

            ! Parse the word to extract the parameter name and value
            err = parseArg(word, paramName, paramValue)
            if (err .ne. 0) then
                write(*, '(A, I2, A, A)') "Invalid string on word ", &
                    &   i, ":", trim(card)
                stat = 2
                cycle
            end if

            ! Extract the data based on card type
            select case(id)
                case ("*job")
                    err = setJobField(model, paramName, paramValue)
                case ("*bcs")
                    err = setBCField(model, paramName, paramValue)
                case ("*props")
                    err = setPropsField(model, paramName, paramValue)
                case ("*grid")
                    err = setGridField(model, paramName, paramValue)
                case ("*solver")
                    err = setSolverField(model, paramName, paramValue)
                case default
                    write(*, '(2X, A, A, A)') "Unrecognized card ID: ", &
                        &   trim(id), ". Card skipped!"
                    stat = 4
                    return
            end select

            ! Check for an error setting fields
            if (err .ne. 0) then
                stat = 3
            end if
        end do

    end function parseCard
    
    
    !!! Set a field from the job card
    !!!
    !!! Input:
    !!!     this = adpmodel object
    !!!     paramName = Name of field to set
    !!!     paramValue = String representation of parameter value
    !!!
    !!! Return:
    !!!     stat = Error status flag (0 = success,
    !!!                               1 = Unrecognized parameter name)
    integer function setJobField(model, paramName, paramValue) result (stat)
        class(adpmodel), intent(inout) :: model
        character*80, intent(in) :: paramName  ! Parameter name
        character*80, intent(in) :: paramValue  ! Parameter value

        ! Initialize the error flag
        stat = 0

        select case(paramName)
            case ("name")
                model%jobName = paramValue
            case default
                write(*, '(6X, A)') &
                    &   "Unrecognized parameter name on job card:", &
                    &    trim(paramName)
                stat = 1
        end select

    end function setJobField

    
    !!! Set a field from the bcs card
    !!!
    !!! Input:
    !!!     model = adpmodel object
    !!!     paramName = Name of field to set
    !!!     paramValue = String representation of parameter value
    !!!
    !!! Return:
    !!!     stat = Error status flag (0 = success,
    !!!                               1 = Unrecognized parameter name)
    integer function setBCField(model, paramName, paramValue) result(stat)
        class(adpmodel), intent(inout) :: model
        character*80, intent(in) :: paramName  ! Parameter name
        character*80, intent(in) :: paramValue  ! Parameter value

        ! Initialize the error flag
        stat = 0

        select case(paramName)
            case ("phi0")
                model%phi0 = stringToReal(paramValue)
            case ("phi1")
                model%phi1 = stringToReal(paramValue)
            case default
                write(*, '(6X, A, 1X, A)') &
                    &   "Unrecognized parameter name on bcs card:", &
                    &    trim(paramName)
                stat = 1
        end select
    end function setBCField

    
    !!! Set a field from the props card
    !!!
    !!! Input:
    !!!     model = adpmodel object
    !!!     paramName = Name of field to set
    !!!     paramValue = String representation of parameter value
    !!!
    !!! Return:
    !!!     stat = Error status flag (0 = success,
    !!!                               1 = Unrecognized parameter name)
    integer function setPropsField(model, paramName, paramValue) result(stat)
        class(adpmodel), intent(inout) :: model
        character*80, intent(in) :: paramName  ! Parameter name
        character*80, intent(in) :: paramValue  ! Parameter value

        ! Initialize the error flag
        stat = 0

        select case(paramName)
            case ("u")
                model%u = stringToReal(paramValue)
            case ("gamma")
                model%gamma = stringToReal(paramValue)
            case ("c")
                model%c = stringToReal(paramValue)
            case default
                write(*, '(6X, A, 1X, A)') &
                    &   "Unrecognized parameter name on props card:", &
                    &    trim(paramName)
                stat = 1
            end select
    end function setPropsField

    
    !!! Set a field from the grid card
    !!!
    !!! Input:
    !!!     model = adpmodel object
    !!!     paramName = Name of field to set
    !!!     paramValue = String representation of parameter value
    !!!
    !!! Return:
    !!!     stat = Error status flag (0 = success,
    !!!                               1 = Unrecognized parameter name)
    integer function setGridField(model, paramName, paramValue) result(stat)
        class(adpmodel), intent(inout) :: model
        character*80, intent(in) :: paramName  ! Parameter name
        character*80, intent(in) :: paramValue  ! Parameter value

        ! Initialize the error flag
        stat = 0

        select case(paramName)
            case ("npts")
                model%npts = stringToInt(paramValue)
            case default
                write(*, '(6X, A, 1X, A)') &
                    &   "Unrecognized parameter name on grid card:", &
                    &    trim(paramName)
                stat = 1
            end select
    end function setGridField

    
    !!! Set a field from the model card
    !!!
    !!! Input:
    !!!     model = adpsolver object
    !!!     paramName = Name of field to set
    !!!     paramValue = String representation of parameter value
    !!!
    !!! Return:
    !!!     stat = Error status flag (0 = success,
    !!!                               1 = Unrecognized parameter name)
    integer function setSolverField(model, paramName, paramValue) result(stat)
        class(adpmodel), intent(inout) :: model
        character*80, intent(in) :: paramName  ! Parameter name
        character*80, intent(in) :: paramValue  ! Parameter value

        ! Initialize the error flag
        stat = 0

        select case(paramName)
            case ("method")
                select case(paramValue)
                    case ("analytical")
                        model%method = analytical
                    case ("implicit")
                        model%method = implicit
                    case default
                        write(*, '(6X, A, 1X, A)') &
                            &   "Unrecognized solution method:", paramValue
                end select
            case ("derivatives")
                select case (paramValue)
                    case ("yes")
                        model%computeDerivatives = .true.
                    case ("no")
                        model%computeDerivatives = .false.
                    case default
                        write(*, '(A)') "Derivatives option must be ", &
                            &   "yes/no. Defaulting to no."
                end select
            case default
                write(*, '(6X, A, 1X, A)') &
                    &   "Unrecognized parameter name on solver card:", &
                    &    trim(paramName)
                stat = 1
            end select
        
    end function setSolverField


    REAL function stringToReal(str) result (val)
        character*80, intent(in) :: str  ! String to convert
        
        integer :: err  ! I/O error status

        read(str, *, iostat = err) val
        if (err .ne. 0) then
            write(*, '(6X, A, A)') "Error converting string to real: ", str
        end if
    end function stringToReal


    integer function stringToInt(str) result (val)
        character*80, intent(in) :: str  ! String to convert

        integer :: err  ! I/O error status

        read(str, *, iostat = err) val
        if (err .ne. 0) then
            write(*, '(6X, A, A)') "Error converting string to int: ", str
        end if
    end function stringToInt

    
    !!! Write the results to an output file
    !!!
    !!! Inputs:
    !!!     model = adpmodel object
    !!!     filename_opt = Optional output filename (defaults to jobName.out)
    !!!
    !!! Return:
    !!!     stat = Error status (0 = success,
    !!!                          1 = Error opening output file)
    integer function writeOutput(model, filename_opt) result(stat)
        class(adpmodel), intent(inout) :: model  ! adpmodel object
        character*80, intent(in), optional :: filename_opt  ! Optional filename

        character*80 :: filename  ! Output file name
        integer :: err  ! Error status flag

        stat = 0

        ! Get the filename to write to
        if (present(filename_opt)) then
            filename = filename_opt
        else
            filename = trim(model%jobname) // ".out"
        end if

        ! Open the output file for writing
        open(unit = ioUnit, file = filename, action = 'write', iostat = err)
        if (err .ne. 0) then
            write(*, *) "Error: Unable to open output file for writing."
            stat = 1
            return
        end if

        ! Write the header and results
        call writeData(model)

        ! Close the output file
        close(unit = ioUnit)

    end function writeOutput
    

    subroutine writeData(model)
        class(adpmodel), intent(inout) :: model  ! adpmodel object
        
        integer :: i, j  ! Loop control variable
        character*80 :: fmtString
        
        if (model%method .eq. analytical .and. model%computeDerivatives) then
            ! Header
            write(ioUnit, '(A)') "x,phi,dPhi/dX,dPhi/dU,dPhi/dGamma,dPhi/dC"

            ! Results
            write(fmtString, *) '(5(ES23.15, ", "), ES23.15)'
            do i=1, model%npts
                write(ioUnit, fmtString) model%x(i), model%phi(i), &
                    &   model%dPhi_dU(i), model%dPhi_dGamma(i), model%dPhi_dC(i)
            end do

        else
            ! Header
            write(ioUnit, '(A)') "x,phi"

            ! Results
            write(fmtString, *) '(ES23.15, ", ", ES23.15)'
            do i=1, model%npts
                write(ioUnit, fmtString) model%x(i), model%phi(i)
            end do
        end if
    end subroutine writeData
    
    
    character*80 function removeWhitespace(str) result(mstr)
        character*80, intent(in) :: str

        integer :: i  ! Loop control variable
        integer :: l  ! Length of modified string

        character, parameter :: tab = achar(9)  ! Tab character

        ! Initialize length of modified string to 0
        l = 0
        mstr = char(0)

        ! Loop over each character in unmodified string
        do i = 1, len(str)
            ! Only consider non-space, non-tab characters
            if (str(i:i) .ne. ' ' .and. str(i:i) .ne. tab) then
                l = l + 1  ! Increment the count
                mstr(l:l) = str(i:i)  ! Add the non-whitespace character
            end if
        end do
    end function removeWhitespace

    
    !!! Parse an argument to determine the name and value
    !!!
    !!! Input:
    !!!     arg = Argument string to parse
    !!!
    !!! Output:
    !!!     argName = name extracted from argument string
    !!!     argValue = value extracted from argument string
    !!!
    !!! Return:
    !!!     stat = Error status (0 = success,
    !!!                          1 = argument value missing,
    !!!                          2 = argument name and value missing)
    integer function parseArg(arg, argName, argValue) result(stat)
        character*80, intent(in) :: arg  ! Argument string to parse
        character*80, intent(out) :: argName  ! Name of argument
        character*80, intent(out) :: argValue  ! Value of argument

        character*80 :: trimmedArg
        integer :: ind  ! Index of equal sign in argument

        argName = ""
        argValue = ""

        trimmedArg = trim(adjustl(arg))
        ind = index(trimmedArg, "=")
        if (len(trimmedArg) .eq. 0) then
            stat = 2
            return
        else if (ind .eq. 0) then
            argName = trimmedArg
            stat = 1
        else
            argName = trimmedArg(1 : ind - 1)
            argValue = trimmedArg(ind + 1 :)
            stat = 0
        end if
    end function parseArg
    
end module adpio