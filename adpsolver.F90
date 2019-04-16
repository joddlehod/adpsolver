module adpsolver
    implicit none
    
    !!! Solution Methods enum
    enum, bind(C)
        enumerator :: implicit = 1, analytical = 2
    end enum

    type :: adpmodel
        character*80 :: jobname  ! Job name
        
        real :: phi0  ! Scalar value at x = 0
        real :: phi1  ! Scalar value at x = 1
        
        real :: u  ! Velocity
        real :: gamma  ! Diffusivity
        real :: c  ! Proportionality constant for scalar production
        
        integer :: npts  ! Number of grid points on discretized domain
                         ! (spaced uniformly from x = 0 to x = 1)
        real :: dx  ! Distance between points in x array
        
        integer :: method  ! Solution Method to use
        logical :: computeDerivatives  ! Calculate analytical derivatives?
                                       ! Only used when method = Analytical

        real, allocatable, dimension(:) :: x  ! array of grid coordinates
        real, allocatable, dimension(:) :: phi  ! scalar field function
        real, allocatable, dimension(:) :: dPhi_dX
        real, allocatable, dimension(:) :: dPhi_dU
        real, allocatable, dimension(:) :: dPhi_dGamma
        real, allocatable, dimension(:) :: dPhi_dC

    contains
        procedure :: init => adpmodel_init
        procedure :: alloc => adpmodel_alloc
        procedure :: dealloc => adpmodel_dealloc
        procedure :: solve => adpmodel_solve
		procedure :: solveAnalytical => adpmodel_solveAnalytical
        procedure :: solveImplicit => adpmodel_solveImplicit
        
    end type adpmodel
    
contains
    !!! Initialize an adpmodel object
    !!!
    !!! Input:
    !!!     this = adpmodel object to initialize
    subroutine adpmodel_init(this)
        class(adpmodel), intent(inout) :: this
        
        this%jobname = "ADPSolution1"
        
        this%phi0 = 1.0
        this%phi1 = 0.0
        
        this%u = 1.0
        this%gamma = 0.1
        this%c = -1.0
        
        this%npts = 11
		
		this%method = implicit
		this%computeDerivatives = .false.
    end subroutine adpmodel_init
    
    
    !!! Allocate dynamic memory
    !!!
    !!! Input:
    !!!     this = adpmodel object
    !!!
    !!! Return:
    !!!     stat = Error status flag (0 = success,
    !!!                               1 = Allocation error)
    integer function adpmodel_alloc(this) result(stat)
        class(adpmodel), intent(inout) :: this

        integer :: n
        integer :: err

        ! Initialize error flags
        err = 0
        stat = 0

        call this%dealloc()

        n = this%npts
        allocate(this%x(n), this%phi(n), this%dPhi_dX(n), this%dPhi_dU(n), &
			&	this%dPhi_dGamma(n), this%dPhi_dC(n), stat = err)
        if (err .ne. 0) then
            write(*, *) "Error: Could not allocate arrays!"
            stat = 1
        end if
    end function adpmodel_alloc


    !!! Deallocate dynamic memory
    !!!
    !!! Input:
    !!!     this = adpmodel object
    subroutine adpmodel_dealloc(this)
        class(adpmodel), intent(inout) :: this

        if (allocated(this%phi)) deallocate(this%phi)
        if (allocated(this%x)) deallocate(this%x)
        if (allocated(this%dPhi_dX)) deallocate(this%dPhi_dX)
        if (allocated(this%dPhi_dU)) deallocate(this%dPhi_dU)
        if (allocated(this%dPhi_dGamma)) deallocate(this%dPhi_dGamma)
        if (allocated(this%dPhi_dC)) deallocate(this%dPhi_dC)
    end subroutine adpmodel_dealloc

    
    !!! Run the solution
    !!! 
    !!! Input:
    !!!     this = adpmodel object
    !!!
    !!! Return:
    !!!     stat = Error status (0 = success,
    !!!                          1 = problem not overdamped,
    !!!                          2 = error allocating arrays,
    !!!                          3 = invalid solution method,
    !!!                          4 = error executing solution method)
    integer function adpmodel_solve(this) result(stat)
        class(adpmodel), intent(inout) :: this

        integer :: err  ! Temporary error flag
        integer :: i  ! Loop control variable

        ! Initialize the error flags
        err = 0
        stat = 0

        ! Make sure we are in the overdamped regime
        if (this%u**2 .le. 4.0 * this%c * this%gamma) then
            write(*, *) "Error: The specified properties do not meet ", &
                "the overdamping requirement for this analysis."
            stat = 1
            return
        end if

        ! Allocate solution arrays
        err = this%alloc()
        if (err .ne. 0) then
            stat = 2
            return
        end if

        ! Populate x array for npts
        this%dx = 1.0 / (this%nPts - 1)
        do i = 1, this%npts
            this%x(i) = (i - 1) * this%dx
        end do

        ! Solve using the selected solution method
        select case(this%method)
            case (implicit)
                err = this%solveImplicit()
            case (analytical)
                err = this%solveAnalytical()
            case default
                write(*, *) "Invalid solution method specified: ", &
                    &   this%method
                stat = 3
                return
        end select

        ! Check solution status
        if (err .ne. 0) then
            stat = 4
            return
        end if

    end function adpmodel_solve

    
    !!! Solve the problem implicitly
    !!!
    !!! Inputs:
    !!!     this = AdvectionSolver object
    !!!
    !!! Return:
    !!!     stat = Error status (0 = success)
    integer function adpmodel_solveImplicit(this) result(stat)
        class(adpmodel), intent(inout) :: this

        integer :: i, j  ! Loop control variables
        integer :: n  ! Shortcut to npts
        real, dimension(:), allocatable :: ld  ! Lower diagonal vector
        real, dimension(:), allocatable :: md  ! Middle diagonal vector
        real, dimension(:), allocatable :: ud  ! Uppder diagonal vector
        real, dimension(:), allocatable :: bc  ! BC vector

        real :: aE, aW, aP  ! Matrix coefficients
        real :: w  ! temporary variable

        ! Initialize error flag
        stat = 0

        ! Allocate the solution matrix and BC vector
        n = this%npts
        allocate(bc(n), ld(n), md(n), ud(n))
        bc = 0.0
        ld = 0.0
        md = 0.0
        ud = 0.0

        ! Calculate the matrix coefficients (1st-order upwinding)
		aW = -this%gamma / this%dx**2; aE = aW
		if (this%u > 0.0) then
			aW = aW - this%u / this%dx
		else
			aE = aE + this%u / this%dx
		end if

        aP = -aE - aW - this%c

        ! Populate the diagonal and BC vectors
        bc(1) = this%phi0
        md(1) = 1.0
        do i = 2, n - 1
            bc(i) = 0.0
            ld(i) = aW
            md(i) = aP
            ud(i) = aE
        end do
        bc(n) = this%phi1
        md(n) = 1.0

        do i = 2, n
            w = ld(i) / md(i - 1)
            md(i) = md(i) - w * ud(i - 1)
            bc(i) = bc(i) - w * bc(i - 1)
        end do
        
        this%phi(n) = bc(n) / md(n)
        do i = n - 1, 1, -1
            this%phi(i) = (bc(i) - ud(i) * this%phi(i + 1)) / md(i)
        end do

        deallocate(bc, ld, md, ud)

    end function adpmodel_solveImplicit


    !!! Solve the problem analytically
    !!!
    !!! Inputs:
    !!!     this = adpmodel object
    !!!
    !!! Return:
    !!!     stat = Error status (0 = success,
    !!!                          1 = Invalid BCs for analytical solution)
    integer function adpmodel_solveAnalytical(this) result(stat)
        class(adpmodel), intent(inout) :: this  ! adpmodel object

        integer :: i  ! Loop control variable
        real :: x, u, g, c
		real :: det, lp, lm, elp, elm, elpx, elmx
		real :: dPhi_dlp, dPhi_dlm
		real :: dlp_dU, dlm_dU
		real :: dlp_dG, dlm_dG
		real :: dlp_dC, dlm_dC

        stat = 0

        u = this%u
        g = this%gamma
        c = this%c
		det = sqrt(u**2 - 4.0 * g * c)

        ! Solution 1: phi(x=0)=1, phi(x=1)=0
        if (this%phi0 .eq. 1.0 .and. this%phi1 .eq. 0.0) then
			lp = (u + det) / (2.0 * g)
			lm = (u - det) / (2.0 * g)
			elp = exp(lp)
			elm = exp(lm)

            do i = 1, this%nPts
                x = this%x(i)
				
				elpx = exp(lp * x)
				elmx = exp(lm * x)
				this%phi(i) = (elm * elpx - elp * elmx) / (elm - elp)
				
                if (this%computeDerivatives) then
                    ! Calculate dPhi/dx
					this%dPhi_dX(i) = (lp * elm * elpx - lm * elp * elmx) / (elm - elp)
					
					! Calculate dPhi/dlambda
					dPhi_dlp = ( elp * elm * (elpx - elmx) - x * elm * elpx * (elp - elm)) &
						&	/ (elm - elp)**2
					dPhi_dlm = (-elp * elm * (elpx - elmx) + x * elp * elmx * (elp - elm)) &
						&	/ (elm - elp)**2

                    ! Calculate dPhi/du
					dlp_dU =  lp / det
					dlm_dU = -lm / det
					this%dPhi_dU(i) = dPhi_dlp * dlp_dU + dPhi_dlm * dlm_dU
					
                    ! Calculate dPhi/dGamma
					dlp_dG =  (c - u * lp) / (g * det)
					dlm_dG = -(c - u * lm) / (g * det)
					this%dPhi_dGamma(i) = dPhi_dlp * dlp_dG + dPhi_dlm * dlm_dG
					
                    ! Calculate dPhi/dC
					dlp_dC = -1.0 / det
					dlm_dC =  1.0 / det
					this%dPhi_dC(i) = dPhi_dlp * dlp_dC + dPhi_dlm * dlm_dC
                end if
            end do
        else
            write(*, *) "Invalid BCs for Analytical solution option."
            stat = 1
            return
        end if

    end function adpmodel_solveAnalytical

end module adpsolver