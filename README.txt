Advection Solver: A numerical solver for the one-dimensional steady-state
advection/diffusion equation:

    u * Phi_x = Gamma * Phi_xx + C * Phi
    ---------   --------------   -------
    Advection   Diffusion term   Source
    term                         term

    where:

        Phi = The scalar quantity to be advected/diffused
        x = The independent parameter (e.g. spatial coordinate)
        Phi_x = The first derivative of Phi with respect to x
        Phi_xx = The second derivative of Phi with respect to x
        u = The advection parameter (e.g. velocity)
        Gamma = The diffusivity of the medium
        C = A proportionality constant for the scalar source term


Compiling with GFortran (v. 6.3.0):
    $ gfortran -fdefault-real-8 [-Ddnad -Dndv=<#> dnad/dnad.F90] adpsolver.F90 adpio.F90 adpinterface.f90 -o adpsolver.exe


Execution command:
    $ ./adpsolver.exe inputfile [outputfile]

        inputfile = Name of file containing input cards
        outputfile = Name of file to write solution to. If omitted,
            output will be written to [jobname].out, where [jobname]
            is the value of the name parameter on the *job card.


Input file format:
    Input files use cards to specify parameters for each execution. Cards are
    composed of a leading asterisk (*) followed directly by a card ID and one
    or more parameters. The card is comma-delimited. All cards are optional and
    order-independent.

    Card parameters specify a parameter name and value, separated by an equal
    sign (=). All parameters are optional and order-independent.

    Note that all cards and associated parameters must be lower-case. Cards
    and parameters containing upper-case letters will be ignored.


Card and Parameter Options:

    This section lists all available cards and their associated options. The
    default values that will be assumed if options are omitted from an input
    deck are shown in square brackets in the examples.

    Job card:

        Example:        *job, name=[AdvectionSolution1]

        Description:    Card for specifying the job name of a simulation.

        Parameters:     1) name = The simulation job name. This value is used
                            to generate an output file name if one is not
                            specified from the command line.

    Boundary Condition 1 card:

        Example:        *bc1, x=[0.0], phi=[1.0]

        Description:    Card for specifying the first (left-hand) boundary
                        condition.

        Parameters:     1) x = The x-coordinate for the first (left-hand)
                            boundary condition. The value specified here must be
                            less than the value specified for x on the *bc2 card.
                        2) phi = The value of the scalar quantity at the
                            x-coordinate specified in (1)

    Boundary Condition 2 card:

        Example:        *bc2, x=[1.0], phi=[0.0]

        Description:    Card for specifying the second (right-hand) boundary
                        condition.

        Parameters:     1) x = The x-coordinate for the second (right-hand)
                            boundary condition. The value specified here must be
                            greater than the value specified for x on the *bc1
                            card.
                        2) phi = The value of the scalar quantity at the
                            x-coordinate specified in (1)

    Properties card:

        Example:        *props, u=[1.0], gamma=[0.1], c=[-1.0]

        Description:    Card for specifying material properties.

        Parameters:     1) u = The advection parameter (e.g. velocity in a
                            fluid system)
                        2) gamma = The diffusivity of the medium
                        3) c = Source term proportionality constant

    Grid card:

        Example:        *grid, npts=[11]

        Description:    Card for specifying the grid density.

        Parameters:     1) npts = The number of grid points at which to
                            calculate a solution. The grid points are
                            distributed evenly between the boundaries with one
                            grid point on each boundary. The solution file will
                            contain a result at each grid point.

    Solver card:

        Example:        *solver, method=[analytical]

        Description:    Card for specifying the solution method.

        Parameters:     1) method = The solution method to use for this
                            analysis. Available options are "analytical" and
                            "implicit". If "analytical" is specified, the
                            boundary conditions specified on the *bc1 and
                            *bc2 cards must match one of the options below:

                            a) *bc1, x=0.0, phi=1.0
                               *bc2, x=[any], phi=0.0
                


