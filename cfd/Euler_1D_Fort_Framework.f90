!!$/* This program is designed to simulate the Euler equations
!!$   in 1 dimension, using the FORCE solver.
!!$
!!$   It provides output in ASCII format, suitable for plotting in gnuplot.  
!!$ */

program euler1D

  implicit none

  integer :: n, screenFreq, iError

  integer :: i, j, k

  integer, parameter :: DEN = 1, VEL = 2, PRE = 3
  integer, parameter :: RHO = 1, MOM = 2, ENE = 3

  real(kind = 8) :: time, dx, dt

  real(kind = 8), allocatable, dimension (:, :) :: cons, flux

  real(kind = 8), dimension(3) :: prim

  real(kind = 8) :: x
  
!!$  User defined parameters
  integer, parameter :: numCells = 100
  real(kind = 8), parameter :: xMin = 0.0 xMax = 1.0
  real(kind = 8), parameter :: tFinal = 0.25
  real(kind = 8), parameter :: cfl = 0.9
  real(kind = 8), parameter :: gamma = 1.4
  
!!$  Set initial array sizes
  allocate(cons(3, 0:numCells+1), &
           flux(3, 0:numCells+1), &
           STAT = iError)

  if(iError /= 0)then
     write(*,*) "Failed to allocate arrays in euler1D"
  end if

!!$  Set user-dependent constants
  dx = (xMax - xMin) / dble(numCells)
  
!!$  Set initial conditions:
  do i = 1, numCells

     x = (dble(i) - 0.5d0) * dx

!!$     TODO
!!$     Determine test and initialise cons appropriately
     
     
  end do
  

!!$  Do simulation
  time = 0d0

  do while(time < tFinal)

!!$     TODO
!!$     Implement boundary conditions

     dt = cfl * timestep(cons, dx)
     dt = min(dt, tFinal - time)

     call computeFluxes(cons, flux, dx, dt)
     call addFluxes(cons, flux, dx, dt)
     
     
     time = time + dt

!!$     TODO
!!$     Output some useful data to screen here so you know the code is running as expected
     
  end do

!!$  Output final state
  open(unit = 10, file = 'Euler1DOutput.out', status = 'UNKNOWN')
  do i = 1, numCells

!!$     TODO
!!$     Compute the value of x associated with cell i
     
     call primitive(cons(:, i), prim)
     write(10, '(15(ES14.7,2X))') x, prim, cons(:, i)
     
  end do
  
  
contains

  subroutine primitive(cons, prim)

    implicit none

    real(kind = 8), dimension(3), intent(in) :: cons
    real(kind = 8), dimension(3), intent(out) :: prim

!!$    TODO
!!$    Convert the conserved state variables q_i to the primitive variables w_i
    

  end subroutine primitive

  subroutine conservative(prim, cons)

    implicit none

    real(kind = 8), dimension(3), intent(in) :: prim
    real(kind = 8), dimension(3), intent(out) :: cons

!!$    TODO
!!$    Convert the primitive variables w_i to the conserved variables q_i


  end subroutine conservative

  subroutine computeFlux(cons, flux)

    implicit none

    real(kind = 8), dimension(3), intent(in) :: cons
    real(kind = 8), dimension(3), intent(out) :: flux

    real(kind = 8), dimension(3) :: prim

    call primitive(cons, prim)
    
!!$    TODO
!!$    Compute the flux
    
  end subroutine computeFlux

  function maxSpeed(cons)

    implicit none

    real(kind = 8) :: maxSpeed
    
    real(kind = 8), dimension(3), intent(in) :: cons

    real(kind = 8), dimension(3) :: prim

    real(kind = 8) :: a

    call primitive(cons, prim)
    
    a = sqrt(gamma * prim(PRE) / prim(RHO))

    maxSpeed = a + abs(prim(VEL))

  end function maxSpeed

  function timestep(cons, dx)

    implicit none

    real(kind = 8) timestep

    real(kind = 8), allocatable, dimension(:, :), intent(in) :: cons

    real(kind = 8), intent(in) :: dx

    integer i
    
    timestep = 1d300

    do i = 0, numCells + 1

!!$       TODO
!!$       Compute the maximum wavespeed over the entire domain, and use this to compute the timestep
       c_s = sqrt((gamma * presure) / density)

    end do

    
  end function timestep

  subroutine FORCEflux(consL, consR, force, dx, dt)

    implicit none

    real(kind = 8), dimension(3), intent(in) :: consL, consR
    real(kind = 8), dimension(3), intent(out) :: force
    
    real(kind = 8), intent(in) :: dx, dt

    real(kind = 8), dimension(3) :: fL, fR
    real(kind = 8), dimension(3) :: cellRM, fluxRM


!!$    TODO
!!$    Compute the Richtmyer flux using q_i and q_iMinus1
!!$    Compute the Lax-Friedrichs flux using q_i and q_iMinus1
!!$    Compute the FORCE flux


  end subroutine FORCEflux

  subroutine computeFluxes(cons, flux, dx, dt)

    implicit none

    real(kind = 8), allocatable, dimension(:, :), intent(in) :: cons
    real(kind = 8), allocatable, dimension(:, :), intent(inout) :: flux

    real(kind = 8), intent(in) :: dx, dt

    integer :: i

!!$    TODO  - consider why this is the choice of index range for the loop
    do i = 1, numCells + 1

       
       call FORCEflux(cons(:,i-1), cons(:,i), flux(:,i), dx, dt)

    end do

  end subroutine computeFluxes

  subroutine addFluxes(cons, flux, dx, dt)

    implicit none

    real(kind = 8), allocatable, dimension(:, :), intent(inout) :: cons
    real(kind = 8), allocatable, dimension(:, :), intent(in) :: flux

    real(kind = 8), intent(in) :: dx, dt

    integer :: i

    do i = 1, numCells

       cons(:,i) = cons(:,i) + (dt / dx) * (flux(:, i) - flux(:, i+1))

    end do

  end subroutine addFluxes
  
  
end program euler1D
  
