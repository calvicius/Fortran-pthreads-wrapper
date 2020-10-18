! ejemplo_04.f95
!
! Example that shows how to exit (not join)
!
module test
    use, intrinsic :: iso_c_binding
    use :: pthread
    implicit none
    public :: ejemplo4
contains
    subroutine ejemplo4(n)
        integer, intent(in) :: n

        call sleep(1)
        print *, "Thread #", n

        call pthread_exit(c_null_ptr)
    end subroutine ejemplo4
end module test

program main
    use, intrinsic :: iso_c_binding
    use :: pthread
    use :: test
    integer, parameter :: NUM_NTHREADS = 5
    integer            :: t, rc

    type(pthread_t), target :: threads(NUM_NTHREADS)
    integer,         target :: routines(NUM_NTHREADS) = [ (t, t = 1, NUM_NTHREADS) ]

    do t = 1, NUM_NTHREADS
        rc = pthread_create(c_loc(threads(t)), c_null_ptr, c_funloc(ejemplo4), c_loc(routines(t)))
        print *, "result create (0 means OK): ", rc
    end do

    call sleep(3)
    !call pthread_exit(c_null_ptr)
end program main
