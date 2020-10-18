module util
    implicit none
    public :: ejemplo3
contains
    subroutine ejemplo3(n)
        integer, intent(in) :: n
        integer             :: i

        do i = 1, 10
            call sleep(1)
            print '("Thread #", i0, " - ", i2)', n, i
        end do
    end subroutine ejemplo3
end module util

program main
    use, intrinsic :: iso_c_binding
    use :: pthread
    use :: util
    integer, parameter :: NTHREADS = 5
    integer            :: i, rc

    type(pthread_t), target :: threads(NTHREADS)
    integer,         target :: routines(NTHREADS) = [ (i, i = 1, NTHREADS) ]

    do i = 1, NTHREADS
        rc = pthread_create(c_loc(threads(i)), c_null_ptr, c_funloc(ejemplo3), c_loc(routines(i)))
    end do

    do i = 1, NTHREADS
        rc = pthread_join(threads(i), c_loc(routines(i)))
    end do
end program main
