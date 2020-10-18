! -----------------
! Ejemplo1.f95
! -----------------
module test_function
    implicit none
    public :: ejemplo1
contains
    subroutine ejemplo1()
        integer :: i

        do i = 1, 10
            print '(i2, ": Ejemplo1 simple sin argumentos")', i
        end do
    end subroutine ejemplo1
end module test_function

program main
    use, intrinsic :: iso_c_binding
    use :: pthread
    use :: test_function
    implicit none
    type(pthread_t), target :: thread
    integer                 :: rc

    rc = pthread_create(c_loc(thread), c_null_ptr, c_funloc(ejemplo1), c_null_ptr)
    rc = pthread_join(thread, c_null_ptr)
end program main
