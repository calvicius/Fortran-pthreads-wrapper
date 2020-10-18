! -----------------
! ejemplo2.f95
! -----------------
module test_function
    use, intrinsic :: iso_c_binding
    implicit none
    public :: ejemplo2
contains
    subroutine ejemplo2(arg)
        type(c_ptr), intent(in) :: arg
        integer             :: i

        do i = 1, 10
            print *, "cycle num.: ", i, "with argument: ", arg, "of suborutine"
        end do
    end subroutine ejemplo2
end module test_function

program main
    use, intrinsic :: iso_c_binding
    use :: pthread
    use :: test_function
    implicit none
    type(pthread_t), target :: thread
    integer                 :: rc
    integer, target         :: arg = 9  !argument to pass
    type(c_ptr)             :: ptr_arg

    ptr_arg = c_loc(arg)
    rc = pthread_create(c_loc(thread), c_null_ptr, c_funloc(ejemplo2), ptr_arg)
    rc = pthread_join(thread, c_null_ptr)
end program main
