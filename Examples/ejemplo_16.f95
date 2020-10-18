! ejemplo16.f95
!
!

module util
    use, intrinsic :: iso_c_binding
    use            :: pthread
    implicit none

contains
    subroutine start_routine(arg)
        type(c_ptr), intent(in), target :: arg

        print *, "we are into start_routine"
        call pthread_exit(arg)
    end subroutine
end module

program main
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use            :: util
    implicit none

    type(pthread_attr_t), target :: tattr
    type(pthread_t), target :: tid
    type(c_ptr) :: arg
    integer :: rc
    integer(kind=c_int), target :: scope

    scope = PTHREAD_SCOPE_SYSTEM
    ! initialized with default attributes
    rc = pthread_attr_init (c_loc(tattr))

    rc = pthread_attr_setscope(c_loc(tattr), scope)
    rc = pthread_create (c_loc(tid), c_loc(tattr), c_funloc(start_routine), arg)
    rc = pthread_attr_getscope(c_loc(tattr), c_loc(scope))
    print *, "result / scope after pthread_create : ", rc, scope, Z'10'

    rc = pthread_join(tid, arg)

    print *, "Main completed"

end program main
