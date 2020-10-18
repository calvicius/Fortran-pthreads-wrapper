! ejemplo17.f95
!
!

module globals
    use pthread
    implicit none

    integer, parameter :: NUMTHREADS = 4
    type(pthread_key_t), target :: glob_var_key
end module


module util
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use            :: globals
    implicit none

contains
    subroutine dosomething
        type(c_ptr) :: glob_spec_var
        character(len=16) :: address
        integer :: addr_num, stat

        ! get thread specific data
        glob_spec_var = pthread_getspecific(glob_var_key)
        ! getting the address as text
        write(address, '(I16)'), glob_spec_var
        ! get the num of address
        read(address,*,iostat=stat) addr_num

        print *, "Thread ", pthread_self(), "before mod value is ", addr_num    ! glob_spec_var is a c_ptr
        addr_num = addr_num + 1

        print *, "Thread ", pthread_self(), " after mod value is ", addr_num   ! glob_spec_var
    end subroutine

    subroutine thread_func
        integer :: rc
        integer, target :: valor

        !int *p = malloc(sizeof(int));
        !*p = 1;
        integer, target :: p

        p = 1
        rc = pthread_setspecific(glob_var_key, c_loc(p))
        call dosomething
        call dosomething
        rc = pthread_setspecific(glob_var_key, c_null_ptr)

        call pthread_exit(c_null_ptr)
    end subroutine
end module

program main
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use            :: util
    use            :: globals
    implicit none

    type(pthread_t), target :: threads(NUMTHREADS)
    integer :: i, rc

    rc = pthread_key_create(c_loc(glob_var_key), c_null_ptr)
    if (rc /= 0) print *, "key_create error"

    do i = 1, NUMTHREADS
        rc = pthread_create(c_loc(threads(i)), c_null_ptr, c_funloc(thread_func), c_null_ptr)
        if (rc /= 0) print *, "pthread_create error"
    end do

    do i = 1, NUMTHREADS
        rc = pthread_join(threads(i), c_null_ptr)
        if (rc /= 0) print *, "pthread_join error"
    end do

end program main
