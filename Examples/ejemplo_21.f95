! ejemplo21.f95
!
! Example of pthread_testcancel
!


module util
    use iso_c_binding
    use pthread
    implicit none

contains

    subroutine cleanupHandler(parm)
        type(c_ptr), intent(in) :: parm
        print *, "Inside cancellation cleanup handler"
    end subroutine

    subroutine thread_fun(arg)
        type(c_ptr), intent(in) :: arg
        integer :: rc, i

        i=1
        rc = pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, c_null_ptr)
        !Synchronized Cancel, Cancel at Next Cancel Point
        rc = pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, c_null_ptr)

        print *, "thread start"
        do  !while(1)
            i = i + 1
            ! Manually create a cancel point to prevent the thread from having no cancel point,
            ! resulting in the thread not canceling.*/
            call pthread_testcancel()
        end do

        call pthread_exit(arg)
    end subroutine
end module


program main
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use util
    implicit none

    type(c_ptr) :: ret
    integer :: iret
    type(pthread_t), target :: tid

    ret = c_null_ptr
    iret = 0

    iret = pthread_create(c_loc(tid), c_null_ptr,c_funloc(thread_fun), c_null_ptr)
    call sleep(1);

    iret = pthread_cancel(tid) ! Cancel thread
    iret = pthread_join(tid, ret)
    print *, "thread 3 exit code ", ret, iret

end program main
