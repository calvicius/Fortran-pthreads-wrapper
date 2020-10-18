! ejemplo20.f95
!
! Example of pthread_rwlock_trywrlock
!
!
module globals
    use pthread
    implicit none

    type(pthread_rwlock_t), target :: rwlock
end module


module util
    use iso_c_binding
    use pthread
    use globals
    implicit none

contains
    subroutine wrlockThread(arg)
        type(c_ptr), intent(in) :: arg
        integer :: rc
        integer :: count_

        count_ = 0

        print *, "Entered thread, getting write lock with timeout : ", pthread_self()
    1111 continue   ! Retry
        rc = pthread_rwlock_trywrlock(c_loc(rwlock))
        if (rc == 16) then   ! 16 = EBUSY --> see errno.h
            if (count_ >= 10) then
                print *, "Retried too many times, failure! : ", pthread_self()
                stop
            end if
            count_ = count_ + 1
            print *, " Go off an do other work, then RETRY... : ", pthread_self()
            call sleep(1)
    goto 1111
        end if
        print *, "pthread_rwlock_trywrlock() 1 ", rc
        print *, "Got the write lock : ", pthread_self()

        call sleep(2)

        print *, "Unlock the write lock : ", pthread_self()
        rc = pthread_rwlock_unlock(c_loc(rwlock))
        print *, "pthread_rwlock_unlock() : ", rc

        print *, "Secondary thread complete : ", pthread_self()
        call pthread_exit(arg)
    end subroutine
end module


program main
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use            :: globals
    use util
    implicit none

    integer :: rc
    type(pthread_t), target :: thread, thread2

    rc = 0
    rwlock = PTHREAD_RWLOCK_INITIALIZER

    print *, "Main, get the write lock"
    rc = pthread_rwlock_wrlock(c_loc(rwlock))
    if (rc /= 0 ) print *, "Error in rwlock_wrlock"

    print *, "Main, create the timed write lock threads"
    rc = pthread_create(c_loc(thread), c_null_ptr, c_funloc(wrlockThread), c_null_ptr)
    if (rc /= 0 ) print *, "Error in create thread"

    rc = pthread_create(c_loc(thread2), c_null_ptr, c_funloc(wrlockThread), c_null_ptr)
    if (rc /= 0 ) print *, "Error in create thread (2)"

    print *, "Main, wait a bit holding this write lock"
    call sleep(1)

    print *, "Main, Now unlock the write lock"
    rc = pthread_rwlock_unlock(c_loc(rwlock))
    if (rc /= 0 ) print *, "Error in rwlock_unlock"

    print *, "Main, wait for the threads to end"
    rc = pthread_join(thread, c_null_ptr)
    if (rc /= 0) print *, "pthread_join", rc

    rc = pthread_join(thread2, c_null_ptr)
    if (rc /= 0) print *, "pthread_join", rc

    rc = pthread_rwlock_destroy(c_loc(rwlock))
    if (rc /= 0) print *, "pthread_rwlock_destroy()", rc

    print *, "Main completed"

end program main
