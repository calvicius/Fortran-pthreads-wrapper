! ejemplo14.f95
!
!

module globals
    use            :: pthread
    implicit none

    type(pthread_rwlock_t), target :: rwlock
end module


module util
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use globals
    implicit none

contains

    subroutine rdlockThread(arg)
        type(c_ptr), intent(in) :: arg
        integer :: rc

        print *, "Entered thread, getting read lock"
        rc = pthread_rwlock_rdlock(c_loc(rwlock))
        if (rc /= 0) print *, "pthread_rwlock_rdlock, Error"
        print *, "got the rwlock read lock"

        call sleep(5)

        print *, "unlock the read lock"
        rc = pthread_rwlock_unlock(c_loc(rwlock))
        if (rc /= 0) print *, "pthread_rwlock_unlock, Error"
        print *, "Secondary thread unlocked"
        call pthread_exit(c_null_ptr)
    end subroutine


    subroutine wrlockThread(arg)
        type(c_ptr), intent(in) :: arg
        integer :: rc

        print *, "Entered thread, getting write lock"
        rc = pthread_rwlock_wrlock(c_loc(rwlock))
        if (rc /= 0) print *, "pthread_rwlock_wrlock, Error"

        call sleep(5)
        print *, "Got the rwlock write lock, now unlock"
        rc = pthread_rwlock_unlock(c_loc(rwlock))
        if (rc /= 0) print *, "pthread_rwlock_unlock, Error"
        print *, "Secondary thread unlocked"
        call pthread_exit(arg)
    end subroutine
end module



program main
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use            :: globals
    use            :: util
    implicit none

    integer :: rc
    type(pthread_t), target :: thread, thread1

    print *, "Main, initialize the read write lock"
    rc = pthread_rwlock_init(c_loc(rwlock), c_null_ptr)
    if (rc /= 0) print *, "pthread_rwlock_init, Error"

    print *, "Main, grab a read lock"
    rc = pthread_rwlock_rdlock(c_loc(rwlock))
    if (rc /= 0) print *, "pthread_rwlock_rdlock, Error"

    print *, "Main, grab the same read lock again"
    rc = pthread_rwlock_rdlock(c_loc(rwlock))
    if (rc /= 0) print *, "pthread_rwlock_rdlock second, Error"

    print *, "Main, create the read lock thread"
    rc = pthread_create(c_loc(thread), c_null_ptr, c_funloc(rdlockThread), c_null_ptr)
    if (rc /= 0) print *, "pthread_create, Error"

    print *, "Main - unlock the first read lock"
    rc = pthread_rwlock_unlock(c_loc(rwlock))
    if (rc /= 0) print *, "pthread_rwlock_unlock, Error"

    print *, "Main, create the write lock thread"
    rc = pthread_create(c_loc(thread1), c_null_ptr, c_funloc(wrlockThread), c_null_ptr)
    if (rc /= 0) print *, "pthread_create, Error"

    call sleep(5)
    print *, "Main - unlock the second read lock"
    rc = pthread_rwlock_unlock(c_loc(rwlock))
    if (rc /= 0) print *, "pthread_rwlock_unlock, Error"

    print *, "Main, wait for the threads"
    rc = pthread_join(thread, c_null_ptr)
    if (rc /= 0) print *, "pthread_join, Error"

    rc = pthread_join(thread1, c_null_ptr)
    if (rc /= 0) print *, "pthread_join, Error"

    rc = pthread_rwlock_destroy(c_loc(rwlock))
    if (rc /= 0) print *, "pthread_rwlock_destroy, Error"

    print *, "Main completed"

end program main
