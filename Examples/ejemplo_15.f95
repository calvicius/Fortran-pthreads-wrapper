! ejemplo15.f95
!
!

module globals
    use            :: pthread
    implicit none

    type(pthread_rwlock_t), target :: rwlock1, rwlock2
    !type(pthread_rwlock_t) :: PTHREAD_RWLOCK_INITIALIZER			!(pthread_rwlock_t)GENERIC_INITIALIZER
end module




program main
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use            :: globals
    implicit none

    integer :: rc
    type(pthread_t), target :: thread, thread1
    type(pthread_rwlockattr_t), target :: attr

    PTHREAD_RWLOCK_INITIALIZER%rwlockt = GENERIC_INITIALIZER
    rwlock2 = PTHREAD_RWLOCK_INITIALIZER

    print *, "Create a default rwlock attribute"
    rc = pthread_rwlockattr_init(c_loc(attr))
    if (rc /= 0) print *, "pthread_rwlockattr_init, Error"

    print *, "Use the rwlock attributes to created rwlocks here"
    rc = pthread_rwlock_init(c_loc(rwlock1), c_loc(attr))
    if (rc /= 0) print *, "pthread_rwlock_init, Error"

    print *, "The rwlock1 is now ready for use."
    print *, "The rwlock2 that was statically initialized ", &
            "was ready when the main routine was entered"

    print *, "Destroy rwlock attribute"
    rc = pthread_rwlockattr_destroy(c_loc(attr))
    if (rc /= 0) print *, "pthread_rwlockattr_destroy, Error"

    print *, "Use the rwlocks"
    rc = pthread_rwlock_rdlock(c_loc(rwlock1))
    if (rc /= 0) print *, "pthread_rwlock_rdlock (1), Error"

    rc = pthread_rwlock_wrlock(c_loc(rwlock2))
    if (rc /= 0) print *, "pthread_rwlock_rdlock (2), Error"

    rc = pthread_rwlock_unlock(c_loc(rwlock1))
    if (rc /= 0) print *, "pthread_rwlock_unlock (1), Error"

    rc = pthread_rwlock_unlock(c_loc(rwlock2))
    if (rc /= 0) print *, "pthread_rwlock_unlock (2), Error"

    print *, "Destroy the rwlocks"
    rc = pthread_rwlock_destroy(c_loc(rwlock1))
    if (rc /= 0) print *, "pthread_rwlock_destroy (1), Error"

    rc = pthread_rwlock_destroy(c_loc(rwlock2))
    if (rc /= 0) print *, "pthread_rwlock_destroy (2), Error"


    print *, "Main completed"

end program main
