! ejemplo19.f95
!
! Example of pthread_mutexattr_getprotocol
!
! RETURN VALUE

!    Upon successful completion, the pthread_mutexattr_getprotocol() and pthread_mutexattr_setprotocol()
!    functions shall return zero; otherwise, an error number shall be returned to indicate the error.

!ERRORS

!    The pthread_mutexattr_setprotocol() function shall fail if:

!    [ENOTSUP] = 129
!        The value specified by protocol is an unsupported value.

!    The pthread_mutexattr_getprotocol() and pthread_mutexattr_setprotocol() functions may fail if:

!    [EINVAL] = 22
!        The value specified by attr or protocol is invalid.
!    [EPERM] = 1
!        The caller does not have the privilege to perform the operation.

!    These functions shall not return an error code of [EINTR].

! See sched.c
! See msdn: there are actually 7 priorities:
!    THREAD_PRIORITY_IDLE    -      -15
!    THREAD_PRIORITY_LOWEST          -2
!    THREAD_PRIORITY_BELOW_NORMAL    -1
!    THREAD_PRIORITY_NORMAL           0
!    THREAD_PRIORITY_ABOVE_NORMAL     1
!    THREAD_PRIORITY_HIGHEST          2
!    THREAD_PRIORITY_TIME_CRITICAL   15
!
! pthread_mutexattr_setprotocol in windows-10 only accepts from -1 to -8, nor 0 also

module globals
    use pthread
    implicit none

    type(pthread_mutex_t), target :: m1
    type(pthread_mutexattr_t), target ::  mutexattr_prioinherit
end module



program main
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use            :: globals
    implicit none

    integer :: rtn
    integer, target :: mutex_protocol

    rtn = 0

    ! What is the default protocol on the host?
    rtn = pthread_mutexattr_init(c_loc(mutexattr_prioinherit))
    if (rtn /= 0) then
        print *, "error en mutexattr_init"
        stop
    end if

    rtn = pthread_mutexattr_getprotocol(c_loc(mutexattr_prioinherit), c_loc(mutex_protocol))
    if (rtn /= 0) then
        print *, "error en mutexattr_getprotocol"
        stop
    end if

    if (mutex_protocol == PTHREAD_PRIO_PROTECT) then
        print *, "Default mutex protocol is PTHREAD_PRIO_PROTECT"
    else if (mutex_protocol == PTHREAD_PRIO_INHERIT) then
        print *, "Default mutex protocol is PTHREAD_PRIO_INHERIT"
    else if (mutex_protocol == PTHREAD_PRIO_NONE) then
        print *, "Default mutex protocol is PTHREAD_PRIO_NONE"
    else
        print *, "Default mutex protocol is unrecognized"
    end if

    ! Set this mutex attribute to INHERIT
    rtn = pthread_mutexattr_setprotocol(c_loc(mutexattr_prioinherit), -8)
    ! Only accepts the range -1 to -8 (in my windows 10)
    if (rtn /= 0) then
        ! with PTHREAD_PRIO_INHERIT gives error 22 -> EINVAL
        print *, "error en mutexattr_setprotocol", rtn
        stop
    end if

    ! Initialize a mutex with the attribute object */
    rtn = pthread_mutex_init(c_loc(m1), c_loc(mutexattr_prioinherit))
    if (rtn /= 0) then
        print *, "error en mutex_init"
        stop
    end if

    rtn = pthread_mutex_lock(c_loc(m1))
    if (rtn /= 0) then
        print *, "error en mutex_loc"
        stop
    end if

    rtn = pthread_mutex_unlock(c_loc(m1))
    if (rtn /= 0) then
        print *, "error en mutex_unloc"
        stop
    end if

end program main
