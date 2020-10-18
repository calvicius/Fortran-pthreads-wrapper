! ejemplo11.f95
!
! test sched priorities functions
!
! Note: see sched.c
! sched_yield in MINGW does nothing
!
! ...
! int sched_yield(void)
! {
!  Sleep(0);
!  return 0;
! }
!
! pthread_attr_setschedpolicy only accepts SCHED_OTHER == 0
! int pthread_attr_setschedpolicy (pthread_attr_t *attr, int pol)
!{
!  if (!attr || pol < SCHED_MIN || pol > SCHED_MAX)
!    return EINVAL;
!  if (pol != SCHED_OTHER)
!    return ENOTSUP;
!  return 0;
!}


module globals
    use, intrinsic :: iso_c_binding
    use :: pthread
    implicit none

    type(pthread_t), target :: tid
end module


module auxiliary_mod
    use, intrinsic :: iso_c_binding
    use :: pthread
    implicit none

    public auxiliary
contains
    subroutine auxiliary(x)
        integer, intent(in) :: x
        print *, "into auxiliary subroutine ", x
    end subroutine auxiliary
end module auxiliary_mod

program main
    use, intrinsic :: iso_c_binding
    use :: pthread
    use :: globals
    use auxiliary_mod
    implicit none


    integer(kind=c_int), target :: estatus, pol
    integer :: ret
    integer :: newprio = 14 ! in windows priority_min = -15 and max = 15 see below
    integer, target :: arg = 99
    integer, target :: tipo

    type(pthread_attr_t), target :: tattr
    !type(pthread_t), target :: tid
    type(sched_param), target :: param

    ! initialized with default attributes
    ret = pthread_attr_init (c_loc(tattr))
print *, "linea 47 ", ret

    ! safe to get existing scheduling param
    ret = pthread_attr_getschedparam (c_loc(tattr), c_loc(param))
print *, "Linea 52 ", param%sched_priority

    ! set the priority; others are unchanged
    param%sched_priority = newprio

    ret = pthread_attr_setschedpolicy(c_loc(tattr), SCHED_OTHER)   ! SCHED_FIFO and SCHED_RR returns /= 0 in Windows
print *, "linea 58 ", ret

    ret = pthread_attr_getschedpolicy(c_loc(tattr), c_loc(pol))
print *, "linea 61 ", ret, pol, tattr%param%sched_priority

    ! setting the new scheduling param
    ret = pthread_attr_setschedparam (c_loc(tattr), c_loc(param))
print *, "linea 65 ", ret, param%sched_priority, tattr%param%sched_priority

    ! specify explicit scheduling
    ret = pthread_attr_setinheritsched (c_loc(tattr), PTHREAD_EXPLICIT_SCHED)
print *, "linea 69 ", ret, tattr%param%sched_priority

    ret = pthread_attr_getinheritsched(c_loc(tattr), c_loc(estatus))
print *, "linea 72 ", ret, estatus

    ! with new priority specified
    ret = pthread_create (c_loc(tid), c_loc(tattr), c_funloc(auxiliary), c_loc(arg))
print *, "linea 76 ", ret

    ret = pthread_setschedparam(pthread_self(), SCHED_RR, c_loc(param))
    ret = pthread_getschedparam(pthread_self(), c_loc(tipo), c_loc(param))
print *, "linea 80 ", ret, param%sched_priority

    ret = pthread_join(tid, c_loc(arg))

    ret = sched_get_priority_min(SCHED_FIFO)
print *, "linea 106 ret = -15", ret
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
    ret = sched_get_priority_min(SCHED_RR)
print *, "linea 117 ret = -15", ret
    ret = sched_get_priority_min(SCHED_OTHER)
print *, "linea 119 ret = -15", ret
    ret = sched_get_priority_max(SCHED_FIFO)
print *, "linea 119 ret = 15", ret
end program main
