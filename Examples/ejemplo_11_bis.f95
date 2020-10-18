! ejemplo11_bis.f95
!
! playing with priorities and barriers


module globals
    use            :: pthread
    implicit none

    integer, parameter :: PRIO_GROUP = 2
    type(pthread_t), target :: hpt(PRIO_GROUP)
    type(pthread_t), target :: mpt(PRIO_GROUP)
    type(pthread_t), target :: lpt(PRIO_GROUP)

    type(pthread_barrier_t), target :: mybarrier
end module


module funcs
    use, intrinsic :: iso_c_binding
    use            :: globals
    implicit none


end module


module util
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use            :: globals
    implicit none

    integer :: rc1

contains


    subroutine highprior_thread()
        type(pthread_t) :: thread_id

        thread_id  = pthread_self()
        rc1 = 1
        do while(rc1 /= 0)
            rc1 = pthread_barrier_wait(c_loc(mybarrier))
            print *, "linea 46 thread_id (highprior_thread)   : ", thread_id
        end do
        call pthread_exit(c_null_ptr)
    end subroutine


    subroutine middleprior_thread()
        type(pthread_t) :: thread_id

        thread_id = pthread_self()
        rc1 = 1
        do while(rc1 /= 0)
            rc1 = pthread_barrier_wait(c_loc(mybarrier))
            print *, "linea 60 thread_id (middleprior_thread) : ", thread_id
        end do
        call pthread_exit(c_null_ptr)

    end subroutine


    subroutine lowprior_thread()
        type(pthread_t) :: thread_id

        thread_id = pthread_self()
        rc1 = 1
        do while(rc1 /= 0)
            rc1 = pthread_barrier_wait(c_loc(mybarrier))
            print *, "linea 75 thread_id (lowprior_thread)    : ", thread_id
        end do
        !rc1 = pthread_barrier_destroy(c_loc(mybarrier))
        call pthread_exit(c_null_ptr)
    end subroutine

end module



program main
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use            :: pthread
    use            :: globals
    use            :: util
    implicit none

    type(sched_param),    target :: my_param
    type(pthread_attr_t), target :: hp_attr     !high priority
    type(pthread_attr_t), target :: mp_attr     !medium priority
    type(pthread_attr_t), target :: lp_attr     !low prio

    integer :: i, rc, min_priority

    ! MAIN-THREAD WITH LOW PRIORITY
    my_param%sched_priority = sched_get_priority_min(SCHED_FIFO)
    rc = pthread_setschedparam(pthread_self(), SCHED_RR, c_loc(my_param))
    print *, "linea 52 - pthread_self(), my_param%sched_priority : ", pthread_self(), my_param%sched_priority

    ! SCHEDULING POLICY AND PRIORITY FOR OTHER THREADS
    rc = pthread_attr_init(c_loc(lp_attr))
    rc = pthread_attr_init(c_loc(mp_attr))
    rc = pthread_attr_init(c_loc(hp_attr))

    rc = pthread_attr_setinheritsched(c_loc(lp_attr), PTHREAD_EXPLICIT_SCHED)
    rc = pthread_attr_setinheritsched(c_loc(mp_attr), PTHREAD_EXPLICIT_SCHED)
    rc = pthread_attr_setinheritsched(c_loc(hp_attr), PTHREAD_EXPLICIT_SCHED)

    rc = pthread_attr_setschedpolicy(c_loc(lp_attr), SCHED_OTHER)
    rc = pthread_attr_setschedpolicy(c_loc(mp_attr), SCHED_OTHER)
    rc = pthread_attr_setschedpolicy(c_loc(hp_attr), SCHED_OTHER)

    min_priority = sched_get_priority_min(SCHED_OTHER)     ! = -15 ; see ejemplo11.f95

    my_param%sched_priority = min_priority + 10
    rc = pthread_attr_setschedparam(c_loc(lp_attr), c_loc(my_param))

    my_param%sched_priority = min_priority + 20
    rc = pthread_attr_setschedparam(c_loc(mp_attr), c_loc(my_param))

    my_param%sched_priority = min_priority + 29
    rc = pthread_attr_setschedparam(c_loc(hp_attr), c_loc(my_param))

    rc = pthread_barrier_init(c_loc(mybarrier), c_null_ptr, PRIO_GROUP*3)

    do i = 1, PRIO_GROUP
        rc = pthread_create(c_loc(lpt(i)), c_loc(lp_attr), c_funloc(lowprior_thread), c_null_ptr)
        rc = pthread_create(c_loc(mpt(i)), c_loc(mp_attr), c_funloc(middleprior_thread), c_null_ptr)
        rc = pthread_create(c_loc(hpt(i)), c_loc(hp_attr), c_funloc(highprior_thread), c_null_ptr)
    end do

    do i = 1, PRIO_GROUP
        rc = pthread_join(hpt(i), c_null_ptr)
        rc = pthread_join(mpt(i), c_null_ptr)
        rc = pthread_join(lpt(i), c_null_ptr)
    end do

    print *, "main exiting ..."


end program main
