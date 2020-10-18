! ejemplo13.f95
!
!

module globals
    use            :: pthread
    implicit none

    type(pthread_spinlock_t), target :: slock
end module


module util
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use globals
    implicit none

contains

    subroutine splock
        integer :: i = 100, rc
        do while(i>0)
            rc = pthread_spin_lock(c_loc(slock))
            if (rc /= 0) print *, "Error en spin_loc"
            i = i - 1
            ! forced error for testing
            rc = pthread_spin_trylock(c_loc(slock))
        print *, "line 28 /= 0 ", rc    ! 16 == BUSY
            rc = pthread_spin_unlock(c_loc(slock))
            if (rc /= 0) print *, "Error en spin_unloc"
        end do
    end subroutine
end module



program main
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use            :: globals
    use            :: util
    implicit none

    integer :: rc

    rc = pthread_spin_init(c_loc(slock), PTHREAD_PROCESS_PRIVATE)
    if (rc /= 0) print *, "Error en spin_init"

    call splock

    rc = pthread_spin_destroy(c_loc(slock))
    if (rc /= 0) print *, "Error en spin_destroy"

    stop
end program main
