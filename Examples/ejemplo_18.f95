! ejemplo18.f95
!
! Example of pthread_cond_timedwait
!
! Las funciones pthread_cond_timedwait () y pthread_cond_wait () se bloquearán en una variable de condición.
! Se llamarán con el mutex bloqueado por el hilo de llamada o los resultados de comportamiento serán impredecibles.

! Estas funciones liberan el mutex atómicamente y hacen que el hilo de llamada se bloquee en la variable de condición cond;
! atómicamente aquí significa "atómicamente con respecto al acceso de otro hilo al mutex y luego a la variable de condición".
! Tras el retorno exitoso, el mutex se habrá bloqueado y será propiedad del hilo de llamada.
!
! Valor de retorno
! Una vez completado con éxito, se devolverá un valor de cero;
! de lo contrario, se devolverá un número de error para indicar el error.

module globals
    use, intrinsic :: iso_fortran_env
    use pthread
    implicit none

    logical, parameter :: S = .TRUE.
    logical, parameter :: F = .FALSE.
    integer, parameter :: TIMES = 10

    type :: PROC
        character(len=1)        :: name
        integer                 :: data_
        type(pthread_t)         :: thread
        type(pthread_mutex_t)   :: lock_
        type(pthread_cond_t)    :: cond
        integer(int64)          :: waits
    end type PROC

    ! cloning c struct
    type :: timespec
        integer(kind=int64) :: tv_sec     ! Seconds
        integer(kind=int64) :: tv_nsec    ! Nanoseconds
    end type

end module


module util
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use            :: pthread
    use            :: globals
    implicit none

    integer(int64), external :: getSecs

contains

    subroutine exec_and_wait(args)
        type(PROC), intent(in) :: args
        !integer(kind=int64), target :: time_to_wait
        type(timespec), target :: time_to_wait
        type(PROC), target :: proc_
        integer :: rc
        integer(int64) :: seconds

        seconds = 0
        proc_ = args  ! *proc = (PROC*) args;

        do while(proc_%data_ < TIMES)
            seconds = getSecs() + proc_%waits
            time_to_wait%tv_sec = seconds
            rc = pthread_mutex_lock(c_loc(proc_%lock_))
            rc = pthread_cond_timedwait(c_loc(proc_%cond), c_loc(proc_%lock_), c_loc(time_to_wait))
            proc_%data_ = proc_%data_ + 1
            print *, "increase proc[",proc_%name , "] data to [", proc_%data_, "]"
            rc = pthread_mutex_unlock(c_loc(proc_%lock_))
        end do
    end subroutine

    function init_proc(proc_, routine) result (Sret)
        type(PROC), intent(in), target :: proc_
        type(c_funptr), intent(in), value :: routine
        type(pthread_attr_t), target :: attr
        logical :: Sret
        integer :: rc

        print *, "try to init proc [",proc_%name , "]"
        rc = pthread_attr_init(c_loc(attr))
        rc = pthread_attr_setdetachstate(c_loc(attr), PTHREAD_CREATE_JOINABLE)

        rc = pthread_mutex_init(c_loc(proc_%lock_), c_null_ptr)
        rc = pthread_cond_init(c_loc(proc_%cond), c_null_ptr)
        rc = pthread_create(c_loc(proc_%thread), c_loc(attr), routine, c_loc(proc_))
        rc = pthread_attr_destroy(c_loc(attr))

        Sret = S
    end function
end module


program main
    use, intrinsic :: iso_c_binding
    use            :: pthread
    use            :: util
    use            :: globals
    implicit none

    integer :: rc
    type(PROC) :: proc_a, proc_b
    logical :: resul


    proc_a%name  = "A"
    proc_a%data_ = 0
    proc_a%waits = 2

    proc_b%name  = "B"
    proc_b%data_ = 0
    proc_b%waits = 1

    resul = init_proc(proc_a, c_funloc(exec_and_wait))
    if (.not. resul) then
        print *, "Fail to init proc [",proc_a%name ,"]"
    end if
    resul = init_proc(proc_b, c_funloc(exec_and_wait))
    if (.not. resul) print *, "Fail to init proc [",proc_b%name ,"]"

    rc = pthread_join(proc_a%thread, c_null_ptr)
    rc = pthread_join(proc_b%thread, c_null_ptr)

end program main


function getSecs () result(iTime)
    use iso_fortran_env
    implicit none

    integer(int32) :: values(8)
    integer(int64) :: iTime
    real    :: rTime

    ! Get the values
    call date_and_time(values=values)

    ! From https://gcc.gnu.org/onlinedocs/gfortran/DATE_005fAND_005fTIME.html
    ! values(5) ... The hour of the day
    ! values(6) ... The minutes of the hour
    ! values(7) ... The seconds of the minute
    ! values(8) ... The milliseconds of the second

    ! Calculate time since midnight
    rTime = ( values(5) )*60.         ! Hours to minutes
    rTime = ( rTime + values(6) )*60. ! Minutes to seconds
    rTime = ( rTime + values(7) )*1e3 ! Seconds to milliseconds
    !rTime = rTime + values(8)         ! Add milliseconds

    ! Time in seconds converted to integer
    iTime = int(rTime, int64)
end function
