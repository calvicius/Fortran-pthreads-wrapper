! ejemplo_05.f95
!
! A "hello world" Pthreads program which demonstrates one safe way
! to pass arguments to threads during thread creation.
!
module test
    use, intrinsic :: iso_c_binding
    use :: pthread
    implicit none
    public :: ejemplo5
    character(len=30), dimension(8) :: messages

contains
    subroutine ejemplo5(n)
        integer, intent(in) :: n
        messages(1) = "English: Hello World!"
        messages(2) = "French: Bonjour, le monde!"
        messages(3) = "Spanish: Hola al mundo"
        messages(4) = "Klingon: Nuq neH!"
        messages(5) = "German: Guten Tag, Welt!"
        messages(6) = "Russian: Zdravstvuyte, mir!"
        messages(7) = "Japan: Sekai e konnichiwa!"
        messages(8) = "Latin: Orbis, te saluto!"

        call sleep(1)
        print *, "Thread : ", n, "message: ", messages(n)
    end subroutine ejemplo5
end module test

program main
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use :: pthread
    use :: test
    integer, parameter :: NUM_NTHREADS = 8
    integer            :: t, rc

    type(pthread_t), target :: threads(NUM_NTHREADS)
    integer(int64),  target :: taskids(NUM_NTHREADS) = [ (t, t = 1, NUM_NTHREADS) ]

    do t = 1, NUM_NTHREADS
        rc = pthread_create(c_loc(threads(t)), c_null_ptr, c_funloc(ejemplo5), c_loc(taskids(t)))
        if (rc /= 0) then
            print *, "ERROR; return code from pthread_create() is ", rc
            exit
        end if
    end do

    do t = 1, NUM_NTHREADS
        rc = pthread_join(threads(t), c_loc(taskids(t)))
    end do
end program main
