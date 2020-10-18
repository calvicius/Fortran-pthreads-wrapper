! ejemplo_06.f95
!
! Example from https://computing.llnl.gov/tutorials/pthreads/
! A "hello world" Pthreads program which demonstrates another safe way
!   to pass arguments to threads during thread creation.  In this case,
!   a structure is used to pass multiple arguments.
!

module globals
    implicit none
    integer, parameter :: NUM_THREADS = 8
    type :: s_thread_data
        integer ::	thread_id
        integer ::  sumation
        character(len= 30) :: message
    end type s_thread_data


    type(s_thread_data), dimension(NUM_THREADS), public, target :: thread_data_array
end module

module test
    use, intrinsic :: iso_c_binding
    use :: pthread
    use :: globals
    implicit none
    public :: ejemplo6

contains
    subroutine ejemplo6(threadarg)
        type(s_thread_data) , intent(in) :: threadarg
        integer :: taskid, sumation
        character(len=30) :: hello_msg
        type(s_thread_data) :: my_data

        my_data = threadarg
        taskid = my_data%thread_id
        sumation = my_data%sumation
        hello_msg = my_data%message

        call sleep(1)
        print *, "Thread ", taskid, " ", hello_msg, "Sum = ", sumation
    end subroutine ejemplo6
end module test

program main
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use :: pthread
    use :: globals
    use :: test

    integer            :: t, rc, sumation
    type(pthread_t), target :: threads(NUM_THREADS)
    !integer(int64),  target :: taskids(NUM_THREADS) = [ (t, t = 1, NUM_THREADS) ]
    type(c_ptr) :: ptr_arr

    character(len=30), dimension(8) :: messages

    messages(1) = "English: Hello World!"
    messages(2) = "French: Bonjour, le monde!"
    messages(3) = "Spanish: Hola al mundo"
    messages(4) = "Klingon: Nuq neH!"
    messages(5) = "German: Guten Tag, Welt!"
    messages(6) = "Russian: Zdravstvuyte, mir!"
    messages(7) = "Japan: Sekai e konnichiwa!"
    messages(8) = "Latin: Orbis, te saluto!"

    sumation = 0

    do t = 1, NUM_THREADS
        sumation = sumation + t
        thread_data_array(t)%thread_id = t
        thread_data_array(t)%sumation = sumation
        thread_data_array(t)%message = messages(t)
        ptr_arr = c_loc(thread_data_array(t))
        print *, "Creating thread : ", t

        rc = pthread_create(c_loc(threads(t)), c_null_ptr, c_funloc(ejemplo6), ptr_arr)
        if (rc /= 0) then
            print *, "ERROR; return code from pthread_create() is ", rc
            exit
        end if
    end do

    call pthread_exit(c_null_ptr)
end program main
