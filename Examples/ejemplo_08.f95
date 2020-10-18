! ejemplo_08.f95
!
! Example from https://computing.llnl.gov/tutorials/pthreads/

! Gestión de pilas

!Rutinas:
!
!     pthread_attr_getstacksize (attr, stacksize)
!     pthread_attr_setstacksize (attr, stacksize)
!     pthread_attr_getstackaddr (attr, stackaddr)
!     pthread_attr_setstackaddr (attr, stackaddr)

!Prevención de problemas de pila:
!
!     El estándar POSIX no dicta el tamaño de la pila de un hilo. Esto depende de la implementación y varía.
!
!     Exceder el límite de pila predeterminado es a menudo muy fácil de hacer, con los resultados habituales:
!     terminación del programa y / o datos corruptos.
!
!     Los programas seguros y portátiles no dependen del límite de pila predeterminado,
!     sino que asignan explícitamente suficiente pila para cada subproceso utilizando la
!     rutina pthread_attr_setstacksize.
!
!     Las rutinas pthread_attr_getstackaddr y pthread_attr_setstackaddr pueden ser utilizadas por aplicaciones
!     en un entorno donde la pila de un subproceso debe colocarse en alguna región particular de la memoria.
!

module globals
    use, intrinsic :: iso_c_binding
    use :: pthread
    implicit none

    integer, parameter :: NTHREADS = 4
    integer, parameter :: N = 1000
    integer, parameter :: MEGEXTRA = 1000000

    type(pthread_attr_t), target    :: attr
    type(c_ptr)         :: ptr_attr
end module

module test
    use, intrinsic :: iso_c_binding
    use :: pthread
    use :: globals
    implicit none
    public :: DoWork

contains
    subroutine DoWork(threadid)
        integer, intent(in), target :: threadid
        real :: A(N, N)
        integer :: i, j, rc
        integer :: tid
        integer(kind=c_size_t), target :: mystacksize

        call sleep(1)
        tid = threadid
        rc = pthread_attr_getstacksize (c_loc(attr), c_loc(mystacksize))
        print *, "Thread ", tid, " stack size = ", mystacksize, " bytes"

        do i=1, N
            do j=1, N
                A(i, j) = ((i*j)/3.452) + (N-i)
            end do
        end do
        call pthread_exit(c_loc(threadid))
    end subroutine DoWork
end module test

program main
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use :: pthread
    use :: globals
    use :: test


    integer(kind=c_size_t), target :: stacksize, stackbase
    integer             :: rc
    integer, target     :: estatus
    integer             :: t

    type(c_ptr)         :: ptr_status
    type(pthread_t), target         :: threads(NTHREADS)
    integer, target                 :: routines(NTHREADS) = [ (t, t = 1, NTHREADS) ]

    ptr_status = c_loc(estatus)
    ptr_attr   = c_loc(attr)

    ! The stacksize attribute shall define the minimum stack size (in  bytes)
    ! allocated for the created threads stack.
    ! RETURN VALUE
    !   Upon    successful    completion,    pthread_attr_getstacksize()    and
    !   pthread_attr_setstacksize() shall return a value of  0;  otherwise,  an
    !   error number shall be returned to indicate the error.
    rc = pthread_attr_init(ptr_attr)    ! 0=OK, else error
    rc = pthread_attr_getstacksize (ptr_attr, c_loc(stacksize))

    print *, "Default stack size = ", stacksize
    stacksize = sizeof(real)*N*N+MEGEXTRA
    print *, "Amount of stack needed per thread = ", stacksize

    rc = pthread_attr_setstacksize (ptr_attr, c_loc(stacksize))
    ! http://users.cs.cf.ac.uk/Dave.Marshall/CM2204/PDF/C.pdf --> Chapter 28
    ! Linux : PTHREAD_STACK_MIN (16384) bytes
    !stackbase = sizeof(stacksize) + Z'4000'     ! 8 + 16384
    !rc = pthread_attr_setstackaddr(ptr_attr, c_loc(stackbase))  ! 0 = OK, /= 0 --> Err
    !print *, "linea 107 ", rc, stackbase
    do t = 1, NTHREADS
        rc = pthread_create(c_loc(threads(t)), ptr_attr, c_funloc(DoWork), c_loc(routines(t)))
        if (rc /= 0) then
            print *, "ERROR; return code from pthread_create() is ", rc
            exit
        end if
        !rc = pthread_attr_getstackaddr(ptr_attr, c_loc(stackbase))  ! 0 = OK, /= 0 --> Err
        !print *, "linea 117 ", rc, stackbase
    end do

    print *, "Main: program completed. Exiting..."
    !call pthread_exit(c_loc(routines(1)))  ! already exited in suborutine
    call sleep(3)
end program main
