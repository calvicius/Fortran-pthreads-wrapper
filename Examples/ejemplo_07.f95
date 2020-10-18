! ejemplo_07.f95
!
! Example from https://computing.llnl.gov/tutorials/pthreads/

! "join" es una forma de lograr la sincronización entre hilos. Por ejemplo:
! master thread --> pthread_create --> (worker thread --> pthread_exit) --> pthread_join
!
! La subrutina pthread_join () bloquea el hilo de llamada hasta que el hilo con el thread_id especificado termina.
!
! El programador puede obtener el estado de retorno de terminación del hilo de destino si se especificó en
! la llamada del hilo de destino a pthread_exit ().
!
! Un hilo que haga join puede coincidir con una llamada pthread_join ().
! Es un error lógico intentar múltiples combinaciones en el mismo hilo.
!
! Otros dos métodos de sincronización, mutex y variables de condición, se discutirán más adelante.

!¿Joinable o no?
!
! Cuando se crea un hilo, uno de sus atributos define si se puede unir(join) o desconectar(detach).
! Solo se pueden unir los subprocesos que se crean como unibles(joinable).
! Si un hilo se crea como separado(detached), nunca se podrá hacer un join.
!
! El borrador final del estándar POSIX especifica que los subprocesos deben crearse como unibles(joinable).
!
! Para crear explícitamente un hilo como unible o separado, se usa el argumento attr en la rutina pthread_create ().
! El proceso típico de 4 pasos es:
!
!         Declarar una variable de atributo pthread del tipo de datos pthread_attr_t
!         Inicializar la variable de atributo con pthread_attr_init()
!         Establecer el estado separado del atributo con pthread_attr_setdetachstate()
!         Cuando termina, liberar los recursos de la biblioteca utilizados por el atributo con pthread_attr_destroy()

! Desmontaje (detaching):
!
!     La rutina pthread_detach () se puede utilizar para separar explícitamente un hilo aunque se haya creado como unible.
!
!     No hay una rutina inversa.

! Recomendaciones:

!   Si un hilo requiere unirse, considere crearlo explícitamente como unible.
!   Esto proporciona portabilidad, ya que no todas las implementaciones pueden crear
!   subprocesos que se puedan unir de forma predeterminada.
!
!   Si sabe de antemano que un hilo nunca necesitará unirse con otro hilo,
!   considere crearlo en un estado separado. Es posible que se puedan liberar algunos recursos del sistema.

! This example demonstrates how to "wait" for thread completions by using
! the Pthread join routine.  Threads are explicitly created in a joinable
! state for portability reasons. Use of the pthread_exit status argument is
! also shown. Compare to detached example
!

module globals
    implicit none

    integer, parameter :: NUM_THREADS = 4

end module

module test
    use, intrinsic :: iso_c_binding
    use :: pthread
    use :: globals
    implicit none
    public :: BusyWork

contains
    subroutine BusyWork(t)
        integer, intent(in), target :: t
        integer :: i
        integer, target :: tid
        type(c_ptr) :: ptr_t
        integer :: resul=0

        tid = t
        ptr_t = c_loc(t)
        print *, "Thread ",tid, " starting..."

        do i = 0, 1000000
          resul = resul + i
        end do
        print *, "Thread ", tid, " done. Result = ", resul
        call pthread_exit(ptr_t)
    end subroutine BusyWork
end module test

program main
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use :: pthread
    use :: globals
    use :: test

    integer             :: rc
    integer, target     :: estatus
    integer             :: t
    integer, target     :: detach = PTHREAD_CREATE_JOINABLE
    type(pthread_t), target         :: threads(NUM_THREADS)
    integer, target                 :: routines(NUM_THREADS) = [ (t, t = 1, NUM_THREADS) ]
    type(pthread_attr_t), target    :: attr
    type(c_ptr)         :: ptr_attr
    type(c_ptr)         :: ptr_status

    ptr_status = c_loc(estatus)
    ptr_attr   = c_loc(attr)

    ! Initialize and set thread detached attribute
    rc = pthread_attr_init(ptr_attr)    ! 0=OK, else error
    ! options (see the module pthread):
    ! PTHREAD_CREATE_DETACHED = Z'04'
    ! PTHREAD_CREATE_JOINABLE = 0
    rc = pthread_attr_setdetachstate(ptr_attr, detach)
    !rc = pthread_attr_getdetachstate(ptr_attr, c_loc(detach)) ! 0 = OK

    do t = 1, NUM_THREADS
        print *, "Main: creating thread :", t
        rc = pthread_create(c_loc(threads(t)), c_loc(attr), c_funloc(BusyWork), c_loc(routines(t)))
        if (rc /= 0) then
            print *, "ERROR; return code from pthread_create() is ", rc
            exit
        end if

        !rc = pthread_detach(threads(t)) ! 0 = OK
        !print *, "linea 126 ", rc
    end do

    ! Free attribute and wait for the other threads */
    rc = pthread_attr_destroy(c_loc(attr))  ! is better ptr_attr?
    do t = 1, NUM_THREADS
        ptr_status = c_loc(routines(t))
        rc = pthread_join(threads(t), ptr_status)
        if (rc /= 0) then
            print *, "ERROR; return code from pthread_join() is ", rc
            exit
        end if
        print *, "Main: completed join with thread ", t, "having a status of ", ptr_status
    end do

    print *, "Main: program completed. Exiting."
    !call pthread_exit(c_null_ptr)
    call sleep(3)
end program main
