! Ejemplo9.f95
! Rutinas varias

!    pthread_self ()
!    pthread_equal (thread1, thread2)

! - pthread_self devuelve el ID único de hilo asignado por el sistema del hilo que hace la llamada.

! - pthread_equal compara dos ID de hilo. Si los dos ID son diferentes, se devuelve 0; de lo contrario,
! se devuelve un valor distinto de cero.

! Tenga en cuenta que para ambas rutinas, los objetos identificadores de hilo son opacos y no
! se pueden inspeccionar fácilmente. Debido a que los ID de subprocesos son objetos opacos,
! el operador de equivalencia == no debe usarse para comparar dos ID de subprocesos entre sí,
! o para comparar un ID de subproceso único con otro valor.

!   pthread_once (once_control, init_routine)

! -pthread_once ejecuta init_routine exactamente una vez en un proceso.
! La primera llamada a esta rutina por cualquier subproceso en el proceso ejecuta la init_routine dada,
! sin parámetros. Cualquier llamada posterior no tendrá ningún efecto.
! La subrutina init_routine es típicamente una simple rutina de inicialización.
! El parámetro once_control es una estructura de control de sincronización que requiere inicialización
! antes de llamar a pthread_once. Por ejemplo:

!    pthread_once_t once_control = PTHREAD_ONCE_INIT;
!
! El tipo pthread_once_t es simplemente un numero de tipo LONG por lo que podríamos definirlo como
!
!    long once_control = PTHREAD_ONCE_INIT o
!    long var_name = PTHREAD_ONCE_INIT
!
! La definción en pthread.h es : typedef long pthread_once_t;


module util
    implicit none
    public :: ejemplo
contains
    subroutine ejemplo
        !call sleep(1)
        print *, "into example function"
    end subroutine ejemplo
end module util

program main
    use, intrinsic :: iso_c_binding
    use :: pthread
    use :: util
    integer, parameter :: NTHREADS = 5
    integer            :: i, rc

    type(pthread_t), target :: threads(NTHREADS)
    integer,         target :: routines(NTHREADS) = [ (i, i = 1, NTHREADS) ]

    !The first we are going to test pthread_once
    !RETURN VALUE
    !   Upon  successful  completion,  pthread_once() shall return zero; other-
    !   wise, an error number shall be returned to indicate the error.
    type(pthread_once_t), target :: pthread_once_var
    pthread_once_var%once = PTHREAD_ONCE_INIT
    rc = pthread_once(c_loc(pthread_once_var), c_funloc(ejemplo))
    print *, "62- first call to pthread_once result: ", rc

    do i = 1, NTHREADS
        if (i==2) then
            !call sleep(1)
            rc = pthread_once(c_loc(pthread_once_var), c_funloc(ejemplo))
            print *, "second call pthread_once"
            print *, "Sub-sequent  calls  of  pthread_once() with the same once_control shall not call the subroutine."
            cycle
        end if
        print *, i
        rc = pthread_create(c_loc(threads(i)), c_null_ptr, c_funloc(ejemplo), c_null_ptr)
    end do

    call sleep(3)
    ! Now see pthread_equal()
    print *, "function shall return a non-zero value if t1 and t2 are equal; otherwise, zero shall be returned.", &
            & pthread_equal(c_loc(threads(1)), c_loc(threads(1)))

    do i = 1, NTHREADS
        rc = pthread_join(threads(i), c_loc(routines(i)))
    end do

    call sleep(2)
    ! finally we see pthread_self()
    print *, "88- pthread_self : ", pthread_self()
end program main
