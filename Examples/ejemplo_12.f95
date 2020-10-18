! ejemplo12.f95
!
! VARIABLES DE CONDICION - Visi�n de conjunto
!
! Las variables de condici�n proporcionan otra forma de sincronizar los subprocesos.
! Mientras que las exclusiones mutuas implementan la sincronizaci�n controlando el acceso
! de los subprocesos a los datos, las variables de condici�n permiten que los subprocesos
! se sincronicen en funci�n del valor real de los datos.
!
! Sin variables de condici�n, el programador necesitar�a tener subprocesos continuamente sondeando
! (posiblemente en una secci�n cr�tica), para verificar si se cumple la condici�n.
! Esto puede consumir muchos recursos ya que el hilo estar�a continuamente ocupado en esta actividad.
! Una variable de condici�n es una forma de lograr el mismo objetivo sin sondeo.
!
! Una variable de condici�n siempre se usa junto con un bloqueo mutex.
!
! A continuaci�n se muestra una secuencia representativa para el uso de variables de condici�n.

! Hilo principal
!
!     Declarar e inicializar datos / variables globales que requieren sincronizaci�n (como "recuento")
!     Declarar e inicializar un objeto de variable de condici�n
!     Declarar e inicializar un mutex asociado
!     Crea hilos A y B para hacer el trabajo

! Hilo A
!
!     -Trabaja hasta el punto en el que deba ocurrir una determinada condici�n
!       (como un "recuento" debe alcanzar un valor espec�fico)
!     -Bloquea el mutex asociado y verifica el valor de una variable global
!     -Llama a pthread_cond_wait () para realizar una espera de bloqueo para la se�al de Thread-B.
!       Tener en cuenta que una llamada a pthread_cond_wait () desbloquea autom�tica y at�micamente
!       la variable mutex asociada para que pueda ser utilizada por Hilo-B.
!     -Cuando se le indique, despierta. Mutex se bloquea autom�tica y at�micamente.
!     -Desbloquear expl�citamente el mutex
!     -Continuar

! Hilo B
!
!    - Hace su trabajo
!    - Bloquear mutex asociado
!    - Cambia el valor de la variable global que espera Hilo-A.
!    - Comprueba el valor de la variable de espera global Hilo-A.
!       Si cumple la condici�n deseada, se�alizar  a Hilo-A.
!    - Desbloquear mutex.
!    - Continuar

! Hilo principal
!
!    - join/Continua


! Crear y destruir variables de condicion
!
!   Subroutines/functions:
!
!    pthread_cond_init (condition,attr)
!    pthread_cond_destroy (condition)
!    pthread_condattr_init (attr)
!    pthread_condattr_destroy (attr)
!
! Uso:
!
!    Las variables de condici�n deben declararse con el tipo pthread_cond_t y deben inicializarse
!    antes de que puedan usarse. Hay dos formas de inicializar una variable de condici�n:
!
!        Est�ticamente,
!        cuando se declara. Por ejemplo:
!        pthread_cond_t myconvar = PTHREAD_COND_INITIALIZER;
!
!        Din�micamente,
!        con la rutina pthread_cond_init ().
!        El ID de la variable de condici�n creada se devuelve al hilo de llamada a
!        trav�s del par�metro de condici�n. Este m�todo permite establecer atributos
!        de objeto de variable de condici�n, attr.
!
!    El objeto attr opcional se utiliza para establecer atributos de variables de condici�n.
!    Solo hay un atributo definido para las variables de condici�n: "process-shared",
!    que permite que los subprocesos de otros procesos vean la variable de condici�n. El objeto de atributo, si se usa, debe ser de tipo pthread_condattr_t (puede especificarse como NULL para aceptar valores predeterminados).
!
!    Tenga en cuenta que no todas las implementaciones pueden proporcionar el atributo de proceso compartido.
!
!    Las rutinas pthread_condattr_init () y pthread_condattr_destroy () se utilizan para crear
!    y destruir objetos de atributo de variable de condici�n.
!
!    pthread_cond_destroy () debe usarse para liberar una variable de condici�n que ya no es necesaria.


! Espera y se�alizaci�n en variables de condici�n
! Rutinas:
!
!    pthread_cond_wait (condici�n, mutex)
!    pthread_cond_signal (condici�n)
!    pthread_cond_broadcast (condici�n)
!
! Uso:
!
!    pthread_cond_wait ()
!    bloquea el hilo de llamada hasta que se se�aliza la condici�n especificada.
!    Esta rutina se debe llamar mientras el mutex est� bloqueado, y lo liberar�
!    autom�ticamente mientras espera. Una vez que se recibe la se�al y se despierta el hilo,
!    el mutex se bloquear� autom�ticamente para que lo utilice el hilo. El programador es
!    responsable de desbloquear el mutex cuando el hilo finaliza con �l.

!
!    Recomendaci�n: Usar un bucle WHILE en lugar de una instrucci�n IF
!    (ver la rutina watch_count en el ejemplo a continuaci�n) para verificar
!    la condici�n esperada puede ayudar a lidiar con varios problemas potenciales, tales como:
!        Si varios subprocesos est�n esperando la misma se�al de activaci�n,
!            se turnar�n para adquirir el mutex y cualquiera de ellos podr� modificar
!            la condici�n que todos esperaban.
!        Si el hilo recibi� la se�al por error debido a un error del programa
!        La biblioteca Pthreads puede emitir reactivaciones falsas a un hilo en espera sin violar el est�ndar.
!
!    La rutina pthread_cond_signal ()
!    se usa para se�alar (o despertar) otro hilo que est� esperando la variable de condici�n.
!    Se debe llamar despu�s de que el mutex est� bloqueado y debe desbloquearlo para
!    que se complete la rutina pthread_cond_wait ().
!
!    La rutina pthread_cond_broadcast ()
!    debe usarse en lugar de pthread_cond_signal () si m�s de un hilo est� en un estado de espera de bloqueo.
!
!    Es un error l�gico llamar a pthread_cond_signal () antes de llamar a pthread_cond_wait ().
!
! ADVERTENCIA:
!   El bloqueo y desbloqueo adecuados de la variable mutex asociada es esencial cuando se utilizan estas rutinas. Por ejemplo:
!
!    No bloquear el mutex antes de llamar a pthread_cond_wait () puede causar que NO se bloquee.
!    No desbloquear el mutex despu�s de llamar a pthread_cond_signal () puede no permitir que
!        se complete una rutina pthread_cond_wait () coincidente (permanecer� bloqueada).


! This Example is about using Condition Variables
!
!    This simple example code demonstrates the use of several Pthread condition variable routines.
!    The main routine creates three threads.
!    Two of the threads perform work and update a "count" variable.
!    The third thread waits until the count variable reaches a specified value.

module globals
    use, intrinsic :: iso_c_binding
    use :: pthread
    implicit none

    integer, parameter :: NUM_THREADS = 3
    integer, parameter :: TCOUNT      = 10
    integer, parameter :: COUNT_LIMIT = 12

    integer ::     count_ = 0
    integer ::     thread_ids(3) = [0,1,2]
    type(pthread_mutex_t), target :: count_mutex
    !integer(kind=C_INT_LEAST128_T), target ::count_mutex
    type(pthread_cond_t), target  :: count_threshold_cv
end module globals


module util
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use            :: pthread
    use            :: globals
    implicit none

contains
    subroutine watch_count(t)
        integer(int64), intent(in) :: t

        integer(int64) :: my_id
        integer        :: rc
        my_id = t
        print *, "Starting watch_count(): thread ", my_id
        !
        ! Lock mutex and wait for signal.  Note that the pthread_cond_wait
        ! routine will automatically and atomically unlock mutex while it waits.
        ! Also, note that if COUNT_LIMIT is reached before this routine is run by
        ! the waiting thread, the loop will be skipped to prevent pthread_cond_wait
        ! from never returning.
        !
        rc = pthread_mutex_lock(c_loc(count_mutex))
        do while (count_ < COUNT_LIMIT)
            rc = pthread_cond_wait(c_loc(count_threshold_cv), c_loc(count_mutex))
            print *, "watch_count(): thread ", my_id, " Condition signal received."
        end do
        count_ = count_ + 125
        print *, "watch_count(): thread ", my_id, " count now = ", count_
        rc = pthread_mutex_unlock(c_loc(count_mutex))

        call pthread_exit(c_null_ptr)
    end subroutine

    subroutine inc_count(t)
        integer, intent(in) :: t

        integer(int64) :: my_id
        integer        :: rc, i
        my_id = t

        do i = 0, TCOUNT-1 !for (i=0; i<TCOUNT; i++) {
            rc = pthread_mutex_lock(c_loc(count_mutex))
            count_ = count_ + 1

            !
            !Check the value of count and signal waiting thread when condition is
            !reached.  Note that this occurs while mutex is locked.
            !
            if (count_ == COUNT_LIMIT) then
                rc = pthread_cond_signal(c_loc(count_threshold_cv))
                print *, "inc_count(): thread ", my_id, " count = ", count_, ", Threshold reached."
            end if
            print *, "inc_count(): thread ", my_id, " count = ", count_, ", unlocking mutex"
            rc = pthread_mutex_unlock(c_loc(count_mutex))

            ! Do some "work" so threads can alternate on mutex lock
            call sleep(1);
        end do
        call pthread_exit(c_null_ptr)
    end subroutine
end module util


program main
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use            :: pthread
    use            :: globals
    use            :: util
    implicit none

    integer                      :: i, rc
    integer(int64),       target :: t1=1, t2=2, t3=3
    type(pthread_t),      target :: threads(3)
    type(pthread_attr_t), target :: attr

    ! Initialize mutex and condition variable objects
    rc = pthread_mutex_init(c_loc(count_mutex), c_null_ptr)
    rc = pthread_cond_init (c_loc(count_threshold_cv), c_null_ptr)

    ! For portability, explicitly create threads in a joinable state
    rc = pthread_attr_init(c_loc(attr))
    rc = pthread_attr_setdetachstate(c_loc(attr), PTHREAD_CREATE_JOINABLE)
    rc = pthread_create(c_loc(threads(1)), c_loc(attr), c_funloc(watch_count), c_loc(t1))
    rc = pthread_create(c_loc(threads(2)), c_loc(attr), c_funloc(inc_count), c_loc(t2))
    rc = pthread_create(c_loc(threads(3)), c_loc(attr), c_funloc(inc_count), c_loc(t3))

    ! Wait for all threads to complete
    do i = 1, NUM_THREADS
        rc = pthread_join(threads(i), c_null_ptr)
    end do

    print *, "Main(): Waited on ", NUM_THREADS," threads. Done."

    ! Clean up and exit
    rc = pthread_attr_destroy(c_loc(attr))
    rc = pthread_mutex_destroy(c_loc(count_mutex))
    rc = pthread_cond_destroy(c_loc(count_threshold_cv))
    !call pthread_exit(c_null_ptr)
    call sleep(2)
end program main
