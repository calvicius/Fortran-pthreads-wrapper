! Ejemplo10.f95
!
! Variables MUTEX
! Visión de conjunto

! Mutex es una abreviatura de "exclusión mutua".
! Las variables de mutex son uno de los medios principales para implementar la sincronización
! de subprocesos y para proteger los datos compartidos cuando ocurren múltiples escrituras.

! Una variable mutex actúa como un "bloqueo" que protege el acceso a un recurso de datos compartido.
! El concepto básico de un mutex como se usa en Pthreads es que solo un hilo puede bloquear (o poseer)
! una variable mutex en un momento dado. Por lo tanto, incluso si varios subprocesos intentan bloquear
! un mutex, solo un subproceso tendrá éxito. Ningún otro subproceso puede poseer ese mutex hasta que
! el subproceso propietario desbloquee ese mutex. Los hilos deben "turnarse" para acceder a datos protegidos.

! Se pueden utilizar exclusiones mutuas para evitar condiciones de "carrera".

! Muy a menudo, la acción realizada por un hilo que posee un mutex es la actualización de variables
! globales. Esta es una forma segura de garantizar que cuando varios subprocesos actualicen la misma
! variable, el valor final sea el mismo que si solo un subproceso realiza la actualización.
! Las variables que se actualizan pertenecen a una "sección crítica".

! Una secuencia típica en el uso de un mutex es la siguiente:

!    Crear e inicializar una variable mutex
!    Varios subprocesos intentan bloquear el mutex
!    Solo uno tiene éxito y ese hilo posee el mutex
!    El hilo propietario realiza un conjunto de acciones
!    El propietario desbloquea el mutex
!    Otro hilo adquiere el mutex y repite el proceso
!    Finalmente se destruye el mutex

! Cuando varios subprocesos compiten por un mutex, los perdedores se bloquean en esa llamada;
! hay una llamada de desbloqueo disponible con "trylock" en lugar de la llamada "lock".

! El proteger los datos compartidos, es responsabilidad del programador asegurarse de que todos
! los subprocesos que necesitan usar un mutex lo hagan. Por ejemplo, si 4 subprocesos actualizan
! los mismos datos, pero solo uno usa un mutex, los datos aún pueden estar corruptos.

! Creación y destrucción de mutex

! Rutinas:
!    pthread_mutex_init (mutex, attr)
!    pthread_mutex_destroy (mutex)
!    pthread_mutexattr_init (attr)
!    pthread_mutexattr_destroy (attr)

! Uso:

! Las variables de mutex deben declararse con el tipo pthread_mutex_t y deben inicializarse antes
! de que puedan usarse. Hay dos formas de inicializar una variable mutex:

!    Estáticamente, cuando se declara. Por ejemplo:
!        pthread_mutex_t mymutex = PTHREAD_MUTEX_INITIALIZER;

!    Dinámicamente, con la rutina pthread_mutex_init ().
!        Este método permite establecer atributos de objeto mutex, attr.

! El mutex está inicialmente desbloqueado.

! El objeto attr se usa para establecer propiedades para el objeto mutex,
! y debe ser de tipo pthread_mutexattr_t si se usa (puede especificarse como NULL para aceptar valores predeterminados).
! El estándar Pthreads define tres atributos de mutex opcionales:

!        Protocol: especifica el protocolo utilizado para evitar inversiones de prioridad para un mutex.
!        Prioceiling: especifica el límite de prioridad de un mutex.
!        Process-shared: especifica el proceso compartido de un mutex.

! Hay que tener en cuenta que puede que no todas las implementaciones proporcionen
! los tres atributos de exclusión mutua opcionales.

! Las rutinas pthread_mutexattr_init () y pthread_mutexattr_destroy () se utilizan para crear y
! destruir objetos de atributo mutex respectivamente.

! pthread_mutex_destroy () debe usarse para liberar un objeto mutex que ya no es necesario.


! Bloqueo y desbloqueo de mutex
! Rutinas:

!    pthread_mutex_lock (mutex)
!    pthread_mutex_trylock (mutex)
!    pthread_mutex_unlock (mutex)

!Uso:

! La rutina pthread_mutex_lock ()
!    es usada por un hilo para adquirir un bloqueo en la variable mutex especificada.
!    Si el mutex ya está bloqueado por otro hilo, esta llamada bloqueará el hilo de llamada hasta que se desbloquee el mutex.

! pthread_mutex_trylock ()
!    intentará bloquear un mutex. Sin embargo, si el mutex ya está bloqueado,
!    la rutina volverá inmediatamente con un código de error "ocupado".
!    Esta rutina puede ser útil para prevenir condiciones de interbloqueo, como en una situación de inversión de prioridad.

! pthread_mutex_unlock ()
!    desbloqueará un mutex si lo llama el hilo propietario. Es necesario llamar a esta rutina después
!    de que un subproceso haya completado su uso de datos protegidos si otros subprocesos deben adquirir
!    el mutex para su trabajo con los datos protegidos. Se devolverá un error si:
!        Si el mutex ya estaba desbloqueado
!        Si el mutex es propiedad de otro hilo

! No hay nada "mágico" en los mutex ... de hecho, son similares a un "acuerdo de caballeros" entre
! los hilos participantes. Depende del escritor de código asegurarse de que todos los subprocesos
! necesarios bloqueen y desbloqueen las llamadas correctamente. El siguiente escenario demuestra un error lógico:

!    Hilo 1     Hilo 2      Hilo 3
!    Lock       Lock
!    A = 2      A = A + 1   A = A * B
!    Unlock     Unlock

! Cuando más de un subproceso está esperando un mutex bloqueado, ¿a qué subproceso se le otorgará
! el bloqueo primero después de que se libere?

! RESPUESTA:
! A menos que se utilice la programación de prioridad de subprocesos ( thread priority scheduling),
! la asignación se dejará al programa del sistema nativo y puede parecer más o menos aleatoria.

! Ejemplo: uso de mutexes

!     Este programa de ejemplo ilustra el uso de variables mutex en un programa de subprocesos que realiza un producto escalar.
!     Los datos principales están disponibles para todos los subprocesos a través de una estructura de acceso global.
!     Cada hilo trabaja en una parte diferente de los datos.
!     El subproceso principal espera que todos los subprocesos completen sus cálculos y luego imprime la suma resultante.


module globals
    use, intrinsic :: iso_c_binding
    use pthread
    implicit none
    ! Define globally accessible variables and a mutex */
    !
    integer, public, parameter :: NUMTHRDS = 4
    integer, public, parameter :: VECLEN   = 100
    !
    !The following structure contains the necessary information
    !to allow the function "dotprod" to access its input data and
    !place its output into the structure.
    !
    type, public :: DOTDATA
        real :: a(NUMTHRDS*VECLEN)
        real :: b(NUMTHRDS*VECLEN)
        real :: sumation
        integer :: veclen
    end type DOTDATA

    type(DOTDATA), public :: dotstr
    type(pthread_t), target :: callThd(NUMTHRDS)

    ! pthread_mutex_t mutexsum;
    ! in pthread.h is defined as : typedef intptr_t pthread_mutex_t;
    !integer(kind=c_int), target :: mutexsum     ! type(c_ptr) ? See module pthread.f95
    type(pthread_mutex_t), target :: mutexsum
end module globals


module mutex_ejemplo
    use, intrinsic :: iso_c_binding
    use globals
    use pthread
    implicit none


contains
    subroutine dotprod(arg)
        integer, intent(in) :: arg
        ! Define and use local variables for convenience
        integer :: i, start, end_, len_, rc
        integer :: offset_
        integer, target :: nulo
        real :: mysum
        real, allocatable :: x(:), y(:)
call sleep(1)
        allocate(x(size(dotstr%a)))
        allocate(y(size(dotstr%b)))

        nulo = 0
        offset_ = arg
        len_ = dotstr%veclen
        start = 1   !offset_*len_
        end_  = size(x) !start + len_
        x = dotstr%a
        y = dotstr%b

        ! Perform the dot product and assign result
        ! to the appropriate variable in the structure.
        mysum = 0.0
        do i = start, end_
            mysum = mysum + (x(i) * y(i))*offset_ !- 1 - offset_
        end do

        ! Lock a mutex prior to updating the value in the shared
        ! structure, and unlock it upon updating.
        ! RETURN VALUE
        !   If  successful,  the  pthread_mutex_lock()  and  pthread_mutex_unlock()
        !   functions  shall  return  zero;  otherwise,  an  error  number shall be
        !   returned to indicate the error.
        rc = pthread_mutex_lock (c_loc(mutexsum))
        dotstr%sumation = dotstr%sumation + mysum
        print *, "Thread ", offset_, &
                " did ", start, &
                " to ", end_, &
                " : mysum = ", mysum, &
                " global sum = ", dotstr%sumation
        rc = pthread_mutex_unlock (c_loc(mutexsum))

        deallocate (y)
        deallocate (x)
        call pthread_exit(c_loc(nulo))
    end subroutine
end module


! The main program creates threads which do all the work and then
! print out result upon completion. Before creating the threads,
! The input data is created. Since all threads update a shared structure, we
! need a mutex for mutual exclusion. The main thread needs to wait for
! all threads to complete, it waits for each one of the threads. We specify
! a thread attribute value that allow the main thread to join with the
! threads it creates. Note also that we free up handles  when they are
! no longer needed.
program mutexes1
    use pthread
    use globals
    use mutex_ejemplo
    implicit none

    integer :: rc
    integer :: i
    real, allocatable :: a(:), b(:)
    type(c_ptr) :: estatus
    type(pthread_attr_t), target :: attr

    integer,         target :: routines(NUMTHRDS) = [ (i, i = 1, NUMTHRDS) ]

    allocate(a(NUMTHRDS*VECLEN))
    allocate(b(NUMTHRDS*VECLEN))
    a = 1
    b = 1

    dotstr%veclen = VECLEN
    dotstr%a = a
    dotstr%b = b
    dotstr%sumation=0



    ! RETURN VALUE
    !   Upon    successful    completion,    pthread_mutexattr_destroy()    and
    !   pthread_mutexattr_init() shall return zero; otherwise, an error  number
    !   shall be returned to indicate the error.
    rc = pthread_mutex_init(c_loc(mutexsum), c_null_ptr)

    ! Create threads to perform the dotproduct
    rc = pthread_attr_init(c_loc(attr))
    rc = pthread_attr_setdetachstate(c_loc(attr), PTHREAD_CREATE_JOINABLE)

    do i = 1, NUMTHRDS
        ! Each thread works on a different set of data.
        ! The offset is specified by 'i'. The size of
        ! the data for each thread is indicated by VECLEN.
        rc = pthread_create(c_loc(callThd(i)), c_loc(attr), c_funloc(dotprod), c_loc(routines(i)))
    end do

    rc = pthread_attr_destroy(c_loc(attr))

    ! Wait on the other threads
    do i = 1, NUMTHRDS
        rc = pthread_join(callThd(i), c_loc(routines(i)))
    end do

    !After joining, print out the results and cleanup */

    print *, "Sum =  ", dotstr%sumation
    deallocate (a)
    deallocate (b)
    rc = pthread_mutex_destroy(c_loc(mutexsum))

    call sleep(1)
end program mutexes1
