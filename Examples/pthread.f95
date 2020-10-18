! pthread.f95
!
! Fortran 2003 ISO C binding interfaces to POSIX Threads.
!
!
!
module pthread
    use, intrinsic :: iso_c_binding
    implicit none


    ! some previous to change things like MAX_INT NUMBER
    integer(kind=c_int) :: INT_MAX

    ! pthread specific defines.

    integer(kind=c_int), parameter :: PTHREAD_CANCEL_DISABLE = 0
    integer(kind=c_int), parameter :: PTHREAD_CANCEL_ENABLE  = Z'01'

    integer(kind=c_int), parameter :: PTHREAD_CANCEL_DEFERRED     = 0
    integer(kind=c_int), parameter :: PTHREAD_CANCEL_ASYNCHRONOUS = Z'02'

    integer(kind=c_int), parameter :: PTHREAD_CREATE_JOINABLE = 0
    integer(kind=c_int), parameter :: PTHREAD_CREATE_DETACHED = Z'04'

    integer(kind=c_int), parameter :: PTHREAD_EXPLICIT_SCHED = 0
    integer(kind=c_int), parameter :: PTHREAD_INHERIT_SCHED  = Z'08'

    integer(kind=c_int), parameter :: PTHREAD_SCOPE_PROCESS  = 0
    integer(kind=c_int), parameter :: PTHREAD_SCOPE_SYSTEM   = Z'10'

    integer(kind=c_int), parameter :: PTHREAD_DEFAULT_ATTR = Z'01'  !(PTHREAD_CANCEL_ENABLE)

    !#define PTHREAD_CANCELED ((void *) (intptr_t) 0xDEADBEEF)
    !#define _PTHREAD_NULL_THREAD ((pthread_t) 0)

    integer(kind=c_int), parameter :: PTHREAD_ONCE_INIT = 0

    integer(kind=c_int), parameter :: PTHREAD_DESTRUCTOR_ITERATIONS = 256
    integer(kind=c_int), parameter :: PTHREAD_KEYS_MAX = shiftl(1,20)   !(1<<20)

    integer(kind=c_int), parameter :: PTHREAD_MUTEX_NORMAL     = 0
    integer(kind=c_int), parameter :: PTHREAD_MUTEX_ERRORCHECK = 1
    integer(kind=c_int), parameter :: PTHREAD_MUTEX_RECURSIVE  = 2
    integer(kind=c_int), parameter :: PTHREAD_MUTEX_DEFAULT    = 0  !PTHREAD_MUTEX_NORMAL

    integer(kind=c_int), parameter :: PTHREAD_MUTEX_SHARED  = 1
    integer(kind=c_int), parameter :: PTHREAD_MUTEX_PRIVATE = 0

    integer(kind=c_int), parameter :: PTHREAD_PRIO_NONE       = 0
    integer(kind=c_int), parameter :: PTHREAD_PRIO_INHERIT    = 8
    integer(kind=c_int), parameter :: PTHREAD_PRIO_PROTECT    = 16
    integer(kind=c_int), parameter :: PTHREAD_PRIO_MULT       = 32
    integer(kind=c_int), parameter :: PTHREAD_PROCESS_SHARED  = 1
    integer(kind=c_int), parameter :: PTHREAD_PROCESS_PRIVATE = 0

    integer(kind=c_int), parameter :: PTHREAD_MUTEX_FAST_NP       = 0   !PTHREAD_MUTEX_NORMAL
    integer(kind=c_int), parameter :: PTHREAD_MUTEX_TIMED_NP      = 0   !PTHREAD_MUTEX_FAST_NP
    integer(kind=c_int), parameter :: PTHREAD_MUTEX_ADAPTIVE_NP   = 0   !PTHREAD_MUTEX_FAST_NP
    integer(kind=c_int), parameter :: PTHREAD_MUTEX_ERRORCHECK_NP = 1   !PTHREAD_MUTEX_ERRORCHECK
    integer(kind=c_int), parameter :: PTHREAD_MUTEX_RECURSIVE_NP  = 2   !PTHREAD_MUTEX_RECURSIVE

    integer(kind=c_int), parameter :: PTHREAD_BARRIER_SERIAL_THREAD = 1

    integer(kind=c_int), parameter :: MAX_READ_LOCKS = huge(INT_MAX) - 1  !(INT_MAX - 1)

    ! Some POSIX realtime extensions, mostly stubbed */
    integer(kind=c_int), parameter :: SCHED_OTHER = 0
    integer(kind=c_int), parameter :: SCHED_FIFO  = 1
    integer(kind=c_int), parameter :: SCHED_RR    = 2
    integer(kind=c_int), parameter :: SCHED_MIN   = 0   !SCHED_OTHER
    integer(kind=c_int), parameter :: SCHED_MAX   = 2   !SCHED_RR

    integer(kind=c_int), parameter :: GENERIC_INITIALIZER            = -1
    integer(kind=c_int), parameter :: GENERIC_ERRORCHECK_INITIALIZER = -2
    integer(kind=c_int), parameter :: GENERIC_RECURSIVE_INITIALIZER  = -3
    integer(kind=c_int), parameter :: GENERIC_NORMAL_INITIALIZER     = -1

    ! must be defined after corresponding type

    !#define PTHREAD_MUTEX_INITIALIZER              (pthread_mutex_t)GENERIC_INITIALIZER            intptr_t
    !#define PTHREAD_RECURSIVE_MUTEX_INITIALIZER    (pthread_mutex_t)GENERIC_RECURSIVE_INITIALIZER  intptr_t
    !#define PTHREAD_ERRORCHECK_MUTEX_INITIALIZER   (pthread_mutex_t)GENERIC_ERRORCHECK_INITIALIZER intptr_t
    !#define PTHREAD_NORMAL_MUTEX_INITIALIZER       (pthread_mutex_t)GENERIC_NORMAL_INITIALIZER     intptr_t
    !#define PTHREAD_DEFAULT_MUTEX_INITIALIZER      PTHREAD_NORMAL_MUTEX_INITIALIZER
    !#define PTHREAD_COND_INITIALIZER               (pthread_cond_t)GENERIC_INITIALIZER             intptr_t
    !#define PTHREAD_RWLOCK_INITIALIZER             (pthread_rwlock_t)GENERIC_INITIALIZER           intptr_t
    !#define PTHREAD_SPINLOCK_INITIALIZER           (pthread_spinlock_t)GENERIC_INITIALIZER         intptr_t


    type, bind(c), public :: pthread_t
        !private
        integer(kind=c_intptr_t) :: hidden   !arbitrary var.name !(SIZE_OF_TYPE)
    end type pthread_t

    type, bind(c), public :: sched_param
        !private
        integer(kind=c_int) :: sched_priority;
    end type sched_param

    type, bind(c), public :: pthread_attr_t
        !private
        integer(kind=c_long) :: p_state  ! in C is unsigned int
        type(c_ptr) :: stack
        integer(kind=c_size_t) :: s_size
        type(sched_param) :: param
    end type pthread_attr_t


    !integer(kind=c_long), public, target :: pthread_once_t
    type, bind(c), public :: pthread_once_t
        !private
        integer(kind=c_long), public :: once
    end type pthread_once_t


    type, bind(c), public :: pthread_mutex_t
        ! must contains a pointer. intptr_t
        integer(kind=c_intptr_t) :: mutext
    end type

    ! typedef unsigned pthread_mutexattr_t;
    ! we take long in order to avoid negatives
    type, bind(c), public :: pthread_mutexattr_t
        integer(kind=c_long) :: mutexattrt
    end type

    type, bind(c), public :: pthread_cond_t
        integer(kind=c_intptr_t) :: cond
    end type pthread_cond_t

    type, bind(c), public :: pthread_condattr_t
        integer(kind=c_int) :: condattr
    end type

    type, bind(c), public :: pthread_barrier_t
        type(c_ptr) :: barriert
    end type

    type, bind(c), public :: pthread_barrierattr_t
        type(c_ptr) :: barrierattrt
    end type

    type, bind(c), public :: pthread_spinlock_t
        integer(kind=c_intptr_t) :: spinlockt
    end type

    type, bind(c), public :: pthread_rwlock_t
        integer(kind=c_intptr_t) :: rwlockt
    end type

    type, bind(c), public :: pthread_rwlockattr_t
        integer(kind=c_int) :: rwlockattrt
    end type

    ! typedef unsigned pthread_key_t
    ! we take long in order to avoid negatives
    type, bind(c), public :: pthread_key_t
        integer(kind=c_size_t) :: keyt
    end type

    type, bind(c), public :: clockid_t
        integer(kind=c_int) :: clockidt
    end type


    !#define PTHREAD_MUTEX_INITIALIZER              (pthread_mutex_t)GENERIC_INITIALIZER            intptr_t
    type(pthread_mutex_t) :: &
        PTHREAD_MUTEX_INITIALIZER = pthread_mutex_t(-1)    ! GENERIC_INITIALIZER = -1
    !#define PTHREAD_RECURSIVE_MUTEX_INITIALIZER    (pthread_mutex_t)GENERIC_RECURSIVE_INITIALIZER  intptr_t
    type(pthread_mutex_t) :: &
        PTHREAD_RECURSIVE_MUTEX_INITIALIZER = pthread_mutex_t(-3)   ! -3 = GENERIC_RECURSIVE_INITIALIZER
    !#define PTHREAD_ERRORCHECK_MUTEX_INITIALIZER   (pthread_mutex_t)GENERIC_ERRORCHECK_INITIALIZER intptr_t
    type(pthread_mutex_t) :: &
        PTHREAD_ERRORCHECK_MUTEX_INITIALIZER = pthread_mutex_t(-2)  ! -2 = GENERIC_ERRORCHECK_INITIALIZER
    !#define PTHREAD_NORMAL_MUTEX_INITIALIZER       (pthread_mutex_t)GENERIC_NORMAL_INITIALIZER     intptr_t
    type(pthread_mutex_t) :: &
        PTHREAD_NORMAL_MUTEX_INITIALIZER = pthread_mutex_t(-1)  ! -1 = GENERIC_NORMAL_INITIALIZER
    !#define PTHREAD_DEFAULT_MUTEX_INITIALIZER      PTHREAD_NORMAL_MUTEX_INITIALIZER
    type(pthread_mutex_t) :: &
        PTHREAD_DEFAULT_MUTEX_INITIALIZER = pthread_mutex_t(-1) ! PTHREAD_NORMAL_MUTEX_INITIALIZER
    !#define PTHREAD_COND_INITIALIZER               (pthread_cond_t)GENERIC_INITIALIZER             intptr_t
    type(pthread_cond_t) :: &
        PTHREAD_COND_INITIALIZER = pthread_cond_t(-1)   ! -1 = GENERIC_INITIALIZER
    !#define PTHREAD_RWLOCK_INITIALIZER             (pthread_rwlock_t)GENERIC_INITIALIZER           intptr_t
    type(pthread_rwlock_t) :: &
        PTHREAD_RWLOCK_INITIALIZER = pthread_rwlock_t(-1)
    ! !#define PTHREAD_SPINLOCK_INITIALIZER           (pthread_spinlock_t)GENERIC_INITIALIZER         intptr_t
    type(pthread_spinlock_t) :: &
        PTHREAD_SPINLOCK_INITIALIZER = pthread_spinlock_t(-1)


    public :: pthread_create
    public :: pthread_join
    public :: pthread_exit
    public :: pthread_cancel
    public :: pthread_detach
    public :: pthread_self
    public :: pthread_equal
    public :: pthread_once
    public :: pthread_kill

    public :: pthread_attr_destroy
    public :: pthread_attr_init
    public :: pthread_attr_setdetachstate
    public :: pthread_attr_getdetachstate
    public :: pthread_attr_getstacksize
    public :: pthread_attr_setstacksize
    public :: pthread_attr_setstackaddr
    public :: pthread_attr_getstackaddr

    public :: pthread_mutex_init
    public :: pthread_mutex_lock
    public :: pthread_mutex_unlock
    public :: pthread_mutex_trylock
    public :: pthread_mutex_destroy
    public :: pthread_mutexattr_init
    public :: pthread_mutexattr_destroy
    public :: pthread_mutex_timedlock
    public :: pthread_mutexattr_getprioceiling
    public :: pthread_mutexattr_setprioceiling
    public :: pthread_mutexattr_getprotocol
    public :: pthread_mutexattr_setprotocol
    public :: pthread_mutexattr_getpshared
    public :: pthread_mutexattr_setpshared
    public :: pthread_mutexattr_gettype
    public :: pthread_mutexattr_settype

    public :: pthread_attr_getschedparam
    public :: pthread_attr_setschedparam
    public :: pthread_attr_getinheritsched
    public :: pthread_attr_setinheritsched
    public :: pthread_attr_setschedpolicy
    public :: pthread_attr_getschedpolicy
    public :: pthread_attr_setscope
    public :: pthread_attr_getscope

    public :: pthread_getschedparam
    public :: pthread_setschedparam
    public :: sched_get_priority_min
    public :: sched_get_priority_max

    public :: pthread_cond_init
    public :: pthread_cond_wait
    public :: pthread_cond_signal
    public :: pthread_cond_destroy
    public :: pthread_cond_broadcast
    public :: pthread_condattr_init
    public :: pthread_condattr_destroy
    public :: pthread_condattr_getpshared
    public :: pthread_condattr_setpshared
    public :: pthread_cond_timedwait
    public :: pthread_condattr_getclock
    public :: pthread_condattr_setclock

    public :: pthread_barrier_init
    public :: pthread_barrier_wait
    public :: pthread_barrier_destroy
    public :: pthread_barrierattr_init
    public :: pthread_barrierattr_getpshared
    public :: pthread_barrierattr_setpshared
    public :: pthread_barrierattr_destroy

    public :: pthread_spin_init
    public :: pthread_spin_destroy
    public :: pthread_spin_lock
    public :: pthread_spin_unlock
    public :: pthread_spin_trylock

    public :: pthread_rwlock_init
    public :: pthread_rwlock_rdlock
    public :: pthread_rwlock_unlock
    public :: pthread_rwlock_wrlock
    public :: pthread_rwlock_destroy
    public :: pthread_rwlockattr_init
    public :: pthread_rwlockattr_destroy
    public :: pthread_rwlock_trywrlock
    public :: pthread_rwlock_tryrdlock
    public :: pthread_rwlockattr_getpshared
    public :: pthread_rwlockattr_setpshared

    public :: pthread_key_create
    public :: pthread_key_delete
    public :: pthread_setspecific
    public :: pthread_getspecific
    public :: pthread_setcancelstate
    public :: pthread_setcanceltype
    public :: pthread_testcancel


    interface
        ! int WINPTHREAD_API pthread_create(pthread_t *th, const pthread_attr_t *attr, void *(* func)(void *), void *arg)
        function pthread_create(thread, attr, start_routine, arg) bind(c, name='pthread_create')
            import :: c_int, c_ptr, c_funptr, pthread_attr_t
            type(c_ptr),    intent(in), value       :: thread
            type(c_ptr),    intent(in), value       :: attr
            type(c_funptr), intent(in), value       :: start_routine
            type(c_ptr),    intent(in), value       :: arg
            integer(kind=c_int)                     :: pthread_create
        end function pthread_create

        ! int pthread_join(pthread_t, void **)
        function pthread_join( thread, value_ptr) bind(c, name='pthread_join')
            import :: c_int, c_ptr, pthread_t
            type(pthread_t), intent(in), value      :: thread
            type(c_ptr),     intent(in)             :: value_ptr
            integer(kind=c_int)                     :: pthread_join
        end function pthread_join

        ! void      WINPTHREAD_API pthread_exit(void *res);
        subroutine pthread_exit(estatus) bind(c, name='pthread_exit')
            import :: c_ptr
            type(c_ptr)                                 :: estatus
        end subroutine

        ! int WINPTHREAD_API pthread_cancel(pthread_t t);
        function pthread_cancel(thread) bind(c, name='pthread_cancel')
            import :: c_int, pthread_t
            type(pthread_t), intent(in), value          :: thread
            integer(kind=c_int)                         :: pthread_cancel
        end function

        !int       WINPTHREAD_API pthread_kill(pthread_t t, int sig)
        ! pthread_kill (only supports zero sig value, for thread validity checking)
        ! in pthread_signal.h --> #define pthread_sigmask(H, S1, S2) 0
        ! if sig is zero, error checking shall be performed but no signal shall actually be sent
        function pthread_kill(t, sig) bind(c, name='pthread_kill')
            import :: c_int, pthread_t
            type(pthread_t), intent(in), value          :: t
            integer(kind=c_int), intent(in), value      :: sig
            integer(kind=c_int)                         :: pthread_kill
        end function

        !int WINPTHREAD_API pthread_attr_init(pthread_attr_t *attr)
        function pthread_attr_init(attr) bind(c, name='pthread_attr_init')
            import :: c_int, c_ptr
            type(c_ptr),    intent(in), value           :: attr
            integer(kind=c_int)                         :: pthread_attr_init
        end function

        ! int WINPTHREAD_API pthread_attr_destroy(pthread_attr_t *attr)
        function pthread_attr_destroy(attr) bind(c, name='pthread_attr_destroy')
            import :: c_int, c_ptr
            type(c_ptr),    intent(in), value           :: attr
            integer(kind=c_int)                         :: pthread_attr_destroy
        end function

        ! int WINPTHREAD_API pthread_attr_setdetachstate(pthread_attr_t *a, int flag)
        function pthread_attr_setdetachstate(a, flag) bind(c, name='pthread_attr_setdetachstate')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value      :: a
            integer(kind=c_int), intent(in), value      :: flag
            integer(kind=c_int)                         :: pthread_attr_setdetachstate
        end function

        ! int WINPTHREAD_API pthread_attr_getdetachstate(const pthread_attr_t *a, int *flag)
        function pthread_attr_getdetachstate(a, flag) bind(c, name='pthread_attr_getdetachstate')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value              :: a
            type(c_ptr), intent(in), value              :: flag
            integer(kind=c_int)                         :: pthread_attr_getdetachstate
        end function

        ! int       WINPTHREAD_API pthread_detach(pthread_t t)
        function pthread_detach(thread) bind(c, name='pthread_detach')
            import :: c_int, pthread_t
            type(pthread_t), intent(in), value          :: thread
            integer(kind=c_int)                         :: pthread_detach
        end function

        ! int WINPTHREAD_API pthread_attr_getstacksize(const pthread_attr_t *attr, size_t *size)
        function pthread_attr_getstacksize(attr, stackSize) bind(c, name='pthread_attr_getstacksize')
            import :: c_ptr, c_size_t, c_int
            type(c_ptr), intent(in), value              :: attr
            type(c_ptr), intent(in), value              :: stackSize
            integer(kind=c_int)                         :: pthread_attr_getstacksize
        end function

        ! int WINPTHREAD_API pthread_attr_setstacksize(pthread_attr_t *attr, size_t size)
        function pthread_attr_setstacksize(attr, stackSize) bind(c, name='pthread_attr_setstacksize')
            import :: c_ptr, c_size_t, c_int
            type(c_ptr), intent(in), value              :: attr
            type(c_ptr), intent(in), value              :: stackSize
            integer(kind=c_int)                         :: pthread_attr_setstacksize
        end function

        ! int WINPTHREAD_API pthread_attr_setstackaddr(pthread_attr_t *attr, void *stack)
        function pthread_attr_setstackaddr(attr, stack) bind(c, name='pthread_attr_setstackaddr')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: attr
            type(c_ptr), intent(in), value              :: stack
            integer(kind=c_int)                         :: pthread_attr_setstackaddr
        end function

        ! int WINPTHREAD_API pthread_attr_getstackaddr(const pthread_attr_t *attr, void **stack)
        function pthread_attr_getstackaddr(attr, stackaddr) bind(c, name='pthread_attr_getstackaddr')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: attr
            type(c_ptr), intent(in)                     :: stackaddr    ! **ptr is a pointer passed by reference
            integer(kind=c_int)                         :: pthread_attr_getstackaddr
        end function

        ! pthread_t WINPTHREAD_API pthread_self(void)
        function pthread_self() bind(c, name='pthread_self')
            import :: c_ptr, pthread_t
            type(pthread_t)                             :: pthread_self
        end function

        ! int       WINPTHREAD_API pthread_equal(pthread_t t1, pthread_t t2)
        function pthread_equal(t1, t2) bind(c, name='pthread_equal')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: t1
            type(c_ptr), intent(in), value              :: t2
            integer(kind=c_int)                         :: pthread_equal
        end function

        ! int       WINPTHREAD_API pthread_once(pthread_once_t *o, void (*func)(void))
        function pthread_once(o, func) bind(c, name='pthread_once')
            import :: c_ptr, c_funptr, c_int
            type(c_ptr),    intent(in), value           :: o
            type(c_funptr), intent(in), value           :: func
            integer(kind=c_int)                         :: pthread_once
        end function

        ! ************ MUTEX routines *************

        ! int WINPTHREAD_API pthread_mutex_init(pthread_mutex_t *m, const pthread_mutexattr_t *a)
        function pthread_mutex_init(m, a) bind(c, name='pthread_mutex_init')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: m
            type(c_ptr), intent(in), value              :: a
            integer(kind=c_int)                         :: pthread_mutex_init
        end function

        ! int WINPTHREAD_API pthread_mutex_lock(pthread_mutex_t *m)
        function pthread_mutex_lock(m) bind(c, name='pthread_mutex_lock')
            import ::c_ptr, c_int
            type(c_ptr), intent(in), value              :: m
            integer(kind=c_int)                         :: pthread_mutex_lock
        end function

        ! int WINPTHREAD_API pthread_mutex_unlock(pthread_mutex_t *m)
        function pthread_mutex_unlock(m) bind(c, name='pthread_mutex_unlock')
            import ::c_ptr, c_int
            type(c_ptr), intent(in), value              :: m
            integer(kind=c_int)                         :: pthread_mutex_unlock
        end function

        ! int WINPTHREAD_API pthread_mutex_trylock(pthread_mutex_t *m)
        function pthread_mutex_trylock(m) bind(c, name='pthread_mutex_trylock')
            import ::c_ptr, c_int
            type(c_ptr), intent(in), value              :: m
            integer(kind=c_int)                         :: pthread_mutex_trylock
        end function

        ! int WINPTHREAD_API pthread_mutex_destroy(pthread_mutex_t *m)
        function pthread_mutex_destroy(m) bind(c, name='pthread_mutex_destroy')
            import ::c_ptr, c_int
            type(c_ptr), intent(in), value              :: m
            integer(kind=c_int)                         :: pthread_mutex_destroy
        end function

        ! int WINPTHREAD_API pthread_mutexattr_init(pthread_mutexattr_t *a)
        function pthread_mutexattr_init(m_attr) bind(c, name='pthread_mutexattr_init')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: m_attr
            integer(kind=c_int)                         :: pthread_mutexattr_init
        end function

        ! int WINPTHREAD_API pthread_mutexattr_destroy(pthread_mutexattr_t *a)
        function pthread_mutexattr_destroy(m_attr) bind(c, name='pthread_mutexattr_destroy')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: m_attr
            integer(kind=c_int)                         :: pthread_mutexattr_destroy
        end function

        ! int WINPTHREAD_API pthread_mutex_timedlock(pthread_mutex_t *m, const struct timespec *ts)
        function pthread_mutex_timedlock(m, ts) bind(c, name='pthread_mutex_timedlock')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: m
            type(c_ptr), intent(in), value              :: ts
            integer(kind=c_int)                         :: pthread_mutex_timedlock
        end function

        ! int WINPTHREAD_API pthread_mutexattr_getprioceiling(const pthread_mutexattr_t *a, int * prio)
        function pthread_mutexattr_getprioceiling(a, prio) bind(c, name='pthread_mutexattr_getprioceiling')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: a
            type(c_ptr), intent(in), value              :: prio
            integer(kind=c_int)                         :: pthread_mutexattr_getprioceiling
        end function

        ! int WINPTHREAD_API pthread_mutexattr_setprioceiling(pthread_mutexattr_t *a, int prio)
        function pthread_mutexattr_setprioceiling(a, prio) bind(c, name='pthread_mutexattr_setprioceiling')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: a
            type(c_ptr), intent(in), value              :: prio
            integer(kind=c_int)                         :: pthread_mutexattr_setprioceiling
        end function

        ! int WINPTHREAD_API pthread_mutexattr_getprotocol(const pthread_mutexattr_t *a, int *type)
        function pthread_mutexattr_getprotocol(a, tipe) bind(c, name='pthread_mutexattr_getprotocol')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: a
            type(c_ptr), intent(in), value              :: tipe
            integer(kind=c_int)                         :: pthread_mutexattr_getprotocol
        end function

        ! int WINPTHREAD_API pthread_mutexattr_setprotocol(pthread_mutexattr_t *a, int type)
        function pthread_mutexattr_setprotocol(a, tipe) bind(c, name='pthread_mutexattr_setprotocol')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: a
            integer(kind=c_int), intent(in), value      :: tipe
            integer(kind=c_int)                         :: pthread_mutexattr_setprotocol
        end function

        ! int WINPTHREAD_API pthread_mutexattr_getpshared(const pthread_mutexattr_t *a, int *type)
        function pthread_mutexattr_getpshared(a, tipe) bind(c, name='pthread_mutexattr_getpshared')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: a
            type(c_ptr), intent(in), value              :: tipe
            integer(kind=c_int)                         :: pthread_mutexattr_getpshared
        end function

        ! int WINPTHREAD_API pthread_mutexattr_setpshared(pthread_mutexattr_t * a, int type)
        function pthread_mutexattr_setpshared(a, tipe) bind (c, name='pthread_mutexattr_setpshared')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: a
            integer(kind=c_int), intent(in), value      :: tipe
            integer(kind=c_int)                         :: pthread_mutexattr_setpshared
        end function

        ! int WINPTHREAD_API pthread_mutexattr_gettype(const pthread_mutexattr_t *a, int *type)
        function pthread_mutexattr_gettype(a, tipe) bind(c, name='pthread_mutexattr_gettype')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: a
            type(c_ptr), intent(in), value              :: tipe
            integer(kind=c_int)                         :: pthread_mutexattr_gettype
        end function

        ! int WINPTHREAD_API pthread_mutexattr_settype(pthread_mutexattr_t *a, int type)
        function pthread_mutexattr_settype(a, tipe) bind(c, name='pthread_mutexattr_settype')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: a
            integer(kind=c_int), intent(in), value      :: tipe
            integer(kind=c_int)                         :: pthread_mutexattr_settype
        end function


        ! ************** PRIORITY routines ***************

        ! int WINPTHREAD_API pthread_attr_getschedparam(const pthread_attr_t *attr, struct sched_param *param)
        function pthread_attr_getschedparam(attr, param) bind(c, name='pthread_attr_getschedparam')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: attr
            type(c_ptr), intent(in), value              :: param
            integer(kind=c_int)                         :: pthread_attr_getschedparam
        end function

        ! int WINPTHREAD_API pthread_attr_setschedparam(pthread_attr_t *attr, const struct sched_param *param)
        function pthread_attr_setschedparam(attr, param) bind(c, name='pthread_attr_setschedparam')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: attr
            type(c_ptr), intent(in), value              :: param
            integer(kind=c_int)                         :: pthread_attr_setschedparam
        end function

        !int WINPTHREAD_API pthread_attr_getinheritsched(const pthread_attr_t *a, int *flag)
        function pthread_attr_getinheritsched(attr, flag) bind(c, name='pthread_attr_getinheritsched')
            import :: c_ptr, c_int
            type(c_ptr),  intent(in), value             :: attr
            type(c_ptr),  intent(in), value             :: flag
            integer(kind=c_int)                         :: pthread_attr_getinheritsched
        end function

        ! int WINPTHREAD_API pthread_attr_setinheritsched(pthread_attr_t *a, int flag)
        function pthread_attr_setinheritsched(attr, flag) bind(c, name='pthread_attr_setinheritsched')
            import :: c_ptr, c_int
            type(c_ptr),         intent(in), value      :: attr
            integer(kind=c_int), intent(in), value      :: flag
            integer(kind=c_int)                         :: pthread_attr_setinheritsched
        end function

        ! int WINPTHREAD_API pthread_getschedparam(pthread_t thread, int *pol, struct sched_param *param)
        function pthread_getschedparam(thread, pol, param) bind(c, name='pthread_getschedparam')
            import :: c_int, c_ptr, pthread_t
            type(pthread_t), intent(in), value          :: thread
            type(c_ptr),     intent(in), value          :: pol
            type(c_ptr),     intent(in), value          :: param
            integer(kind=c_int)                         :: pthread_getschedparam
        end function

        ! int WINPTHREAD_API pthread_setschedparam(pthread_t thread, int pol, const struct sched_param *param)
        function pthread_setschedparam(thread, pol, param) bind(c, name='pthread_setschedparam')
            import :: c_int, c_ptr, pthread_t
            type(pthread_t),     intent(in), value      :: thread
            integer(kind=c_int), intent(in), value      :: pol
            type(c_ptr),         intent(in), value      :: param
            integer(kind=c_int)                         :: pthread_setschedparam
        end function

        ! int WINPTHREAD_API pthread_attr_setscope(pthread_attr_t *a, int flag)
        function pthread_attr_setscope(a, flag) bind(c, name='pthread_attr_setscope')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value      :: a
            integer(kind=c_int), intent(in), value      :: flag
            integer(kind=c_int)                         :: pthread_attr_setscope
        end function

        ! int WINPTHREAD_API pthread_attr_getscope(const pthread_attr_t *a, int *flag)
        function pthread_attr_getscope(a, flag) bind(c, name='pthread_attr_getscope')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value      :: a
            type(c_ptr), intent(in), value              :: flag
            integer(kind=c_int)                         :: pthread_attr_getscope
        end function

        ! int WINPTHREAD_API pthread_attr_setschedpolicy (pthread_attr_t *attr, int pol)
        function pthread_attr_setschedpolicy(attr, pol) bind(c, name='pthread_attr_setschedpolicy')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: attr
            integer(kind=c_int),  intent(in), value     :: pol
            integer(kind=c_int)                         :: pthread_attr_setschedpolicy
        end function

        ! int WINPTHREAD_API pthread_attr_getschedpolicy (const pthread_attr_t *attr, int *pol)
        function pthread_attr_getschedpolicy(attr, pol) bind(c, name='pthread_attr_getschedpolicy')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: attr
            type(c_ptr),          intent(in), value     :: pol
            integer(kind=c_int)                         :: pthread_attr_getschedpolicy
        end function

        ! int WINPTHREAD_API sched_get_priority_min(int pol)
        function sched_get_priority_min(pol) bind(c, name='sched_get_priority_min')
            import :: c_int
            integer(kind=c_int),  intent(in), value     :: pol
            integer(kind=c_int)                         :: sched_get_priority_min
        end function

        ! int WINPTHREAD_API sched_get_priority_max(int pol)
        function sched_get_priority_max(pol) bind(c, name='sched_get_priority_max')
            import :: c_int
            integer(kind=c_int),  intent(in), value     :: pol
            integer(kind=c_int)                         :: sched_get_priority_max
        end function


        ! *************** CONDITION VARIABLES ***********

        ! int WINPTHREAD_API pthread_cond_init(pthread_cond_t *cv, const pthread_condattr_t *a)
        function pthread_cond_init(cv, a) bind(c, name='pthread_cond_init')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: cv
            type(c_ptr),          intent(in), value     :: a
            integer(kind=c_int)                         :: pthread_cond_init
        end function

        ! int WINPTHREAD_API pthread_cond_destroy(pthread_cond_t *cv)
        function pthread_cond_destroy(cv) bind (c, name='pthread_cond_destroy')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: cv
            integer(kind=c_int)                         :: pthread_cond_destroy
        end function

        ! int WINPTHREAD_API pthread_cond_wait (pthread_cond_t *cv, pthread_mutex_t *external_mutex)
        function pthread_cond_wait(cv, external_mutex) bind(c, name='pthread_cond_wait')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: cv
            type(c_ptr),          intent(in), value     :: external_mutex
            integer(kind=c_int)                         :: pthread_cond_wait
        end function

        ! int WINPTHREAD_API pthread_cond_signal (pthread_cond_t *cv)
        function pthread_cond_signal(cv) bind(c, name='pthread_cond_signal')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: cv
            integer(kind=c_int)                         :: pthread_cond_signal
        end function

        ! int WINPTHREAD_API pthread_cond_broadcast (pthread_cond_t *cv)
        function pthread_cond_broadcast(cv) bind(c, name='pthread_cond_broadcast')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: cv
            integer(kind=c_int)                         :: pthread_cond_broadcast
        end function

        ! int WINPTHREAD_API pthread_condattr_init(pthread_condattr_t *a)
        function pthread_condattr_init(a) bind(c, name='pthread_condattr_init')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: a
            integer(kind=c_int)                         :: pthread_condattr_init
        end function

        ! int WINPTHREAD_API pthread_condattr_destroy(pthread_condattr_t *a)
        function pthread_condattr_destroy(a) bind(c, name='pthread_condattr_destroy')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: a
            integer(kind=c_int)                         :: pthread_condattr_destroy
        end function

        ! int WINPTHREAD_API pthread_condattr_getpshared(const pthread_condattr_t *a, int *s)
        function pthread_condattr_getpshared(a, s) bind(c, name='pthread_condattr_getpshared')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: a
            type(c_ptr),          intent(in), value     :: s
            integer(kind=c_int)                         :: pthread_condattr_getpshared
        end function

        ! int WINPTHREAD_API pthread_condattr_setpshared(pthread_condattr_t *a, int s)
        function pthread_condattr_setpshared(a, s) bind(c, name='pthread_condattr_setpshared')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: a
            integer(kind=c_int),  intent(in), value     :: s
            integer(kind=c_int)                         :: pthread_condattr_setpshared
        end function

        ! int WINPTHREAD_API pthread_cond_timedwait(
        !   pthread_cond_t *cv, pthread_mutex_t *external_mutex, const struct timespec *t)
        function pthread_cond_timedwait(cv, external_mutex, t) bind(c, name='pthread_cond_timedwait')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: cv
            type(c_ptr), intent(in), value              :: external_mutex
            type(c_ptr), intent(in), value              :: t
            integer(kind=c_int)                         :: pthread_cond_timedwait
        end function

        ! int WINPTHREAD_API pthread_condattr_getclock (const pthread_condattr_t *attr,
        !         clockid_t *clock_id)
        function pthread_condattr_getclock(attr, clock_id) bind(c, name='pthread_condattr_getclock')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: attr
            type(c_ptr), intent(in), value              :: clock_id
            integer(kind=c_int)                         :: pthread_condattr_getclock
        end function

        ! int WINPTHREAD_API pthread_condattr_setclock(pthread_condattr_t *attr,
        !      clockid_t clock_id)
        function pthread_condattr_setclock(attr, clock_id) bind(c, name='pthread_condattr_setclock')
            import :: c_ptr, c_int, clockid_t
            type(c_ptr), intent(in), value              :: attr
            type(clockid_t), intent(in), value          :: clock_id
            integer(kind=c_int)                         :: pthread_condattr_setclock
        end function


        ! **************** BARRIER METHODS ***************

        ! int WINPTHREAD_API pthread_barrier_init(pthread_barrier_t *b, const void *attr, unsigned int count)
        function pthread_barrier_init(b, attr, count_) bind(c, name='pthread_barrier_init')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: b
            type(c_ptr),          intent(in), value     :: attr
            integer(kind=c_int),  intent(in), value     :: count_
            integer(kind=c_int)                         :: pthread_barrier_init
        end function

        ! int WINPTHREAD_API pthread_barrier_wait(pthread_barrier_t *b)
        function pthread_barrier_wait(b) bind(c, name='pthread_barrier_wait')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: b
            integer(kind=c_int)                         :: pthread_barrier_wait
        end function

        ! int WINPTHREAD_API pthread_barrier_destroy(pthread_barrier_t *b)
        function pthread_barrier_destroy(b) bind(c, name='pthread_barrier_destroy')
            import :: c_int, c_ptr
            type(c_ptr),          intent(in), value     :: b
            integer(kind=c_int)                         :: pthread_barrier_destroy
        end function

        ! int WINPTHREAD_API pthread_barrierattr_init(void **attr)
        function pthread_barrierattr_init(attr) bind(c, name='pthread_barrierattr_init')
            import :: c_ptr, c_int
            type(c_ptr), intent(in)                     :: attr    ! **ptr is a pointer passed by reference not value
            integer(kind=c_int)                         :: pthread_barrierattr_init
        end function

        ! int WINPTHREAD_API pthread_barrierattr_getpshared(void **attr, int *s)
        function pthread_barrierattr_getpshared(attr, s) bind(c, name='pthread_barrierattr_getpshared')
            import :: c_ptr, c_int
            type(c_ptr), intent(in)                     :: attr    ! **ptr is a pointer passed by reference not value
            type(c_ptr), intent(in), value              :: s
            integer(kind=c_int)                         :: pthread_barrierattr_getpshared
        end function

        ! int WINPTHREAD_API pthread_barrierattr_setpshared(void **attr, int s)
        function pthread_barrierattr_setpshared(attr, s) bind(c, name='pthread_barrierattr_setpshared')
            import :: c_ptr, c_int
            type(c_ptr), intent(in)                     :: attr    ! **ptr is a pointer passed by reference not value
            integer(kind=c_int), intent(in), value      :: s
            integer(kind=c_int)                         :: pthread_barrierattr_setpshared
        end function

        ! int WINPTHREAD_API pthread_barrierattr_destroy(void **attr)
        function pthread_barrierattr_destroy(attr) bind(c, name='pthread_barrierattr_destroy')
            import :: c_ptr, c_int
            type(c_ptr), intent(in)                     :: attr    ! **ptr is a pointer passed by reference not value
            integer(kind=c_int)                         :: pthread_barrierattr_destroy
        end function

        ! **************** SPIN METHODS *********************

        ! int WINPTHREAD_API pthread_spin_init(pthread_spinlock_t *l, int pshared)
        function pthread_spin_init(l, pshared) bind(c, name='pthread_spin_init')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: l
            integer(kind=c_int), intent(in), value      :: pshared
            integer(kind=c_int)                         :: pthread_spin_init
        end function

        ! int WINPTHREAD_API pthread_spin_destroy(pthread_spinlock_t *l)
        function pthread_spin_destroy(l) bind(c, name='pthread_spin_destroy')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: l
            integer(kind=c_int)                         :: pthread_spin_destroy
        end function

        ! int WINPTHREAD_API pthread_spin_lock(pthread_spinlock_t *l)
        function pthread_spin_lock(l) bind(c, name='pthread_spin_lock')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: l
            integer(kind=c_int)                         :: pthread_spin_lock
        end function

        ! int WINPTHREAD_API pthread_spin_unlock(pthread_spinlock_t *l)
        function pthread_spin_unlock(l) bind(c, name='pthread_spin_unlock')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: l
            integer(kind=c_int)                         :: pthread_spin_unlock
        end function

        ! int WINPTHREAD_API pthread_spin_trylock(pthread_spinlock_t *l)
        function pthread_spin_trylock(l) bind(c, name='pthread_spin_trylock')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: l
            integer(kind=c_int)                         :: pthread_spin_trylock
        end function

        ! ************** RWLOCK METHODS ****************

        ! int WINPTHREAD_API pthread_rwlock_init(pthread_rwlock_t *rwlock_, const pthread_rwlockattr_t *attr)
        function pthread_rwlock_init(rwlock, attr) bind(c, name='pthread_rwlock_init')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: rwlock
            type(c_ptr), intent(in), value              :: attr
            integer(kind=c_int)                         :: pthread_rwlock_init
        end function

        ! int WINPTHREAD_API pthread_rwlock_rdlock(pthread_rwlock_t *l)
        function pthread_rwlock_rdlock(l) bind(c, name='pthread_rwlock_rdlock')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: l
            integer(kind=c_int)                         :: pthread_rwlock_rdlock
        end function

        ! int WINPTHREAD_API pthread_rwlock_unlock(pthread_rwlock_t *l)
        function pthread_rwlock_unlock(l) bind(c, name='pthread_rwlock_unlock')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: l
            integer(kind=c_int)                         :: pthread_rwlock_unlock
        end function

        ! int WINPTHREAD_API pthread_rwlock_wrlock(pthread_rwlock_t *l)
        function pthread_rwlock_wrlock(l) bind(c, name='pthread_rwlock_wrlock')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: l
            integer(kind=c_int)                         :: pthread_rwlock_wrlock
        end function

        ! int WINPTHREAD_API pthread_rwlock_destroy (pthread_rwlock_t *l)
        function pthread_rwlock_destroy(l) bind(c, name='pthread_rwlock_destroy')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: l
            integer(kind=c_int)                         :: pthread_rwlock_destroy
        end function

        ! int WINPTHREAD_API pthread_rwlockattr_init(pthread_rwlockattr_t *a)
        function pthread_rwlockattr_init(a) bind(c, name='pthread_rwlockattr_init')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: a
            integer(kind=c_int)                         :: pthread_rwlockattr_init
        end function

        ! int WINPTHREAD_API pthread_rwlockattr_destroy(pthread_rwlockattr_t *a)
        function pthread_rwlockattr_destroy(a) bind(c, name='pthread_rwlockattr_destroy')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: a
            integer(kind=c_int)                         :: pthread_rwlockattr_destroy
        end function

        ! int WINPTHREAD_API pthread_rwlock_trywrlock(pthread_rwlock_t *l)
        function pthread_rwlock_trywrlock(l) bind(c, name='pthread_rwlock_trywrlock')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: l
            integer(kind=c_int)                         :: pthread_rwlock_trywrlock
        end function

        ! int WINPTHREAD_API pthread_rwlock_tryrdlock(pthread_rwlock_t *l)
        function pthread_rwlock_tryrdlock(l) bind(c, name='pthread_rwlock_tryrdlock')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: l
            integer(kind=c_int)                         :: pthread_rwlock_tryrdlock
        end function

        ! int WINPTHREAD_API pthread_rwlockattr_getpshared(pthread_rwlockattr_t *a, int *s)
        function pthread_rwlockattr_getpshared(a, s) bind(c, name='pthread_rwlockattr_getpshared')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: a
            type(c_ptr), intent(in), value              :: s
            integer(kind=c_int)                         :: pthread_rwlockattr_getpshared
        end function

        ! int WINPTHREAD_API pthread_rwlockattr_setpshared(pthread_rwlockattr_t *a, int s)
        function pthread_rwlockattr_setpshared(a, s) bind(c, name='pthread_rwlockattr_setpshared')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value              :: a
            integer(c_int), intent(in), value           :: s
            integer(kind=c_int)                         :: pthread_rwlockattr_setpshared
        end function


        ! *********************** KEY METHODS ****************

        ! int WINPTHREAD_API pthread_key_create(pthread_key_t *key, void (* dest)(void *))
        function pthread_key_create(key, dest) bind(c, name='pthread_key_create')
            import :: c_ptr, c_int, c_funptr, pthread_key_t
            type(c_ptr), intent(in), value              :: key
            type(c_funptr), intent(in), value           :: dest
            integer(kind=c_int)                         :: pthread_key_create
        end function

        ! int WINPTHREAD_API pthread_key_delete(pthread_key_t key)
        function pthread_key_delete(key) bind(c, name='pthread_key_delete')
            import :: c_int, pthread_key_t
            type(pthread_key_t), intent(in), value      :: key
            integer(kind=c_int)                         :: pthread_key_delete
        end function

        ! *******************+ OTHER METHODS *****************

        ! int WINPTHREAD_API pthread_setspecific(pthread_key_t key, const void *value)
        function pthread_setspecific(key, v) bind(c, name='pthread_setspecific')
            import :: c_int, c_ptr, pthread_key_t
            type(pthread_key_t), intent(in), value      :: key
            type(c_ptr), intent(in), value              :: v
            integer(kind=c_int)                         :: pthread_setspecific
        end function

        ! void * WINPTHREAD_API pthread_getspecific(pthread_key_t key)
        function pthread_getspecific(key) bind(c, name='pthread_getspecific')
            import :: pthread_key_t, c_ptr
            type(pthread_key_t), intent(in), value      :: key
            type(c_ptr)                                 :: pthread_getspecific
        end function

        ! int WINPTHREAD_API pthread_setcancelstate(int state, int *oldstate)
        function pthread_setcancelstate(state, oldstate) bind(c, name='pthread_setcancelstate')
            import :: c_int, c_ptr
            integer(kind=c_int), intent(in), value      :: state
            type(c_ptr), intent(in), value              :: oldstate
            integer(kind=c_int)                         :: pthread_setcancelstate
        end function

        ! int WINPTHREAD_API pthread_setcanceltype(int type, int *oldtype)
        function pthread_setcanceltype(tipe, oldtype) bind(c, name='pthread_setcanceltype')
            import :: c_int, c_ptr
            integer(kind=c_int), intent(in), value      :: tipe
            type(c_ptr), intent(in), value              :: oldtype
            integer(kind=c_int)                         :: pthread_setcanceltype
        end function

        ! void WINPTHREAD_API pthread_testcancel(void)
        subroutine pthread_testcancel() bind(c, name='pthread_testcancel')

        end subroutine

    end interface

end module pthread
