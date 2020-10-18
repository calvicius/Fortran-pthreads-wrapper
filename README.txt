Fortran pthreads wrapper

A fortran library that wraps c-pthreads in MinGW-Windows_OS with iso_c_binding

GNU Fortran (x86_64-posix-seh-rev0, Built by MinGW-W64) 8.1.0 (2003)
Code::Blocks version 20.03

pthread_atfork						No fork() in windows - so ignore this
pthread_attr_destroy				Implemented
pthread_attr_getdetachstate			Implemented
pthread_attr_getguardsize			Unsupported
pthread_attr_getinheritsched		Implemented
pthread_attr_getschedparam			Implemented
pthread_attr_getschedpolicy			Implemented
pthread_attr_getscope				Implemented
pthread_attr_getstack				Unsupported
pthread_attr_getstackaddr			Implemented
pthread_attr_getstacksize			Implemented
pthread_attr_init					Implemented
pthread_attr_setdetachstate			Implemented
pthread_attr_setguardsize			Unsupported
pthread_attr_setinheritsched		Implemented
pthread_attr_setschedparam			Implemented
pthread_attr_setschedpolicy			Implemented
pthread_attr_setscope				Implemented
pthread_attr_setstack				Unsupported
pthread_attr_setstackaddr			Implemented
pthread_attr_setstacksize			Implemented
pthread_barrier_destroy				Implemented
pthread_barrier_init				Implemented
pthread_barrier_wait				Implemented
pthread_barrierattr_destroy			Implemented
pthread_barrierattr_getpshared		Implemented
pthread_barrierattr_init			Implemented
pthread_barrierattr_setpshared		Implemented
pthread_cancel						Implemented
pthread_cleanup_pop					it is a macro in mingw
pthread_cleanup_push				ir is a macro in mingw
pthread_cond_broadcast				Implemented
pthread_cond_destroy				Implemented
pthread_cond_init					Implemented
pthread_cond_signal					Implemented
pthread_cond_timedwait				Implemented
pthread_cond_wait					Implemented
pthread_condattr_destroy			Implemented
pthread_condattr_getclock			Implemented
pthread_condattr_getpshared			Implemented
pthread_condattr_init				Implemented
pthread_condattr_setclock			Implemented
pthread_condattr_setpshared			Implemented
pthread_create						Implemented
pthread_detach						Implemented
pthread_equal						Implemented
pthread_exit						Implemented
pthread_getconcurrency
pthread_getcpuclockid				Unsupported
pthread_getschedparam				Implemented
pthread_getspecific					Implemented
pthread_join						Implemented
pthread_key_create					Implemented
pthread_key_delete					Implemented
pthread_kill						Implemented
pthread_mutex_destroy				Implemented
pthread_mutex_getprioceiling		Unsupported
pthread_mutex_init					Implemented
pthread_mutex_lock					Implemented
pthread_mutex_setprioceiling		Unsupported
pthread_mutex_timedlock				Implemented
pthread_mutex_trylock				Implemented
pthread_mutex_unlock				Implemented
pthread_mutexattr_destroy			Implemented
pthread_mutexattr_getprioceiling	Implemented
pthread_mutexattr_getprotocol		Implemented
pthread_mutexattr_getpshared		Implemented
pthread_mutexattr_gettype			Implemented
pthread_mutexattr_init				Implemented
pthread_mutexattr_setprioceiling	Implemented
pthread_mutexattr_setprotocol		Implemented
pthread_mutexattr_setpshared		Implemented
pthread_mutexattr_settype			Implemented
pthread_once						Implemented
pthread_rwlock_destroy				Implemented
pthread_rwlock_init					Implemented
pthread_rwlock_rdlock				Implemented
pthread_rwlock_timedrdlock
pthread_rwlock_timedwrlock
pthread_rwlock_tryrdlock			Implemented
pthread_rwlock_trywrlock			Implemented
pthread_rwlock_unlock				Implemented
pthread_rwlock_wrlock				Implemented
pthread_rwlockattr_destroy			Implemented
pthread_rwlockattr_getpshared		Implemented
pthread_rwlockattr_init				Implemented
pthread_rwlockattr_setpshared		Implemented
pthread_self						Implemented
pthread_setcancelstate				Implemented
pthread_setcanceltype				Implemented
pthread_setconcurrency
pthread_setschedparam				Implemented
pthread_setschedprio				doesn' exist in mingw?
pthread_setspecific					Implemented
pthread_sigmask						#define pthread_sigmask(H, S1, S2) 0
									pthread_signal.h
pthread_spin_destroy				Implemented
pthread_spin_init					Implemented
pthread_spin_lock					Implemented
pthread_spin_trylock				Implemented
pthread_spin_unlock					Implemented
pthread_testcancel 