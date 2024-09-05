#ifndef __MUTEX_GUARD_H__
#define __MUTEX_GUARD_H__

#include <pthread.h>

class Mutex_Guard {

    public:
	Mutex_Guard(pthread_mutex_t* mutex) :  mutex_(mutex) { pthread_mutex_lock(mutex_); }
	~Mutex_Guard() {  pthread_mutex_unlock(mutex_); }

    private:
	pthread_mutex_t* mutex_;

};

#endif
