package fc

import (
	"fmt"
	"runtime"
	"sync"
)

type Job func()

type WorkerPool struct {
	wg          sync.WaitGroup
	jobChan     chan Job
	jobChanSize int
	concurrent  int
}

func (pool *WorkerPool) AddWorker(n int) {
	pool.concurrent += n
	for i := 0; i < n; i++ {
		go worker(pool)
	}
}

func NewWorkerPool(num int, jobChanSize int) *WorkerPool {
	if num == 0 {
		num = runtime.NumCPU() * 2
	}

	if jobChanSize == 0 {
		jobChanSize = 4096
	}

	pool := &WorkerPool{
		jobChan:     make(chan Job, jobChanSize),
		jobChanSize: jobChanSize,
	}

	pool.AddWorker(num)

	return pool
}

func (pool *WorkerPool) WaitPool() {
	pool.wg.Wait()

	for i := 0; i < pool.concurrent; i++ {
		pool.AddJob(nil)
	}

	pool.wg.Wait()
	close(pool.jobChan)
}

func (pool *WorkerPool) IsBusy() bool {
	return pool.jobChanSize-pool.concurrent < len(pool.jobChan)
}

func (pool *WorkerPool) AddJob(job Job) {
	if pool.IsBusy() {
		pool.AddWorker(1)
	}

	pool.wg.Add(1)
	pool.jobChan <- job
}

func (pool *WorkerPool) Info() string {
	return fmt.Sprintf("WorkPool: concurrent = %d", pool.concurrent)
}

func worker(pool *WorkerPool) {
	for job := range pool.jobChan {
		if job == nil {
			pool.wg.Done()

			return
		}

		job()
		pool.wg.Done()
	}
}
