/*
 * Header file for the BFQ I/O scheduler: data structures and
 * prototypes of interface functions among BFQ components.
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 */
#ifndef _BFQ_H
#define _BFQ_H

#include <linux/blktrace_api.h>
#include <linux/hrtimer.h>
#include <linux/blk-cgroup.h>
//bfq调度类有 IOPRIO_CLASS_NONE、IOPRIO_CLASS_RT、IOPRIO_CLASS_BE、IOPRIO_CLASS_IDLE
#define BFQ_IOPRIO_CLASSES	3
#define BFQ_CL_IDLE_TIMEOUT	(HZ/5)

#define BFQ_MIN_WEIGHT			1
#define BFQ_MAX_WEIGHT			1000
#define BFQ_WEIGHT_CONVERSION_COEFF	10

#define BFQ_DEFAULT_QUEUE_IOPRIO	4

#define BFQ_WEIGHT_LEGACY_DFL	100
#define BFQ_DEFAULT_GRP_IOPRIO	0
#define BFQ_DEFAULT_GRP_CLASS	IOPRIO_CLASS_BE

#define MAX_PID_STR_LENGTH 12

/*
 * Soft real-time applications are extremely more latency sensitive
 * than interactive ones. Over-raise the weight of the former to
 * privilege them against the latter.
 */
#define BFQ_SOFTRT_WEIGHT_FACTOR	100

struct bfq_entity;

/**
 * struct bfq_service_tree - per ioprio_class service tree.
 *
 * Each service tree represents a B-WF2Q+ scheduler on its own.  Each
 * ioprio_class has its own independent scheduler, and so its own
 * bfq_service_tree.  All the fields are protected by the queue lock
 * of the containing bfqd.
 */
//bfq_sched_data、bfq_service_tree、bfq_entity 3者关系见bfq_entity_service_tree()
//一个bfq调度类对应一个bfq_service_tree结构，bfq调度类有IOPRIO_CLASS_NONE、IOPRIO_CLASS_RT、IOPRIO_CLASS_BE、IOPRIO_CLASS_IDLE
struct bfq_service_tree {
	/* tree for active entities (i.e., those backlogged) */
    
    /*1:active属性的bfqq都是插入这个active这个红黑树。并且bfq_sched_data的in_service_entity和next_in_service指向的entity都是
    active的entity。idle属性的bfqq是插入到idle红黑树
    */
	struct rb_root active;//bfq_active_insert()把entity插入active链表，bfq_active_extract()中把entity从active链表剔除
	/* tree for idle entities (i.e., not backlogged, with V < F_i)*/
	struct rb_root idle;//bfq_idle_insert()把entity插入idle链表，bfq_idle_extract()中把entity从idle链表剔除

	/* idle entity with minimum F_i */
    //st->idle红黑树的最左边的entiry的finish最小，最右边的entity的finish最大。st->first_idle记录entity->finish最小的entity，
    //就是st->idle红黑树最靠左的entity。st->last_idle记录entity->finish最大的entity，就是st->idle红黑树最靠右的entity。
    //bfq_forget_idle()中如果st->vtime大于first_idle->finish，则把first_idle指向的entity从st->idle tree剔除掉，如果不再被用到，释放掉bfqq和entity
	struct bfq_entity *first_idle;//st->idle tree中finish最小的entity。bfq_idle_extract()或bfq_idle_insert()中赋值
	/* idle entity with maximum F_i */
	struct bfq_entity *last_idle;//st->idle tree中finish最大的entity。//bfq_idle_extract()或bfq_idle_insert()中赋值

	/* scheduler virtual time */
    
    /*st->vtime是整个调度器的虚拟运行时间，st->vtime是累加bfq_bfqq_served()累计待派发req的配额served除以st->wsum。而struct bfq_entity *
    entity->finish是bfq_calc_finish()中根据派发req的配额除以entity->weight计算entity虚拟运行时间，然后entity->finish=entity->start+
    entity虚拟运行时间。st->vtime、entity->start、entity->finish单位一样，都是虚拟运行时间，一个是整个调度器的，一个是entity的*/
     
    //调度器st虚拟运行时间,与entity->start和entity->finish单位一样,bfq_bfqq_served()中累加待派发req的配额served除以st->wsum算出的
    //虚拟运行时间。
	u64 vtime;
	
	/* scheduler weight sum; active and idle entities contribute to it */
    //调度器st的active和tree上所有entity的权重之和，bfq_forget_entity()中从st剔除entity时，wsum减去该entity的权重entity->weight
    //__bfq_activate_entity()中把entiry加入调度器时，wsum再累加entity->weight
	unsigned long wsum;
};

/**
 * struct bfq_sched_data - multi-class scheduler.
 *
 * bfq_sched_data is the basic scheduler queue.  It supports three
 * ioprio_classes, and can be used either as a toplevel queue or as an
 * intermediate queue in a hierarchical setup.
 *
 * The supported ioprio_classes are the same as in CFQ, in descending
 * priority order, IOPRIO_CLASS_RT, IOPRIO_CLASS_BE, IOPRIO_CLASS_IDLE.
 * Requests from higher priority queues are served before all the
 * requests from lower priority queues; among requests of the same
 * queue requests are served according to B-WF2Q+.
 *
 * The schedule is implemented by the service trees, plus the field
 * @next_in_service, which points to the entity on the active trees
 * that will be served next, if 1) no changes in the schedule occurs
 * before the current in-service entity is expired, 2) the in-service
 * queue becomes idle when it expires, and 3) if the entity pointed by
 * in_service_entity is not a queue, then the in-service child entity
 * of the entity pointed by in_service_entity becomes idle on
 * expiration. This peculiar definition allows for the following
 * optimization, not yet exploited: while a given entity is still in
 * service, we already know which is the best candidate for next
 * service among the other active entitities in the same parent
 * entity. We can then quickly compare the timestamps of the
 * in-service entity with those of such best candidate.
 *
 * All fields are protected by the lock of the containing bfqd.
 */
//bfq_sched_data和bfq_service_tree其实差别不大，bfq_sched_data包含不同调度策略的bfq_service_tree
struct bfq_sched_data {//bfq_sched_data、bfq_service_tree、bfq_entity 3者关系见bfq_entity_service_tree()
	/* entity in service */
    //in_service_entity是当前sd正在使用的entity，bfq_get_next_queue()。__bfq_deactivate_entity()中设置NULL，此时是把当前bfq使用的entity剔除掉
	struct bfq_entity *in_service_entity;
	/* head-of-line entity (see comments above) */
    //next_in_service是下一次sd要使用的entity,，bfq_get_next_queue()，bfq_update_next_in_service()中赋值
    //bfq_update_next_in_service()中把分配的bfqq的entity赋值给sd->next_in_service
	struct bfq_entity *next_in_service;
	/* array of service trees, one per ioprio_class */
    //一个bfq调度类对应一个bfq_service_tree结构。service_tree[]数组保存了3+1个bfq调度类bfq_service_tree结构
	struct bfq_service_tree service_tree[BFQ_IOPRIO_CLASSES];
	/* last time CLASS_IDLE was served */
	unsigned long bfq_class_idle_last_service;

};

/**
 * struct bfq_weight_counter - counter of the number of all active queues
 *                             with a given weight.
 */
//对bfq_weight_counter各个成员的操作见__bfq_weights_tree_remove() 和 bfq_weights_tree_add()
struct bfq_weight_counter {
    //bfq_add_bfqq_busy->bfq_weights_tree_add中赋值entity->weight
	unsigned int weight; /* weight of the queues this counter refers to */
    //bfq_add_bfqq_busy->bfq_weights_tree_add()中加1
	unsigned int num_active; /* nr of active queues with this weight */
	/*
	 * Weights tree member (see bfq_data's @queue_weights_tree)
	 */
	//bfq_add_bfqq_busy->bfq_weights_tree_add()中插入bfqq->weight_counter->weights_node
	struct rb_node weights_node;
};

/**
 * struct bfq_entity - schedulable entity.
 *
 * A bfq_entity is used to represent either a bfq_queue (leaf node in the
 * cgroup hierarchy) or a bfq_group into the upper level scheduler.  Each
 * entity belongs to the sched_data of the parent group in the cgroup
 * hierarchy.  Non-leaf entities have also their own sched_data, stored
 * in @my_sched_data.
 *
 * Each entity stores independently its priority values; this would
 * allow different weights on different devices, but this
 * functionality is not exported to userspace by now.  Priorities and
 * weights are updated lazily, first storing the new values into the
 * new_* fields, then setting the @prio_changed flag.  As soon as
 * there is a transition in the entity state that allows the priority
 * update to take place the effective and the requested priority
 * values are synchronized.
 *
 * Unless cgroups are used, the weight value is calculated from the
 * ioprio to export the same interface as CFQ.  When dealing with
 * ``well-behaved'' queues (i.e., queues that do not spend too much
 * time to consume their budget and have true sequential behavior, and
 * when there are no external factors breaking anticipation) the
 * relative weights at each level of the cgroups hierarchy should be
 * guaranteed.  All the fields are protected by the queue lock of the
 * containing bfqd.
 */
//bfq_get_queue->bfq_init_entity()中对bfqq的bfq_entity初始化
struct bfq_entity {//bfq_sched_data、bfq_service_tree、bfq_entity 3者关系见bfq_entity_service_tree()
	/* service_tree member */
	struct rb_node rb_node;//bfq_entity靠rb_node 插入st->active 或st->idle 红黑树。bfq_insert()

	/*
	 * Flag, true if the entity is on a tree (either the active or
	 * the idle one of its service_tree) or is in service.
	 */
	bool on_st_or_in_serv;//bfq_forget_entity()中设置false，bfqq不再被调度

	/* B-WF2Q+ start and finish timestamps [sectors/weight] */
    //bfq_calc_finish()中根据派发req的配额除以entity->weight计算entity虚拟运行时间，然后entity->finish=entity->start+entity虚拟运行时间,
    //bfq_insert()中把entity按照entity->finish插入st->idle或者st->active 树，entity正是根据entity->finish插入st->idle或st->active红黑树，
    //entity->finish越小越往红黑树左边插入。entity虚拟运行时间，与"entiry传输字节数/weight" 有关。__bfq_activate_entity()中更新start。
    u64 start, finish;//bfqq或entity的起始和结束虚拟运行时间

	/* tree the entity is enqueued into; %NULL if not on a tree */
	struct rb_root *tree;//指向entity所属st->active或st->idle 红黑树root节点，见bfq_insert()

	/*
	 * minimum start time of the (active) subtree rooted at this
	 * entity; used for O(log N) lookups into active trees
	 */
	//bfq_active_extract或bfq_active_insert->bfq_update_active_tree->bfq_update_active_node->bfq_update_min 流程更新
	//bfqq刚插入st->active tree时的entity->start，初始entity->start值，或者叫最小的entity->start
	u64 min_start;

	/* amount of service received during the last service slot */
	int service;//bfqq或者entity已经消耗的配额,bfq_bfqq_served()中累加派发req的配额，bfq_bfqq_expire()中有清0

	/* budget, used also to calculate F_i: F_i = S_i + @budget / @weight */
	int budget;//bfqq或者entity的总配额

	/* device weight, if non-zero, it overrides the default weight of
	 * bfq_group_data */
	int dev_weight;
	/* weight of the queue */
	int weight;//bfqq权重,bfq_init_entity()中初始化，__bfq_entity_update_weight_prio()中动态更新
	/* next weight if a change is in progress */
	int new_weight;

	/* original weight, used to implement weight boosting */
	int orig_weight;//bfq_init_entity()中初始化

	/* parent entity, for hierarchical scheduling */
	struct bfq_entity *parent;//指向父bfq_entity，bfq_init_entity()中初始化，应该是blkio cgroup使用时才会用到parent

	/*
	 * For non-leaf nodes in the hierarchy, the associated
	 * scheduler queue, %NULL on leaf nodes.
	 */
	struct bfq_sched_data *my_sched_data;
	/* the scheduler queue this entity belongs to */
	struct bfq_sched_data *sched_data;////调度队列，通过它找到entity所属IO调度类的结构bfq_service_tree，bfq_init_entity()中初始化

	/* flag, set to request a weight, ioprio or ioprio_class change  */
    //bfq_dispatch_rq_from_bfqq->bfq_update_wr_data-> bfq_bfqq_end_wr中更新为1
    //bfq_dispatch_rq_from_bfqq->bfq_update_wr_data->switch_back_to_interactive_wr中更新为1
    //bfq_bfqq_handle_idle_busy_switch()中置1
	int prio_changed;//如果bfqq->wr_coeff变化了，就把bfqq->entity.prio_changed置1

	/* flag, set if the entity is counted in groups_with_pending_reqs */
	bool in_groups_with_pending_reqs;
};

struct bfq_group;

/**
 * struct bfq_ttime - per process thinktime stats.
 */
//bfq_update_io_thinktime()中更新大部分成员，有详细注释
struct bfq_ttime {
	/* completion time of the last request */
	u64 last_end_request;//bfq_completed_request()中赋值为最近一次bfqq上IO请求传输完成的时间，bfq_init_bfqq()默认赋于当前时间

    //ttime->ttime_total 和 ttime->ttime_mean越大，说明bfqq绑定的进程向bfqq插入IO请求越慢
	/* total process thinktime */
	u64 ttime_total;
	/* number of thinktime samples */
	unsigned long ttime_samples;//传输的IO请求个数
	/* average process thinktime */
	u64 ttime_mean;
};

/**
 * struct bfq_queue - leaf schedulable entity.
 *
 * A bfq_queue is a leaf request queue; it can be associated with an
 * io_context or more, if it  is  async or shared  between  cooperating
 * processes. @cgroup holds a reference to the cgroup, to be sure that it
 * does not disappear while a bfqq still references it (mostly to avoid
 * races between request issuing and task migration followed by cgroup
 * destruction).
 * All the fields are protected by the queue lock of the containing bfqd.
 */
//bfq_init_bfqq()中分配bfqq并对bfqq大部分成员初始化
struct bfq_queue {
	/* reference counter */
	int ref;//bfq_init_bfqq()中加1，bfq_add_bfqq_busy->bfq_weights_tree_add()中加1
	/* parent bfq_data */
	struct bfq_data *bfqd;

	/* current ioprio and ioprio class */
    //ioprio_class 是bfqq所属调度类ID，ioprio是具体调度类里的优先级。bfq_init_entity()中初始化，bfq_class_idx()计算bfqq所属调度类
	unsigned short ioprio, ioprio_class;
	/* next ioprio and ioprio class if a change is in progress */
	unsigned short new_ioprio, new_ioprio_class;

	/* last total-service-time sample, see bfq_update_inject_limit() */
    //在bfqq->sort_list空时向bfqq上插入第一个IO请求 到 该IO请求传输完成的时间。注意，第一个插入bfqq->sort_list的req并不能保证
    //第一个派发，这里假设第一个插入bfqq->sort_list的req派发时也是第一个派发。bfq_add_request()有详细注释
    //向bfqq队列插入IO请求bfq_add_request->bfq_reset_inject_limit()中每1s被设置一次0，bfq_update_inject_limit()中更新
	u64 last_serv_time_ns;
	/* limit for request injection */
    //向bfqq队列插入IO请求bfq_add_request->bfq_reset_inject_limit()中每1s设置inject_limit 0或者1。inject_limit是inject bfqq抢占bfqd->in_service_queue的阀值
    //在bfq_choose_bfqq_for_injection()体现的比较明显:只有bfqd->rq_in_driver < inject_limit 才允许选一个inject bfqq抢占bfqd->in_service_queue，
    //即只有bfq总的已派发但还在驱动层未传输完成的IO请求数(bfqd->rq_in_driver)小于inject_limit，才允许inject bfqq抢占bfqd->in_service_queue，
	unsigned int inject_limit;
	/* last time the inject limit has been decreased, in jiffies */
    //向bfqq队列插入IO请求bfq_add_request->bfq_reset_inject_limit()中每1s被设置一次jiffies
	unsigned long decrease_time_jif;//一轮更新inject_limit的系统时间

	/*
	 * Shared bfq_queue if queue is cooperating with one or more
	 * other queues.
	 */
	struct bfq_queue *new_bfqq;
	/* request-position tree member (see bfq_group's @rq_pos_tree) */
	struct rb_node pos_node;
	/* request-position tree root (see bfq_group's @rq_pos_tree) */
	struct rb_root *pos_root;

	/* sorted list of pending requests */
	struct rb_root sort_list;//bfq_add_request()中把req添加到sort_list链表，bfq_remove_request()中从bfqq->sort_list链表剔除req
	/* if fifo isn't expired, next request to serve */
	struct request *next_rq;//下一次优先派发的req，bfq_remove_request()中更新。bfq_add_request()中赋值
	/* number of sync and async requests queued */
	int queued[2];//bfq_add_request()加1，bfq_remove_request()减1
	/* number of requests currently allocated */
	int allocated;//bfq_init_rq()中分配一个bfqq则加1
	/* number of pending metadata requests */
	int meta_pending;
	/* fifo list of requests in sort_list */
	struct list_head fifo;//__bfq_insert_request()中把req插入到该链表,超时派发

	/* entity representing this queue in the scheduler */
	struct bfq_entity entity;

	/* pointer to the weight counter associated with this entity */
    //bfq_add_bfqq_busy->bfq_weights_tree_add()中分配
	struct bfq_weight_counter *weight_counter;

	/* maximum budget allowed from the feedback mechanism */
    //bfq_init_bfqq()中赋初值，另外bfq_bfqq_expire->__bfq_bfqq_recalc_budget()计算更新bfqq->max_budget
    //bfq_updated_next_req()中根据bfqq->max_budget计算新的entity的配额entity->budget
	int max_budget;
	/* budget expiration (in jiffies) */
    //bfq_init_bfqq()中赋初值。bfq_bfqq_budget_timeout()中判断bfqq->budget_timeout大于jiffies,则bfqq运行时间太长了，该bfqq要失效了。
    //bfq_completed_request()中可能赋值req传输完成时间。__bfq_set_in_service_queue()->bfq_set_budget_timeout()设置bfqq的超时时间
	unsigned long budget_timeout;

	/* number of requests on the dispatch list or inside driver */
    //还没有传输完成的IO请求个数，为0表示所有的IO请求都传输完成了，跟bfqd->rq_in_driver类似
	int dispatched;//bfq_dispatch_remove()每派发一个req则dispatched加1,bfq_completed_request()中传输完成一个req减1

	/* status flags */
	unsigned long flags;

	/* node for active/idle bfqq list inside parent bfqd */
	struct list_head bfqq_list;//bfqq靠bfqq_list插入bfqd的active_list或idle_list链表，见 bfq_active_insert()和 bfq_idle_insert()

	/* associated @bfq_ttime struct */
	struct bfq_ttime ttime;

	/* bit vector: a 1 for each seeky requests in history */
    //__bfq_insert_request()->bfq_update_io_seektime()中更新，应该表示bfqq派发的IO是不是随机IO，如果连续派发的随机IO越多seek_history越大
	u32 seek_history;//bfq_init_bfqq()中赋初值1

	/* node for the device's burst list */
	struct hlist_node burst_list_node;

	/* position of the last request enqueued */
    //__bfq_insert_request()->bfq_rq_enqueued()中赋值req的扇区结束地址,就是上一次派发的IO请求的扇区结束地址
	sector_t last_request_pos;

	/* Number of consecutive pairs of request completion and
	 * arrival, such that the queue becomes idle after the
	 * completion, but the next request arrives within an idle
	 * time slice; used only if the queue's IO_bound flag has been
	 * cleared.
	 */
	unsigned int requests_within_timer;

	/* pid of the process owning the queue, used for logging purposes */
	pid_t pid;//bfq_init_bfqq()中赋初值

	/*
	 * Pointer to the bfq_io_cq owning the bfq_queue, set to %NULL
	 * if the queue is shared.
	 */
	struct bfq_io_cq *bic;//bfq_init_rq()中赋值

	/* current maximum weight-raising time for this queue */
    //bfq_dispatch_rq_from_bfqq->bfq_update_wr_data-> bfq_bfqq_end_wr() 中更新为0
    //bfq_dispatch_rq_from_bfqq->bfq_update_wr_data->switch_back_to_interactive_wr() 中更新为bfq_wr_duration(bfqd)
    //bfq_bfqq_handle_idle_busy_switch()->bfq_update_bfqq_wr_on_rq_arrival()中更新为bfq_wr_duration(bfqd)或bfqd->bfq_wr_rt_max_time
	unsigned long wr_cur_max_time;
	/*
	 * Minimum time instant such that, only if a new request is
	 * enqueued after this time instant in an idle @bfq_queue with
	 * no outstanding requests, then the task associated with the
	 * queue it is deemed as soft real-time (see the comments on
	 * the function bfq_bfqq_softrt_next_start())
	 */
	//计算实时性IO的bfqq下一次插入IO请求的时间
	//bfq_init_bfqq()中赋值为jiffies，bfq_completed_request()和bfq_bfqq_expire()中更新，bfq_bfqq_handle_idle_busy_switch()中使用
	unsigned long soft_rt_next_start;
	/*
	 * Start time of the current weight-raising period if
	 * the @bfq-queue is being weight-raised, otherwise
	 * finish time of the last weight-raising period.
	 */
	//bfq_init_bfqq()中赋初值jiffies
	//bfq_dispatch_rq_from_bfqq->bfq_update_wr_data->bfq_bfqq_end_wr()，此时last_wr_start_finish记录的是bfqq权重更新结束时间
	//bfq_dispatch_rq_from_bfqq->bfq_update_wr_data->switch_back_to_interactive_wr()中更新为bfqq->wr_start_at_switch_to_srt
	//bfq_add_request()->bfq_bfqq_handle_idle_busy_switch()->bfq_update_bfqq_wr_on_rq_arrival在实时IO特性的bfqq在权重提升期间传输IO，更新为jiffies
	//bfq_add_request()最后赋值为jiffies，此时last_wr_start_finish记录的是bfqq权重更新开始时间
	//bfq_bfqq_expire()中bfqq过期失效赋值为jiffies，在bfqq->wr_coeff是1的情况下
	/*last_wr_start_finish可以理解为bfqq权重提升的开始时间，并且在bfqq权重提升结束时。
	bfq_add_request()最后对last_wr_start_finish有详细注释*/
	unsigned long last_wr_start_finish;
	
	/* factor by which the weight of this queue is multiplied */
    //bfq_dispatch_rq_from_bfqq->bfq_update_wr_data-> bfq_bfqq_end_wr中更新为1
    //bfq_dispatch_rq_from_bfqq->bfq_update_wr_data->switch_back_to_interactive_wr中更新为bfqd->bfq_wr_coeff
	unsigned int wr_coeff;//bfq_init_bfqq()初值是1，bfq_update_bfqq_wr_on_rq_arrival()赋值为bfqd->bfq_wr_coeff即30或者更大
	/*
	 * Time of the last transition of the @bfq_queue from idle to
	 * backlogged.
	 */
	//bfq_add_request->bfq_bfqq_handle_idle_busy_switch()中设置为jiffies
	unsigned long last_idle_bklogged;
	/*
	 * Cumulative service received from the @bfq_queue since the
	 * last transition from idle to backlogged.
	 */
	//bfq_add_request->bfq_bfqq_served()中累加待派发req的配额served
	unsigned long service_from_backlogged;
	/*
	 * Cumulative service received from the @bfq_queue since its
	 * last transition to weight-raised state.
	 */
	//是bfqq在权重提升的情况下消耗的配额，bfq_bfqq_served()中累加
	unsigned long service_from_wr;

	/*
	 * Value of wr start time when switching to soft rt
	 */
	//bfq_init_bfqq()赋初值为很大的负数，bfq_update_bfqq_wr_on_rq_arrival()中赋值。如果bfqq原本是交互式IO，然后又被判定为
	//实时性IO，此时在bfq_update_bfqq_wr_on_rq_arrival()中就会把wr_start_at_switch_to_srt更新为jiffies，有详细解释
	unsigned long wr_start_at_switch_to_srt;
    //bfq_init_bfqq()赋初值为很大的负数
	unsigned long split_time; /* time of last split */
    //bfq_dispatch_rq_from_bfqq->bfq_bfqq_served queue 第一次派发req的时间
	unsigned long first_IO_time; /* time of first I/O for this queue */

	/* max service rate measured so far */
	u32 max_service_rate;

	/*
	 * Pointer to the waker queue for this queue, i.e., to the
	 * queue Q such that this queue happens to get new I/O right
	 * after some I/O request of Q is completed. For details, see
	 * the comments on the choice of the queue for injection in
	 * bfq_select_queue().
	 */
	struct bfq_queue *waker_bfqq;
	/* node for woken_list, see below */
	struct hlist_node woken_list_node;
	/*
	 * Head of the list of the woken queues for this queue, i.e.,
	 * of the list of the queues for which this queue is a waker
	 * queue. This list is used to reset the waker_bfqq pointer in
	 * the woken queues when this queue exits.
	 */
	struct hlist_head woken_list;
};

/**
 * struct bfq_io_cq - per (request_queue, io_context) structure.
 */
//struct bfq_io_cq  就是bic
struct bfq_io_cq {
	/* associated io_cq structure */
	struct io_cq icq; /* must be the first member */
	/* array of two process queues, the sync and the async */
    //bfq_get_bfqq_handle_split->bic_set_bfqq()函数中bic->bfqq[is_sync] = bfqq，即新分配的bfqq是保存到bic->bfqq[]里的
    //struct bfq_queue *bfqq[2]数组有两个bfqq，一个sync一个async
	struct bfq_queue *bfqq[2];
	/* per (request_queue, blkcg) ioprio */
	int ioprio;
#ifdef CONFIG_BFQ_GROUP_IOSCHED
	uint64_t blkcg_serial_nr; /* the current blkcg serial */
#endif
	/*
	 * Snapshot of the has_short_time flag before merging; taken
	 * to remember its value while the queue is merged, so as to
	 * be able to restore it in case of split.
	 */
	bool saved_has_short_ttime;
	/*
	 * Same purpose as the previous two fields for the I/O bound
	 * classification of a queue.
	 */
	bool saved_IO_bound;

	/*
	 * Same purpose as the previous fields for the value of the
	 * field keeping the queue's belonging to a large burst
	 */
	bool saved_in_large_burst;
	/*
	 * True if the queue belonged to a burst list before its merge
	 * with another cooperating queue.
	 */
	bool was_in_burst_list;

	/*
	 * Save the weight when a merge occurs, to be able
	 * to restore it in case of split. If the weight is not
	 * correctly resumed when the queue is recycled,
	 * then the weight of the recycled queue could differ
	 * from the weight of the original queue.
	 */
	unsigned int saved_weight;

	/*
	 * Similar to previous fields: save wr information.
	 */
	unsigned long saved_wr_coeff;
	unsigned long saved_last_wr_start_finish;
	unsigned long saved_wr_start_at_switch_to_srt;
	unsigned int saved_wr_cur_max_time;
	struct bfq_ttime saved_ttime;
};

/**
 * struct bfq_data - per-device data structure.
 *
 * All the fields are protected by @lock.
 */
//bfq_init_queue()中分配bfqd并初始化大部分工具，并建立与request_queue的联系
struct bfq_data {
	/* device request queue */
	struct request_queue *queue;
	/* dispatch queue */
    //有概率向该链表添加rq，blk_mq_make_request()向IO队列插入IO请求时传入的at_head可能是true，此时就会把rq添加到bfqd->dispatch
	struct list_head dispatch;

	/* root bfq_group for the device */
	struct bfq_group *root_group;

	/*
	 * rbtree of weight counters of @bfq_queues, sorted by
	 * weight. Used to keep track of whether all @bfq_queues have
	 * the same weight. The tree contains one counter for each
	 * distinct weight associated to some active and not
	 * weight-raised @bfq_queue (see the comments to the functions
	 * bfq_weights_tree_[add|remove] for further details).
	 */
	//__bfq_weights_tree_remove()中从queue_weights_tree中remove
	struct rb_root_cached queue_weights_tree;

	/*
	 * Number of groups with at least one descendant process that
	 * has at least one request waiting for completion. Note that
	 * this accounts for also requests already dispatched, but not
	 * yet completed. Therefore this number of groups may differ
	 * (be larger) than the number of active groups, as a group is
	 * considered active only if its corresponding entity has
	 * descendant queues with at least one request queued. This
	 * number is used to decide whether a scenario is symmetric.
	 * For a detailed explanation see comments on the computation
	 * of the variable asymmetric_scenario in the function
	 * bfq_better_to_idle().
	 *
	 * However, it is hard to compute this number exactly, for
	 * groups with multiple descendant processes. Consider a group
	 * that is inactive, i.e., that has no descendant process with
	 * pending I/O inside BFQ queues. Then suppose that
	 * num_groups_with_pending_reqs is still accounting for this
	 * group, because the group has descendant processes with some
	 * I/O request still in flight. num_groups_with_pending_reqs
	 * should be decremented when the in-flight request of the
	 * last descendant process is finally completed (assuming that
	 * nothing else has changed for the group in the meantime, in
	 * terms of composition of the group and active/inactive state of child
	 * groups and processes). To accomplish this, an additional
	 * pending-request counter must be added to entities, and must
	 * be updated correctly. To avoid this additional field and operations,
	 * we resort to the following tradeoff between simplicity and
	 * accuracy: for an inactive group that is still counted in
	 * num_groups_with_pending_reqs, we decrement
	 * num_groups_with_pending_reqs when the first descendant
	 * process of the group remains with no request waiting for
	 * completion.
	 *
	 * Even this simpler decrement strategy requires a little
	 * carefulness: to avoid multiple decrements, we flag a group,
	 * more precisely an entity representing a group, as still
	 * counted in num_groups_with_pending_reqs when it becomes
	 * inactive. Then, when the first descendant queue of the
	 * entity remains with no request waiting for completion,
	 * num_groups_with_pending_reqs is decremented, and this flag
	 * is reset. After this flag is reset for the entity,
	 * num_groups_with_pending_reqs won't be decremented any
	 * longer in case a new descendant queue of the entity remains
	 * with no request waiting for completion.
	 */
	unsigned int num_groups_with_pending_reqs;

	/*
	 * Per-class (RT, BE, IDLE) number of bfq_queues containing
	 * requests (including the queue in service, even if it is
	 * idling).
	 */
	//bfq_tot_busy_queues()中判断busy_queues[0~2]累加是否为0，bfq_del_bfqq_busy()中减1,bfq_add_bfqq_busy()中加1
	//每向st->active红黑树添加一个bfqq，bfqq对应调度算法的bfqd->busy_queues[]成员加1。每从st->active 红黑树剔除一个bfqq减1
	unsigned int busy_queues[3];
	/* number of weight-raised busy @bfq_queues */
    //bfq_dispatch_rq_from_bfqq->bfq_update_wr_data-> bfq_bfqq_end_wr中减1，繁忙的bfqq个数。bfq_add_bfqq_busy()中加1
    //bfq_del_bfqq_busy()中减1，但是有个前提，bfqq->wr_coeff要大于1
	int wr_busy_queues;
	/* number of queued requests */
	int queued;//bfq_add_request()中加1，bfq_remove_request()减1
	/* number of requests dispatched and waiting for completion */
	int rq_in_driver;//已经派发但是还没传输完成的req个数，bfq_completed_request()中减1，__bfq_dispatch_request()中加1

	/* true if the device is non rotational and performs queueing */
	bool nonrot_with_queueing;

	/*
	 * Maximum number of requests in driver in the last
	 * @hw_tag_samples completed requests.
	 */
	int max_rq_in_driver;
	/* number of samples used to calculate hw_tag */
	int hw_tag_samples;
	/* flag set to one if the driver is showing a queueing behavior */
    //bfq_completed_request()->bfq_update_hw_tag()IO请求传输完成更新hw_tag，
    //bfq已经派发但是还没传输完成的IO请求数大于3则设置bfqd->hw_tag为true
	int hw_tag;

	/* number of budgets assigned */
	int budgets_assigned;//__bfq_set_in_service_queue()

	/*
	 * Timer set when idling (waiting) for the next request from
	 * the queue in service.
	 */
	struct hrtimer idle_slice_timer;//ilde定时器，bfq_arm_slice_timer()中启动，定时器函数是 bfq_idle_slice_timer()

	/* bfq_queue in service */
    //__bfq_dispatch_request()->bfq_select_queue()->__bfq_set_in_service_queue()中设置
	struct bfq_queue *in_service_queue;//bfqd当前正在使用的bfqq。__bfq_bfqd_reset_in_service()中置NULL

	/* on-disk position of the last served request */
	sector_t last_position;//bfq_update_peak_rate()等于要本次要派发的req的结束扇区

	/* position of the last served request for the in-service queue */
	sector_t in_serv_last_pos;//bfq_update_peak_rate()中更新，上一次要派发的req的扇区结束地址

	/* time of last request completion (ns) */
	u64 last_completion;//bfq_completed_request()中req传输完成更新时间

	/* bfqq owning the last completed rq */
    //bfq_completed_request()赋值传输完成的req的bfqq
	struct bfq_queue *last_completed_rq_bfqq;

	/* time of last transition from empty to non-empty (ns) */
    //向bfqq队列插入IO请求bfq_add_request()设置为当前时间
	u64 last_empty_occupied_ns;

	/*
	 * Flag set to activate the sampling of the total service time
	 * of a just-arrived first I/O request (see
	 * bfq_update_inject_limit()). This will cause the setting of
	 * waited_rq when the request is finally dispatched.
	 */
	//向bfqq队列插入IO请求bfq_add_request()中设置wait_dispatch=true
	bool wait_dispatch;
	/*
	 *  If set, then bfq_update_inject_limit() is invoked when
	 *  waited_rq is eventually completed.
	 */
	//向bfqq队列插入IO请求bfq_add_request->bfq_reset_inject_limit()中每1s被设置NULL
	struct request *waited_rq;//bfq_dispatch_rq_from_bfqq()赋值为派发的req
	/*
	 * True if some request has been injected during the last service hole.
	 */
	//bfq_update_inject_limit()和bfq_reset_inject_limit()设置为false。在bfq_choose_bfqq_for_injection()中，rqs_injected被设置为true
	//rqs_injected为true表示开始使用inject bfqq抢占了bfqd->in_service_queue，之后开始派发inject bfqq上的一个IO请求，注意只是一个IO请求。
	bool rqs_injected;

	/* time of first rq dispatch in current observation interval (ns) */
	u64 first_dispatch;//bfq_reset_rate_computation()中更新
	/* time of last rq dispatch in current observation interval (ns) */
	u64 last_dispatch;//bfq_update_peak_rate()中记录本次派发req的时间

	/* beginning of the last budget */
    //bfq_set_in_service_queue->__bfq_set_in_service_queue->bfq_set_budget_timeout()中设置为当前时间
	ktime_t last_budget_start;
	/* beginning of the last idle slice */
	ktime_t last_idling_start;//bfq_arm_slice_timer()中设置为启动定时器的时间
	unsigned long last_idling_start_jiffies;//bfq_arm_slice_timer()中设置为启动定时器的时间

	/* number of samples in current observation interval */
	int peak_rate_samples;//bfq_dispatch_remove->bfq_update_peak_rate()中每派发一个IO请求则加1
	/* num of samples of seq dispatches in current observation interval */
	u32 sequential_samples;
	/* total num of sectors transferred in current observation interval */
	u64 tot_sectors_dispatched;//bfq_update_peak_rate()中累加派发的req的字节数
	/* max rq size seen during current observation interval (sectors) */
	u32 last_rq_max_size;//派发的req对应的最大的字节数，bfq_update_peak_rate()中更新
	/* time elapsed from first dispatch in current observ. interval (us) */
	u64 delta_from_first;//从派发第一个req到当前派发req之间的时间差，bfq_update_peak_rate()中更新。bfq_update_rate_reset()中也有更新
	/*
	 * Current estimate of the device peak rate, measured in
	 * [(sectors/usec) / 2^BFQ_RATE_SHIFT]. The left-shift by
	 * BFQ_RATE_SHIFT is performed to increase precision in
	 * fixed-point calculations.
	 */
	u32 peak_rate;//bfq_update_rate_reset()计算peak_rate，bfq_calc_max_budget()中根据peak_rate计算bfqd->bfq_max_budget

	/* maximum budget allotted to a bfq_queue before rescheduling */
	int bfq_max_budget;//影响bfqq的最大配额budget

	/* list of all the bfq_queues active on the device */
    //bfq_active_insert()把entity插入st->active tree后，把entity所属bfqq插入次active_list链表
	struct list_head active_list;
	/* list of all the bfq_queues idle on the device */
    //bfq_idle_insert()把entity插入st->idle tree后，把entity所属bfqq插入次idle_list链表
	struct list_head idle_list;

	/*
	 * Timeout for async/sync requests; when it fires, requests
	 * are served in fifo order.
	 */
	u64 bfq_fifo_expire[2];//req超时时间，__bfq_insert_request()中赋值
	/* weight of backward seeks wrt forward ones */
	unsigned int bfq_back_penalty;
	/* maximum allowed backward seek */
	unsigned int bfq_back_max;
	/* maximum idling time */
	u32 bfq_slice_idle;//bfq_init_queue()赋初值8ms

	/* user-configured max budget value (0 for auto-tuning) */
	int bfq_user_max_budget;
	/*
	 * Timeout for bfq_queues to consume their budget; used to
	 * prevent seeky queues from imposing long latencies to
	 * sequential or quasi-sequential ones (this also implies that
	 * seeky queues cannot receive guarantees in the service
	 * domain; after a timeout they are charged for the time they
	 * have been in service, to preserve fairness among them, but
	 * without service-domain guarantees).
	 */
	unsigned int bfq_timeout;

	/*
	 * Number of consecutive requests that must be issued within
	 * the idle time slice to set again idling to a queue which
	 * was marked as non-I/O-bound (see the definition of the
	 * IO_bound flag for further details).
	 */
	unsigned int bfq_requests_within_timer;//bfq_init_queue()中默认120

	/*
	 * Force device idling whenever needed to provide accurate
	 * service guarantees, without caring about throughput
	 * issues. CAVEAT: this may even increase latencies, in case
	 * of useless idling for processes that did stop doing I/O.
	 */
	bool strict_guarantees;

	/*
	 * Last time at which a queue entered the current burst of
	 * queues being activated shortly after each other; for more
	 * details about this and the following parameters related to
	 * a burst of activations, see the comments on the function
	 * bfq_handle_burst.
	 */
	unsigned long last_ins_in_burst;
	/*
	 * Reference time interval used to decide whether a queue has
	 * been activated shortly after @last_ins_in_burst.
	 */
	unsigned long bfq_burst_interval;
	/* number of queues in the current burst of queue activations */
	int burst_size;

	/* common parent entity for the queues in the burst */
	struct bfq_entity *burst_parent_entity;
	/* Maximum burst size above which the current queue-activation
	 * burst is deemed as 'large'.
	 */
	unsigned long bfq_large_burst_thresh;
	/* true if a large queue-activation burst is in progress */
	bool large_burst;
	/*
	 * Head of the burst list (as for the above fields, more
	 * details in the comments on the function bfq_handle_burst).
	 */
	struct hlist_head burst_list;

	/* if set to true, low-latency heuristics are enabled */
	bool low_latency;//bfq_init_queue()默认初值1
	/*
	 * Maximum factor by which the weight of a weight-raised queue
	 * is multiplied.
	 */
	unsigned int bfq_wr_coeff;//bfq_init_queue()赋初值30
	/* maximum duration of a weight-raising period (jiffies) */
	unsigned int bfq_wr_max_time;//bfq_init_queue()中赋值0

	/* Maximum weight-raising duration for soft real-time processes */
	unsigned int bfq_wr_rt_max_time;//bfq_init_queue()赋初值300
	/*
	 * Minimum idle period after which weight-raising may be
	 * reactivated for a queue (in jiffies).
	 */
	//bfq_init_queue()赋值2000ms
	unsigned int bfq_wr_min_idle_time;
	/*
	 * Minimum period between request arrivals after which
	 * weight-raising may be reactivated for an already busy async
	 * queue (in jiffies).
	 */
	//bfq_init_queue()赋初值500ms
	unsigned long bfq_wr_min_inter_arr_async;

	/* Max service-rate for a soft real-time queue, in sectors/sec */
	unsigned int bfq_wr_max_softrt_rate;//bfq_init_queue()中赋值7000
	/*
	 * Cached value of the product ref_rate*ref_wr_duration, used
	 * for computing the maximum duration of weight raising
	 * automatically.
	 */
	u64 rate_dur_prod;

	/* fallback dummy bfqq for extreme OOM conditions */
	struct bfq_queue oom_bfqq;

	spinlock_t lock;

	/*
	 * bic associated with the task issuing current bio for
	 * merging. This and the next field are used as a support to
	 * be able to perform the bic lookup, needed by bio-merge
	 * functions, before the scheduler lock is taken, and thus
	 * avoid taking the request-queue lock while the scheduler
	 * lock is being held.
	 */
	struct bfq_io_cq *bio_bic;
	/* bfqq associated with the task issuing current bio for merging */
	struct bfq_queue *bio_bfqq;

	/*
	 * Depth limits used in bfq_limit_depth (see comments on the
	 * function)
	 */
	unsigned int word_depths[2][2];
};

enum bfqq_state_flags {
	BFQQF_just_created = 0,	/*0 queue just allocated */
	BFQQF_busy,		/*1 has requests or is in service */
	BFQQF_wait_request,	/*2 waiting for a request */
	BFQQF_non_blocking_wait_rq, /*3
				     * waiting for a request
				     * without idling the device
				     */
	BFQQF_fifo_expire,	/*4 FIFO checked in this slice */
	BFQQF_has_short_ttime,	/*5 queue has a short think time */
	BFQQF_sync,		/* synchronous queue */
	BFQQF_IO_bound,		/*6
				 * bfqq has timed-out at least once
				 * having consumed at most 2/10 of
				 * its budget
				 */
	BFQQF_in_large_burst,	/*
				 * bfqq activated in a large burst,
				 * see comments to bfq_handle_burst.
				 */
	BFQQF_softrt_update,	/*
				 * may need softrt-next-start
				 * update
				 */
	BFQQF_coop,		/* bfqq is shared */
	BFQQF_split_coop,	/* shared bfqq will be split */
	BFQQF_has_waker		/* bfqq has a waker queue */
};

#define BFQ_BFQQ_FNS(name)						\
void bfq_mark_bfqq_##name(struct bfq_queue *bfqq);			\
void bfq_clear_bfqq_##name(struct bfq_queue *bfqq);			\
int bfq_bfqq_##name(const struct bfq_queue *bfqq);

BFQ_BFQQ_FNS(just_created);
BFQ_BFQQ_FNS(busy);
BFQ_BFQQ_FNS(wait_request);
BFQ_BFQQ_FNS(non_blocking_wait_rq);
BFQ_BFQQ_FNS(fifo_expire);
BFQ_BFQQ_FNS(has_short_ttime);
BFQ_BFQQ_FNS(sync);
BFQ_BFQQ_FNS(IO_bound);
BFQ_BFQQ_FNS(in_large_burst);
BFQ_BFQQ_FNS(coop);
BFQ_BFQQ_FNS(split_coop);
BFQ_BFQQ_FNS(softrt_update);
BFQ_BFQQ_FNS(has_waker);
#undef BFQ_BFQQ_FNS

/* Expiration reasons. */
enum bfqq_expiration {
	BFQQE_TOO_IDLE = 0,		/*
					 * queue has been idling for
					 * too long
					 */
	BFQQE_BUDGET_TIMEOUT,	/* budget took too long to be used */
	BFQQE_BUDGET_EXHAUSTED,	/* budget consumed *///bfqq配额不够了
	BFQQE_NO_MORE_REQUESTS,	/* the queue has no more requests *///bfqq没有req要传输了
	BFQQE_PREEMPTED		/* preemption in progress */
};

struct bfq_stat {
	struct percpu_counter		cpu_cnt;
	atomic64_t			aux_cnt;
};

struct bfqg_stats {
#ifdef CONFIG_BFQ_CGROUP_DEBUG
	/* number of ios merged */
	struct blkg_rwstat		merged;
	/* total time spent on device in ns, may not be accurate w/ queueing */
	struct blkg_rwstat		service_time;
	/* total time spent waiting in scheduler queue in ns */
	struct blkg_rwstat		wait_time;
	/* number of IOs queued up */
	struct blkg_rwstat		queued;
	/* total disk time and nr sectors dispatched by this group */
	struct bfq_stat		time;
	/* sum of number of ios queued across all samples */
	struct bfq_stat		avg_queue_size_sum;
	/* count of samples taken for average */
	struct bfq_stat		avg_queue_size_samples;
	/* how many times this group has been removed from service tree */
	struct bfq_stat		dequeue;
	/* total time spent waiting for it to be assigned a timeslice. */
	struct bfq_stat		group_wait_time;
	/* time spent idling for this blkcg_gq */
	struct bfq_stat		idle_time;
	/* total time with empty current active q with other requests queued */
	struct bfq_stat		empty_time;
	/* fields after this shouldn't be cleared on stat reset */
	u64				start_group_wait_time;
	u64				start_idle_time;
	u64				start_empty_time;
	uint16_t			flags;
#endif /* CONFIG_BFQ_CGROUP_DEBUG */
};

#ifdef CONFIG_BFQ_GROUP_IOSCHED

/*
 * struct bfq_group_data - per-blkcg storage for the blkio subsystem.
 *
 * @ps: @blkcg_policy_storage that this structure inherits
 * @weight: weight of the bfq_group
 */
//bfq_cpd_alloc()里分配struct bfq_group_data，struct bfq_group_data结构体包含blkcg_policy_data
struct bfq_group_data {
	/* must be the first member */
	struct blkcg_policy_data pd;

	unsigned int weight;
};

/**
 * struct bfq_group - per (device, cgroup) data structure.
 * @entity: schedulable entity to insert into the parent group sched_data.
 * @sched_data: own sched_data, to contain child entities (they may be
 *              both bfq_queues and bfq_groups).
 * @bfqd: the bfq_data for the device this group acts upon.
 * @async_bfqq: array of async queues for all the tasks belonging to
 *              the group, one queue per ioprio value per ioprio_class,
 *              except for the idle class that has only one queue.
 * @async_idle_bfqq: async queue for the idle class (ioprio is ignored).
 * @my_entity: pointer to @entity, %NULL for the toplevel group; used
 *             to avoid too many special cases during group creation/
 *             migration.
 * @stats: stats for this bfqg.
 * @active_entities: number of active entities belonging to the group;
 *                   unused for the root group. Used to know whether there
 *                   are groups with more than one active @bfq_entity
 *                   (see the comments to the function
 *                   bfq_bfqq_may_idle()).
 * @rq_pos_tree: rbtree sorted by next_request position, used when
 *               determining if two or more queues have interleaving
 *               requests (see bfq_find_close_cooperator()).
 *
 * Each (device, cgroup) pair has its own bfq_group, i.e., for each cgroup
 * there is a set of bfq_groups, each one collecting the lower-level
 * entities belonging to the group that are acting on the same device.
 *
 * Locking works as follows:
 *    o @bfqd is protected by the queue lock, RCU is used to access it
 *      from the readers.
 *    o All the other fields are protected by the @bfqd queue lock.
 */
struct bfq_group {
	/* must be the first member */
	struct blkg_policy_data pd;

	/* cached path for this blkg (see comments in bfq_bic_update_cgroup) */
	char blkg_path[128];

	/* reference counter (see comments in bfq_bic_update_cgroup) */
	int ref;

	struct bfq_entity entity;
	struct bfq_sched_data sched_data;

	void *bfqd;

	struct bfq_queue *async_bfqq[2][IOPRIO_BE_NR];
	struct bfq_queue *async_idle_bfqq;

	struct bfq_entity *my_entity;

	int active_entities;//st->active tree有多少个entity

	struct rb_root rq_pos_tree;

	struct bfqg_stats stats;
};

#else
struct bfq_group {
	struct bfq_entity entity;
	struct bfq_sched_data sched_data;

	struct bfq_queue *async_bfqq[2][IOPRIO_BE_NR];
	struct bfq_queue *async_idle_bfqq;

	struct rb_root rq_pos_tree;
};
#endif

struct bfq_queue *bfq_entity_to_bfqq(struct bfq_entity *entity);

/* --------------- main algorithm interface ----------------- */

#define BFQ_SERVICE_TREE_INIT	((struct bfq_service_tree)		\
				{ RB_ROOT, RB_ROOT, NULL, NULL, 0, 0 })

extern const int bfq_timeout;

struct bfq_queue *bic_to_bfqq(struct bfq_io_cq *bic, bool is_sync);
void bic_set_bfqq(struct bfq_io_cq *bic, struct bfq_queue *bfqq, bool is_sync);
struct bfq_data *bic_to_bfqd(struct bfq_io_cq *bic);
void bfq_pos_tree_add_move(struct bfq_data *bfqd, struct bfq_queue *bfqq);
void bfq_weights_tree_add(struct bfq_data *bfqd, struct bfq_queue *bfqq,
			  struct rb_root_cached *root);
void __bfq_weights_tree_remove(struct bfq_data *bfqd,
			       struct bfq_queue *bfqq,
			       struct rb_root_cached *root);
void bfq_weights_tree_remove(struct bfq_data *bfqd,
			     struct bfq_queue *bfqq);
void bfq_bfqq_expire(struct bfq_data *bfqd, struct bfq_queue *bfqq,
		     bool compensate, enum bfqq_expiration reason);
void bfq_put_queue(struct bfq_queue *bfqq);
void bfq_end_wr_async_queues(struct bfq_data *bfqd, struct bfq_group *bfqg);
void bfq_schedule_dispatch(struct bfq_data *bfqd);
void bfq_put_async_queues(struct bfq_data *bfqd, struct bfq_group *bfqg);

/* ------------ end of main algorithm interface -------------- */

/* ---------------- cgroups-support interface ---------------- */

void bfqg_stats_update_io_add(struct bfq_group *bfqg, struct bfq_queue *bfqq,
			      unsigned int op);
void bfqg_stats_update_io_remove(struct bfq_group *bfqg, unsigned int op);
void bfqg_stats_update_io_merged(struct bfq_group *bfqg, unsigned int op);
void bfqg_stats_update_completion(struct bfq_group *bfqg, u64 start_time_ns,
				  u64 io_start_time_ns, unsigned int op);
void bfqg_stats_update_dequeue(struct bfq_group *bfqg);
void bfqg_stats_set_start_empty_time(struct bfq_group *bfqg);
void bfqg_stats_update_idle_time(struct bfq_group *bfqg);
void bfqg_stats_set_start_idle_time(struct bfq_group *bfqg);
void bfqg_stats_update_avg_queue_size(struct bfq_group *bfqg);
void bfq_bfqq_move(struct bfq_data *bfqd, struct bfq_queue *bfqq,
		   struct bfq_group *bfqg);

void bfq_init_entity(struct bfq_entity *entity, struct bfq_group *bfqg);
void bfq_bic_update_cgroup(struct bfq_io_cq *bic, struct bio *bio);
void bfq_end_wr_async(struct bfq_data *bfqd);
struct bfq_group *bfq_find_set_group(struct bfq_data *bfqd,
				     struct blkcg *blkcg);
struct blkcg_gq *bfqg_to_blkg(struct bfq_group *bfqg);
struct bfq_group *bfqq_group(struct bfq_queue *bfqq);
struct bfq_group *bfq_create_group_hierarchy(struct bfq_data *bfqd, int node);
void bfqg_and_blkg_get(struct bfq_group *bfqg);
void bfqg_and_blkg_put(struct bfq_group *bfqg);

#ifdef CONFIG_BFQ_GROUP_IOSCHED
extern struct cftype bfq_blkcg_legacy_files[];
extern struct cftype bfq_blkg_files[];
extern struct blkcg_policy blkcg_policy_bfq;
#endif

/* ------------- end of cgroups-support interface ------------- */

/* - interface of the internal hierarchical B-WF2Q+ scheduler - */

#ifdef CONFIG_BFQ_GROUP_IOSCHED//yes
/* both next loops stop at one of the child entities of the root group */
#define for_each_entity(entity)	\
	for (; entity ; entity = entity->parent)//测试证实，正常使用该for循环只有一次，entity->parent是NULL

/*
 * For each iteration, compute parent in advance, so as to be safe if
 * entity is deallocated during the iteration. Such a deallocation may
 * happen as a consequence of a bfq_put_queue that frees the bfq_queue
 * containing entity.
 */
#define for_each_entity_safe(entity, parent) \
	for (; entity && ({ parent = entity->parent; 1; }); entity = parent)

#else /* CONFIG_BFQ_GROUP_IOSCHED */
/*
 * Next two macros are fake loops when cgroups support is not
 * enabled. I fact, in such a case, there is only one level to go up
 * (to reach the root group).
 */
#define for_each_entity(entity)	\
	for (; entity ; entity = NULL)

#define for_each_entity_safe(entity, parent) \
	for (parent = NULL; entity ; entity = parent)
#endif /* CONFIG_BFQ_GROUP_IOSCHED */

struct bfq_group *bfq_bfqq_to_bfqg(struct bfq_queue *bfqq);
struct bfq_queue *bfq_entity_to_bfqq(struct bfq_entity *entity);
unsigned int bfq_tot_busy_queues(struct bfq_data *bfqd);
struct bfq_service_tree *bfq_entity_service_tree(struct bfq_entity *entity);
struct bfq_entity *bfq_entity_of(struct rb_node *node);
unsigned short bfq_ioprio_to_weight(int ioprio);
void bfq_put_idle_entity(struct bfq_service_tree *st,
			 struct bfq_entity *entity);
struct bfq_service_tree *
__bfq_entity_update_weight_prio(struct bfq_service_tree *old_st,
				struct bfq_entity *entity,
				bool update_class_too);
void bfq_bfqq_served(struct bfq_queue *bfqq, int served);
void bfq_bfqq_charge_time(struct bfq_data *bfqd, struct bfq_queue *bfqq,
			  unsigned long time_ms);
bool __bfq_deactivate_entity(struct bfq_entity *entity,
			     bool ins_into_idle_tree);
bool next_queue_may_preempt(struct bfq_data *bfqd);
struct bfq_queue *bfq_get_next_queue(struct bfq_data *bfqd);
bool __bfq_bfqd_reset_in_service(struct bfq_data *bfqd);
void bfq_deactivate_bfqq(struct bfq_data *bfqd, struct bfq_queue *bfqq,
			 bool ins_into_idle_tree, bool expiration);
void bfq_activate_bfqq(struct bfq_data *bfqd, struct bfq_queue *bfqq);
void bfq_requeue_bfqq(struct bfq_data *bfqd, struct bfq_queue *bfqq,
		      bool expiration);
void bfq_del_bfqq_busy(struct bfq_data *bfqd, struct bfq_queue *bfqq,
		       bool expiration);
void bfq_add_bfqq_busy(struct bfq_data *bfqd, struct bfq_queue *bfqq);

/* --------------- end of interface of B-WF2Q+ ---------------- */

/* Logging facilities. */
static inline void bfq_pid_to_str(int pid, char *str, int len)
{
	if (pid != -1)
		snprintf(str, len, "%d", pid);
	else
		snprintf(str, len, "SHARED-");
}

#ifdef CONFIG_BFQ_GROUP_IOSCHED//yes
struct bfq_group *bfqq_group(struct bfq_queue *bfqq);

#define bfq_log_bfqq(bfqd, bfqq, fmt, args...)	do {			\
	char pid_str[MAX_PID_STR_LENGTH];	\
	if (likely(!blk_trace_note_message_enabled((bfqd)->queue)))	\
		break;							\
	bfq_pid_to_str((bfqq)->pid, pid_str, MAX_PID_STR_LENGTH);	\
	blk_add_cgroup_trace_msg((bfqd)->queue,				\
			bfqg_to_blkg(bfqq_group(bfqq))->blkcg,		\
			"bfq%s%c " fmt, pid_str,			\
			bfq_bfqq_sync((bfqq)) ? 'S' : 'A', ##args);	\
} while (0)

#define bfq_log_bfqg(bfqd, bfqg, fmt, args...)	do {			\
	blk_add_cgroup_trace_msg((bfqd)->queue,				\
		bfqg_to_blkg(bfqg)->blkcg, fmt, ##args);		\
} while (0)

#else /* CONFIG_BFQ_GROUP_IOSCHED */

#define bfq_log_bfqq(bfqd, bfqq, fmt, args...) do {	\
	char pid_str[MAX_PID_STR_LENGTH];	\
	if (likely(!blk_trace_note_message_enabled((bfqd)->queue)))	\
		break;							\
	bfq_pid_to_str((bfqq)->pid, pid_str, MAX_PID_STR_LENGTH);	\
	blk_add_trace_msg((bfqd)->queue, "bfq%s%c " fmt, pid_str,	\
			bfq_bfqq_sync((bfqq)) ? 'S' : 'A',		\
				##args);	\
} while (0)
#define bfq_log_bfqg(bfqd, bfqg, fmt, args...)		do {} while (0)

#endif /* CONFIG_BFQ_GROUP_IOSCHED */

#define bfq_log(bfqd, fmt, args...) \
	blk_add_trace_msg((bfqd)->queue, "bfq " fmt, ##args)

#endif /* _BFQ_H */
