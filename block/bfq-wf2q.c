/*
 * Hierarchical Budget Worst-case Fair Weighted Fair Queueing
 * (B-WF2Q+): hierarchical scheduling algorithm by which the BFQ I/O
 * scheduler schedules generic entities. The latter can represent
 * either single bfq queues (associated with processes) or groups of
 * bfq queues (associated with cgroups).
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
#include "bfq-iosched.h"

/**
 * bfq_gt - compare two timestamps.
 * @a: first ts.
 * @b: second ts.
 *
 * Return @a > @b, dealing with wrapping correctly.
 */
static int bfq_gt(u64 a, u64 b)//a>b返回true，前大于后返回true
{
	return (s64)(a - b) > 0;
}

static struct bfq_entity *bfq_root_active_entity(struct rb_root *tree)
{
	struct rb_node *node = tree->rb_node;

	return rb_entry(node, struct bfq_entity, rb_node);
}
//计算bfqq所属的调度类编号
static unsigned int bfq_class_idx(struct bfq_entity *entity)
{
	struct bfq_queue *bfqq = bfq_entity_to_bfqq(entity);

	return bfqq ? bfqq->ioprio_class - 1 :
		BFQ_DEFAULT_GRP_CLASS - 1;
}
//只要有有一个bfqq可使用就返回非0，除非没有一个bfqq则返回0
unsigned int bfq_tot_busy_queues(struct bfq_data *bfqd)
{
	return bfqd->busy_queues[0] + bfqd->busy_queues[1] +
		bfqd->busy_queues[2];
}

static struct bfq_entity *bfq_lookup_next_entity(struct bfq_sched_data *sd,
						 bool expiration);

static bool bfq_update_parent_budget(struct bfq_entity *next_in_service);

/**
 * bfq_update_next_in_service - update sd->next_in_service
 * @sd: sched_data for which to perform the update.
 * @new_entity: if not NULL, pointer to the entity whose activation,
 *		requeueing or repositionig triggered the invocation of
 *		this function.
 * @expiration: id true, this function is being invoked after the
 *             expiration of the in-service entity
 *
 * This function is called to update sd->next_in_service, which, in
 * its turn, may change as a consequence of the insertion or
 * extraction of an entity into/from one of the active trees of
 * sd. These insertions/extractions occur as a consequence of
 * activations/deactivations of entities, with some activations being
 * 'true' activations, and other activations being requeueings (i.e.,
 * implementing the second, requeueing phase of the mechanism used to
 * reposition an entity in its active tree; see comments on
 * __bfq_activate_entity and __bfq_requeue_entity for details). In
 * both the last two activation sub-cases, new_entity points to the
 * just activated or requeued entity.
 *
 * Returns true if sd->next_in_service changes in such a way that
 * entity->parent may become the next_in_service for its parent
 * entity.
 */
//从st查找下一次使用的bfqq的entity并更新到sd->next_in_service，没有找到则设置sd->next_in_service为NULL
static bool bfq_update_next_in_service(struct bfq_sched_data *sd,
				       struct bfq_entity *new_entity,//new_entity有时是NULL，有时不是
				       bool expiration)//expiration为true说明是bfqq到期了，为false说明是其他情况，比如在bfq_update_next_in_service()
{
	struct bfq_entity *next_in_service = sd->next_in_service;
	bool parent_sched_may_change = false;
	bool change_without_lookup = false;

	/*
	 * If this update is triggered by the activation, requeueing
	 * or repositiong of an entity that does not coincide with
	 * sd->next_in_service, then a full lookup in the active tree
	 * can be avoided. In fact, it is enough to check whether the
	 * just-modified entity has the same priority as
	 * sd->next_in_service, is eligible and has a lower virtual
	 * finish time than sd->next_in_service. If this compound
	 * condition holds, then the new entity becomes the new
	 * next_in_service. Otherwise no change is needed.
	 */
	if (new_entity && new_entity != sd->next_in_service) {
		/*
		 * Flag used to decide whether to replace
		 * sd->next_in_service with new_entity. Tentatively
		 * set to true, and left as true if
		 * sd->next_in_service is NULL.
		 */
		change_without_lookup = true;//这里赋值change_without_lookup=true

		/*
		 * If there is already a next_in_service candidate
		 * entity, then compare timestamps to decide whether
		 * to replace sd->service_tree with new_entity.
		 */
		if (next_in_service) {
			unsigned int new_entity_class_idx =
				bfq_class_idx(new_entity);
			struct bfq_service_tree *st =
				sd->service_tree + new_entity_class_idx;

			change_without_lookup =
				(new_entity_class_idx ==
				 bfq_class_idx(next_in_service)//next_in_service和new_entity 二者同一个调度策略
				 &&
				 !bfq_gt(new_entity->start, st->vtime)//new_entity->start 小于 st->vtime
				 &&
				 bfq_gt(next_in_service->finish,//next_in_service->finish 大于 new_entity->finish
					new_entity->finish));
		}
        //change_without_lookup为true把传入的new_entity赋值给next_in_service，下边把new_entity分配给sd->next_in_service
		if (change_without_lookup)
			next_in_service = new_entity;
	}
    //如果change_without_lookup为false则执行bfq_lookup_next_entity()选择下一个使用的bfqq的entity。可能找不到，则返回NULL
	if (!change_without_lookup) /* lookup needed */
		next_in_service = bfq_lookup_next_entity(sd, expiration);

	if (next_in_service) {
		bool new_budget_triggers_change =
			bfq_update_parent_budget(next_in_service);

		parent_sched_may_change = !sd->next_in_service ||
			new_budget_triggers_change;
	}
    //为sd->next_in_servic赋值下一次使用的bfqq的entity。如果前边没找到则sd->next_in_service被赋值NULL
	sd->next_in_service = next_in_service;

	if (!next_in_service)
		return parent_sched_may_change;

	return parent_sched_may_change;
}

#ifdef CONFIG_BFQ_GROUP_IOSCHED//yes

struct bfq_group *bfq_bfqq_to_bfqg(struct bfq_queue *bfqq)
{
	struct bfq_entity *group_entity = bfqq->entity.parent;

	if (!group_entity)
		group_entity = &bfqq->bfqd->root_group->entity;

	return container_of(group_entity, struct bfq_group, entity);
}

/*
 * Returns true if this budget changes may let next_in_service->parent
 * become the next_in_service entity for its parent entity.
 */
static bool bfq_update_parent_budget(struct bfq_entity *next_in_service)
{
	struct bfq_entity *bfqg_entity;
	struct bfq_group *bfqg;
	struct bfq_sched_data *group_sd;
	bool ret = false;

	group_sd = next_in_service->sched_data;

	bfqg = container_of(group_sd, struct bfq_group, sched_data);
	/*
	 * bfq_group's my_entity field is not NULL only if the group
	 * is not the root group. We must not touch the root entity
	 * as it must never become an in-service entity.
	 */
	bfqg_entity = bfqg->my_entity;
	if (bfqg_entity) {
		if (bfqg_entity->budget > next_in_service->budget)
			ret = true;
		bfqg_entity->budget = next_in_service->budget;
	}

	return ret;
}

/*
 * This function tells whether entity stops being a candidate for next
 * service, according to the restrictive definition of the field
 * next_in_service. In particular, this function is invoked for an
 * entity that is about to be set in service.
 *
 * If entity is a queue, then the entity is no longer a candidate for
 * next service according to the that definition, because entity is
 * about to become the in-service queue. This function then returns
 * true if entity is a queue.
 *
 * In contrast, entity could still be a candidate for next service if
 * it is not a queue, and has more than one active child. In fact,
 * even if one of its children is about to be set in service, other
 * active children may still be the next to serve, for the parent
 * entity, even according to the above definition. As a consequence, a
 * non-queue entity is not a candidate for next-service only if it has
 * only one active child. And only if this condition holds, then this
 * function returns true for a non-queue entity.
 */
static bool bfq_no_longer_next_in_service(struct bfq_entity *entity)
{//yes
	struct bfq_group *bfqg;

	if (bfq_entity_to_bfqq(entity))
		return true;

	bfqg = container_of(entity, struct bfq_group, entity);

	/*
	 * The field active_entities does not always contain the
	 * actual number of active children entities: it happens to
	 * not account for the in-service entity in case the latter is
	 * removed from its active tree (which may get done after
	 * invoking the function bfq_no_longer_next_in_service in
	 * bfq_get_next_queue). Fortunately, here, i.e., while
	 * bfq_no_longer_next_in_service is not yet completed in
	 * bfq_get_next_queue, bfq_active_extract has not yet been
	 * invoked, and thus active_entities still coincides with the
	 * actual number of active entities.
	 */
	//
	if (bfqg->active_entities == 1)
		return true;

	return false;
}

#else /* CONFIG_BFQ_GROUP_IOSCHED */

struct bfq_group *bfq_bfqq_to_bfqg(struct bfq_queue *bfqq)//no
{
	return bfqq->bfqd->root_group;
}

static bool bfq_update_parent_budget(struct bfq_entity *next_in_service)
{//no
	return false;
}

static bool bfq_no_longer_next_in_service(struct bfq_entity *entity)
{
	return true;
}

#endif /* CONFIG_BFQ_GROUP_IOSCHED */

/*
 * Shift for timestamp calculations.  This actually limits the maximum
 * service allowed in one timestamp delta (small shift values increase it),
 * the maximum total weight that can be used for the queues in the system
 * (big shift values increase it), and the period of virtual time
 * wraparounds.
 */
#define WFQ_SERVICE_SHIFT	22

struct bfq_queue *bfq_entity_to_bfqq(struct bfq_entity *entity)
{
	struct bfq_queue *bfqq = NULL;

	if (!entity->my_sched_data)
		bfqq = container_of(entity, struct bfq_queue, entity);

	return bfqq;
}


/**
 * bfq_delta - map service into the virtual time domain.
 * @service: amount of service.
 * @weight: scale factor (weight of an entity or weight sum).
 */
//service是entity已经消耗的配额，除以weight得到entity已经消耗的虚拟运行时间
static u64 bfq_delta(unsigned long service, unsigned long weight)
{
	return div64_ul((u64)service << WFQ_SERVICE_SHIFT, weight);
}

/**
 * bfq_calc_finish - assign the finish time to an entity.
 * @entity: the entity to act upon.
 * @service: the service to be charged to the entity.
 */
//根据service/entity->weight计算entity已经消耗的虚拟运行时间，entity->start加上这段虚拟运行时间就是entity->finish
static void bfq_calc_finish(struct bfq_entity *entity, unsigned long service)//service是entity->budget或entity->service
{
	struct bfq_queue *bfqq = bfq_entity_to_bfqq(entity);
    
    //service是entity已经消耗的配额，除以weight得到entity已经消耗的虚拟运行时间.entity->finish等于entity->start加上entity已经消耗的虚拟运行时间
	entity->finish = entity->start +
		bfq_delta(service, entity->weight);

	if (bfqq) {
		bfq_log_bfqq(bfqq->bfqd, bfqq,
			"calc_finish: serv %lu, w %d",
			service, entity->weight);
		bfq_log_bfqq(bfqq->bfqd, bfqq,
			"calc_finish: start %llu, finish %llu, delta %llu",
			entity->start, entity->finish,
			bfq_delta(service, entity->weight));
	}
}

/**
 * bfq_entity_of - get an entity from a node.
 * @node: the node field of the entity.
 *
 * Convert a node pointer to the relative entity.  This is used only
 * to simplify the logic of some functions and not as the generic
 * conversion mechanism because, e.g., in the tree walking functions,
 * the check for a %NULL value would be redundant.
 */
struct bfq_entity *bfq_entity_of(struct rb_node *node)
{
	struct bfq_entity *entity = NULL;

	if (node)
		entity = rb_entry(node, struct bfq_entity, rb_node);

	return entity;
}

/**
 * bfq_extract - remove an entity from a tree.
 * @root: the tree root.
 * @entity: the entity to remove.
 */
static void bfq_extract(struct rb_root *root, struct bfq_entity *entity)
{
	entity->tree = NULL;
	rb_erase(&entity->rb_node, root);
}

/**
 * bfq_idle_extract - extract an entity from the idle tree.
 * @st: the service tree of the owning @entity.
 * @entity: the entity being removed.
 */
//entity从st->idle tree剔除掉，可能更新st->first_idle或st->last_idle
static void bfq_idle_extract(struct bfq_service_tree *st,
			     struct bfq_entity *entity)
{
	struct bfq_queue *bfqq = bfq_entity_to_bfqq(entity);
	struct rb_node *next;

    //entity是st->first_idle指向的entity，即st->idle红黑树最左边的entity。因为entiry要从st->idle tree剔除，则选择entity右边的entity赋于st->first_idle
	if (entity == st->first_idle) {
		next = rb_next(&entity->rb_node);
		st->first_idle = bfq_entity_of(next);
	}
    //entity是st->last_idle指向的entity，即st->idle红黑树最右边的entity。因为entiry要从st->idle tree剔除，则选择entity左边的entity赋于st->last_idle
	if (entity == st->last_idle) {
		next = rb_prev(&entity->rb_node);
		st->last_idle = bfq_entity_of(next);
	}
    //entity从st->idle tree剔除掉
	bfq_extract(&st->idle, entity);

	if (bfqq)
		list_del(&bfqq->bfqq_list);
}

/**
 * bfq_insert - generic tree insertion.
 * @root: tree root.
 * @entity: entity to insert.
 *
 * This is used for the idle and the active tree, since they are both
 * ordered by finish time.
 */
//把entity按照entity->finish插入st->idle或者st->active红黑树，entity->finish越小entity在红黑树越靠左
static void bfq_insert(struct rb_root *root, struct bfq_entity *entity)
{
	struct bfq_entity *entry;
    //st->idle或者st->active 树root节点
	struct rb_node **node = &root->rb_node;
	struct rb_node *parent = NULL;

	while (*node) {
		parent = *node;
		entry = rb_entry(parent, struct bfq_entity, rb_node);
        //entity->finish小于红黑树节点的entry->finish则if成立，然后取出entry的左节点。显然，
        //st->idle或者st->active红黑树中的的entity是按照entity->finish由小到大从左到右进行排序的
		if (bfq_gt(entry->finish, entity->finish))
			node = &parent->rb_left;
		else
			node = &parent->rb_right;
	}

	rb_link_node(&entity->rb_node, parent, node);
	rb_insert_color(&entity->rb_node, root);
    //entity->tree指向st->active或者st->idle树根节点
	entity->tree = root;
}

/**
 * bfq_update_min - update the min_start field of a entity.
 * @entity: the entity to update.
 * @node: one of its children.
 *
 * This function is called when @entity may store an invalid value for
 * min_start due to updates to the active tree.  The function  assumes
 * that the subtree rooted at @node (which may be its left or its right
 * child) has a valid min_start value.
 */
//如果当前entity在红黑树下边的子节点的entity(即child)的min_start更小，则赋值给当前entity的min_start
static void bfq_update_min(struct bfq_entity *entity, struct rb_node *node)
{
	struct bfq_entity *child;

	if (node) {
		child = rb_entry(node, struct bfq_entity, rb_node);
        //entity->min_start大于child->min_start成立，更新entity->min_start为更小的child->min_start。这是什么用意?child是entity
        //的在红黑树下边的左节点或者右节点，即子节点，如果子节点的min_start更小，就用子节点的min_start更新entity->min_start
		if (bfq_gt(entity->min_start, child->min_start))
			entity->min_start = child->min_start;
	}
}

/**
 * bfq_update_active_node - recalculate min_start.
 * @node: the node to update.
 *
 * @node may have changed position or one of its children may have moved,
 * this function updates its min_start value.  The left and right subtrees
 * are assumed to hold a correct min_start value.
 */
//先把entity->start赋值给entity->min_start，如果entity在红黑树下边的左右子节点的entity的min_start更小，再重新赋值给entity->min_start
static void bfq_update_active_node(struct rb_node *node)
{
	struct bfq_entity *entity = rb_entry(node, struct bfq_entity, rb_node);

    //用entity->start更新entity->min_start
	entity->min_start = entity->start;
    //如果当前entity在红黑树下边的左子节点的entity的min_start更小，则赋值给当前entity的min_start
	bfq_update_min(entity, node->rb_right);
    //如果当前entity在红黑树下边的右子节点的entity的min_start更小，则赋值给当前entity的min_start
	bfq_update_min(entity, node->rb_left);
}

/**
 * bfq_update_active_tree - update min_start for the whole active tree.
 * @node: the starting node.
 *
 * @node must be the deepest modified node after an update.  This function
 * updates its min_start using the values held by its children, assuming
 * that they did not change, and then updates all the nodes that may have
 * changed in the path to the root.  The only nodes that may have changed
 * are the ones in the path or their siblings.
 */
//从entity的node所在节点，一直在红黑树向上得到它的parent节点，更新这些红黑树节点对应的entity的成员min_start，同时也更新
//这些node在红黑树的左右子节点对应的entity的min_start，这个更新操作一直到根节点才结束。
static void bfq_update_active_tree(struct rb_node *node)
{
	struct rb_node *parent;

up:
    //先更新当前node对应的entity的min_start
	bfq_update_active_node(node);

	parent = rb_parent(node);
	if (!parent)//更新entity的min_start，直到entity红黑树根节点才return返回
		return;
    //如果node这个entity是父节点parent的左节点，则更新parent节点的右节点对应的entity的min_start
	if (node == parent->rb_left && parent->rb_right)
		bfq_update_active_node(parent->rb_right);
	else if (parent->rb_left)//否则说明node这个entity是父节点parent的右节点，则更新parent节点的左节点对应的entity的min_start
		bfq_update_active_node(parent->rb_left);//更新entity->min_start

	node = parent;
	goto up;
}

/**
 * bfq_active_insert - insert an entity in the active tree of its
 *                     group/device.
 * @st: the service tree of the entity.
 * @entity: the entity being inserted.
 *
 * The active tree is ordered by finish time, but an extra key is kept
 * per each node, containing the minimum value for the start times of
 * its children (and the node itself), so it's possible to search for
 * the eligible node with the lowest finish time in logarithmic time.
 */
//把entity按照新的entity->finish插入st->active红黑树
static void bfq_active_insert(struct bfq_service_tree *st,
			      struct bfq_entity *entity)
{
	struct bfq_queue *bfqq = bfq_entity_to_bfqq(entity);
	struct rb_node *node = &entity->rb_node;
#ifdef CONFIG_BFQ_GROUP_IOSCHED
	struct bfq_sched_data *sd = NULL;
	struct bfq_group *bfqg = NULL;
	struct bfq_data *bfqd = NULL;
#endif
    //把entity按照entity->finish插入st->active红黑树，entity->finish越小entity在红黑树越靠左
	bfq_insert(&st->active, entity);

	if (node->rb_left)
		node = node->rb_left;
	else if (node->rb_right)
		node = node->rb_right;

    //从entity的node所在节点，一直在红黑树向上得到它的parent节点，用entity->start更新这些红黑树节点的entity->min_start，
    //同时也更新这些node在红黑树的左右子节点对应的entity的min_start。这个更新操作一直到根节点才结束
	bfq_update_active_tree(node);

#ifdef CONFIG_BFQ_GROUP_IOSCHED
	sd = entity->sched_data;
	bfqg = container_of(sd, struct bfq_group, sched_data);
	bfqd = (struct bfq_data *)bfqg->bfqd;
#endif
	if (bfqq)//把entity所属bfqq插入次bfqq->bfqd->active_list链表
		list_add(&bfqq->bfqq_list, &bfqq->bfqd->active_list);
#ifdef CONFIG_BFQ_GROUP_IOSCHED
	if (bfqg != bfqd->root_group)
		bfqg->active_entities++;
#endif
}

/**
 * bfq_ioprio_to_weight - calc a weight from an ioprio.
 * @ioprio: the ioprio value to convert.
 */
unsigned short bfq_ioprio_to_weight(int ioprio)
{
	return (IOPRIO_BE_NR - ioprio) * BFQ_WEIGHT_CONVERSION_COEFF;
}

/**
 * bfq_weight_to_ioprio - calc an ioprio from a weight.
 * @weight: the weight value to convert.
 *
 * To preserve as much as possible the old only-ioprio user interface,
 * 0 is used as an escape ioprio value for weights (numerically) equal or
 * larger than IOPRIO_BE_NR * BFQ_WEIGHT_CONVERSION_COEFF.
 */
static unsigned short bfq_weight_to_ioprio(int weight)
{
	return max_t(int, 0,
		     IOPRIO_BE_NR * BFQ_WEIGHT_CONVERSION_COEFF - weight);
}

static void bfq_get_entity(struct bfq_entity *entity)
{
	struct bfq_queue *bfqq = bfq_entity_to_bfqq(entity);

	if (bfqq) {
		bfqq->ref++;
		bfq_log_bfqq(bfqq->bfqd, bfqq, "get_entity: %p %d",
			     bfqq, bfqq->ref);
	} else
		bfqg_and_blkg_get(container_of(entity, struct bfq_group,
					       entity));
}

/**
 * bfq_find_deepest - find the deepest node that an extraction can modify.
 * @node: the node being removed.
 *
 * Do the first step of an extraction in an rb tree, looking for the
 * node that will replace @node, and returning the deepest node that
 * the following modifications to the tree can touch.  If @node is the
 * last node in the tree return %NULL.
 */
//看着是找到传参node节点下 最下边的节点
static struct rb_node *bfq_find_deepest(struct rb_node *node)
{
	struct rb_node *deepest;

	if (!node->rb_right && !node->rb_left)//如果node没有左右节点则返回node的parent
		deepest = rb_parent(node);
	else if (!node->rb_right)//如果node没右节点但有左节点则返回node->rb_left
		deepest = node->rb_left;
	else if (!node->rb_left)//如果node没左节点但有右节点则返回node->rb_right
		deepest = node->rb_right;
	else {//如果node左右节点全有
		deepest = rb_next(node);
		if (deepest->rb_right)
			deepest = deepest->rb_right;
		else if (rb_parent(deepest) != node)
			deepest = rb_parent(deepest);
	}

	return deepest;
}

/**
 * bfq_active_extract - remove an entity from the active tree.
 * @st: the service_tree containing the tree.
 * @entity: the entity being removed.
 */
//从st->acitve tree剔除entity
static void bfq_active_extract(struct bfq_service_tree *st,
			       struct bfq_entity *entity)
{
	struct bfq_queue *bfqq = bfq_entity_to_bfqq(entity);
	struct rb_node *node;
#ifdef CONFIG_BFQ_GROUP_IOSCHED
	struct bfq_sched_data *sd = NULL;
	struct bfq_group *bfqg = NULL;
	struct bfq_data *bfqd = NULL;
#endif
    //返回entity所属红黑树下边的entiry对应节点
	node = bfq_find_deepest(&entity->rb_node);
    //把entiry从st active tree剔除
	bfq_extract(&st->active, entity);

    /*node此时是红黑树entity节点下最下方的那个节点，下边的bfq_update_active_tree()是更新node到根节点之间所有节点及其子节点
      对应的entity的min_start*/
    //从node所在节点，一直在红黑树向上得到它的parent节点，更新这些红黑树节点对应的bfq_entity的成员min_start，同时也更新
    //这些node在红黑树的左右子节点对应的entity的min_start。这个更新操作一直到根节点才结束
	if (node)
		bfq_update_active_tree(node);

#ifdef CONFIG_BFQ_GROUP_IOSCHED
	sd = entity->sched_data;
	bfqg = container_of(sd, struct bfq_group, sched_data);
	bfqd = (struct bfq_data *)bfqg->bfqd;
#endif
	if (bfqq)
		list_del(&bfqq->bfqq_list);
#ifdef CONFIG_BFQ_GROUP_IOSCHED
	if (bfqg != bfqd->root_group)
		bfqg->active_entities--;
#endif
}

/**
 * bfq_idle_insert - insert an entity into the idle tree.
 * @st: the service tree containing the tree.
 * @entity: the entity to insert.
 */
//把entity插入st->idle树，并可能更新st->first_idle或st->last_idle
static void bfq_idle_insert(struct bfq_service_tree *st,
			    struct bfq_entity *entity)
{
	struct bfq_queue *bfqq = bfq_entity_to_bfqq(entity);
	struct bfq_entity *first_idle = st->first_idle;
	struct bfq_entity *last_idle = st->last_idle;

    //st->idle红黑树的最左边的entiry的finish最小，最右边的entity的finish最大。
    
    //st->first_idle记录entity->finish最小的entity，就是st->idle红黑树最靠左的entity
	if (!first_idle || bfq_gt(first_idle->finish, entity->finish))
		st->first_idle = entity;//st->first_idle 记录st->idle中finish最小的entity

    //st->last_idle记录entity->finish最大的entity，就是st->idle红黑树最靠右的entity
	if (!last_idle || bfq_gt(entity->finish, last_idle->finish))
		st->last_idle = entity;//st->last_idle 记录st->idle中finish最大的entity
    
    //把entity按照entity->finish插入st->idle红黑树，entity->finish越小entity在红黑树越靠左
	bfq_insert(&st->idle, entity);

	if (bfqq)//加入st->idle 的红黑树都加入到bfqd->idle_list链表
		list_add(&bfqq->bfqq_list, &bfqq->bfqd->idle_list);
}

/**
 * bfq_forget_entity - do not consider entity any longer for scheduling
 * @st: the service tree.
 * @entity: the entity being removed.
 * @is_in_service: true if entity is currently the in-service entity.
 *
 * Forget everything about @entity. In addition, if entity represents
 * a queue, and the latter is not in service, then release the service
 * reference to the queue (the one taken through bfq_get_entity). In
 * fact, in this case, there is really no more service reference to
 * the queue, as the latter is also outside any service tree. If,
 * instead, the queue is in service, then __bfq_bfqd_reset_in_service
 * will take care of putting the reference when the queue finally
 * stops being served.
 */
//entity不再被用到，释放掉bfqq和entity
static void bfq_forget_entity(struct bfq_service_tree *st,
			      struct bfq_entity *entity,
			      bool is_in_service)
{
	struct bfq_queue *bfqq = bfq_entity_to_bfqq(entity);

	entity->on_st_or_in_serv = false;
    //bfqq的entity要从st剔除，st->wsum减去该enyity的权重
	st->wsum -= entity->weight;
    
	if (is_in_service)//is_in_service为1则该entity还在st的active或idle tree，不能释放bfqq
		return;

	if (bfqq)//entity不再被用到，释放bfqq,把entity剔除掉
		bfq_put_queue(bfqq);
	else
		bfqg_and_blkg_put(container_of(entity, struct bfq_group,
					       entity));
}

/**
 * bfq_put_idle_entity - release the idle tree ref of an entity.
 * @st: service tree for the entity.
 * @entity: the entity being released.
 */
//entity从st->idle tree剔除掉，entity如果不再被用到，释放掉bfqq和entity
void bfq_put_idle_entity(struct bfq_service_tree *st, struct bfq_entity *entity)
{
	bfq_idle_extract(st, entity);//entity从st->idle tree剔除掉，可能更新st->first_idle或st->last_idle
	//entity如果不再被用到，释放bfqq,把entity剔除掉
	bfq_forget_entity(st, entity,
			  entity == entity->sched_data->in_service_entity);
}

/**
 * bfq_forget_idle - update the idle tree if necessary.
 * @st: the service tree to act upon.
 *
 * To preserve the global O(log N) complexity we only remove one entry here;
 * as the idle tree will not grow indefinitely this can be done safely.
 */
//处理st->idle tree的first_idle和last_idle，可能会把first_idle指向的entity从st->idle tree剔除掉，释放掉它的bfqq和entity
static void bfq_forget_idle(struct bfq_service_tree *st)
{
	struct bfq_entity *first_idle = st->first_idle;
	struct bfq_entity *last_idle = st->last_idle;
    //st->active红黑树空并且last_idle->finish小于st->vtime则if成立，
	if (RB_EMPTY_ROOT(&st->active) && last_idle &&
	    !bfq_gt(last_idle->finish, st->vtime)) {
		/*
		 * Forget the whole idle tree, increasing the vtime past
		 * the last finish time of idle entities.
		 */
		st->vtime = last_idle->finish;
	}
    //first_idle非空，并且st->vtime大于first_idle->finish则if成立
	if (first_idle && !bfq_gt(first_idle->finish, st->vtime))
        //first_idle指向的entity从st->idle tree剔除掉，如果不再被用到，释放掉它的bfqq和entity
		bfq_put_idle_entity(st, first_idle);
}
//返回bfqq或entity所属调度类对应的bfq_service_tree结构
struct bfq_service_tree *bfq_entity_service_tree(struct bfq_entity *entity)
{
    //entity->sched_data指向的就是bfq_sched_data
	struct bfq_sched_data *sched_data = entity->sched_data;
	unsigned int idx = bfq_class_idx(entity);//计算bfqq所属的调度类编号

    //按照entity的调度策略从sched_data->service_tree[idx]找到bfq_service_tree
	return sched_data->service_tree + idx;
}

/*
 * Update weight and priority of entity. If update_class_too is true,
 * then update the ioprio_class of entity too.
 *
 * The reason why the update of ioprio_class is controlled through the
 * last parameter is as follows. Changing the ioprio class of an
 * entity implies changing the destination service trees for that
 * entity. If such a change occurred when the entity is already on one
 * of the service trees for its previous class, then the state of the
 * entity would become more complex: none of the new possible service
 * trees for the entity, according to bfq_entity_service_tree(), would
 * match any of the possible service trees on which the entity
 * is. Complex operations involving these trees, such as entity
 * activations and deactivations, should take into account this
 * additional complexity.  To avoid this issue, this function is
 * invoked with update_class_too unset in the points in the code where
 * entity may happen to be on some tree.
 */
struct bfq_service_tree *
__bfq_entity_update_weight_prio(struct bfq_service_tree *old_st,
				struct bfq_entity *entity,
				bool update_class_too)
{
	struct bfq_service_tree *new_st = old_st;

	if (entity->prio_changed) {
		struct bfq_queue *bfqq = bfq_entity_to_bfqq(entity);
		unsigned int prev_weight, new_weight;
		struct bfq_data *bfqd = NULL;
		struct rb_root_cached *root;
#ifdef CONFIG_BFQ_GROUP_IOSCHED
		struct bfq_sched_data *sd;
		struct bfq_group *bfqg;
#endif

		if (bfqq)
			bfqd = bfqq->bfqd;
#ifdef CONFIG_BFQ_GROUP_IOSCHED
		else {
			sd = entity->my_sched_data;
			bfqg = container_of(sd, struct bfq_group, sched_data);
			bfqd = (struct bfq_data *)bfqg->bfqd;
		}
#endif

		/* Matches the smp_wmb() in bfq_group_set_weight. */
		smp_rmb();
		old_st->wsum -= entity->weight;

		if (entity->new_weight != entity->orig_weight) {
			if (entity->new_weight < BFQ_MIN_WEIGHT ||
			    entity->new_weight > BFQ_MAX_WEIGHT) {
				pr_crit("update_weight_prio: new_weight %d\n",
					entity->new_weight);
				if (entity->new_weight < BFQ_MIN_WEIGHT)
					entity->new_weight = BFQ_MIN_WEIGHT;
				else
					entity->new_weight = BFQ_MAX_WEIGHT;
			}
			entity->orig_weight = entity->new_weight;
			if (bfqq)
				bfqq->ioprio =
				  bfq_weight_to_ioprio(entity->orig_weight);
		}

		if (bfqq && update_class_too)
			bfqq->ioprio_class = bfqq->new_ioprio_class;

		/*
		 * Reset prio_changed only if the ioprio_class change
		 * is not pending any longer.
		 */
		if (!bfqq || bfqq->ioprio_class == bfqq->new_ioprio_class)
			entity->prio_changed = 0;

		/*
		 * NOTE: here we may be changing the weight too early,
		 * this will cause unfairness.  The correct approach
		 * would have required additional complexity to defer
		 * weight changes to the proper time instants (i.e.,
		 * when entity->finish <= old_st->vtime).
		 */
		new_st = bfq_entity_service_tree(entity);

		prev_weight = entity->weight;
		new_weight = entity->orig_weight *
			     (bfqq ? bfqq->wr_coeff : 1);
		/*
		 * If the weight of the entity changes, and the entity is a
		 * queue, remove the entity from its old weight counter (if
		 * there is a counter associated with the entity).
		 */
		if (prev_weight != new_weight && bfqq) {
			root = &bfqd->queue_weights_tree;
			__bfq_weights_tree_remove(bfqd, bfqq, root);
		}
		entity->weight = new_weight;
		/*
		 * Add the entity, if it is not a weight-raised queue,
		 * to the counter associated with its new weight.
		 */
		if (prev_weight != new_weight && bfqq && bfqq->wr_coeff == 1) {
			/* If we get here, root has been initialized. */
			bfq_weights_tree_add(bfqd, bfqq, root);
		}

		new_st->wsum += entity->weight;

		if (new_st != old_st)
			entity->start = new_st->vtime;
	}

	return new_st;
}

/**
 * bfq_bfqq_served - update the scheduler status after selection for
 *                   service.
 * @bfqq: the queue being served.
 * @served: bytes to transfer.
 *
 * NOTE: this can be optimized, as the timestamps of upper level entities
 * are synchronized every time a new bfqq is selected for service.  By now,
 * we keep it to better check consistency.
 */
//根据待派发req的配额served，增加entity配额entity->service和调度器虚拟运行时间st->vtime
void bfq_bfqq_served(struct bfq_queue *bfqq, int served)//served是当前要派发req消耗的配额
{
	struct bfq_entity *entity = &bfqq->entity;
	struct bfq_service_tree *st;

	if (!bfqq->service_from_backlogged)
		bfqq->first_IO_time = jiffies;

	if (bfqq->wr_coeff > 1)
		bfqq->service_from_wr += served;

	bfqq->service_from_backlogged += served;
	for_each_entity(entity) {
        //取出entity指向的调度器实体st
		st = bfq_entity_service_tree(entity);
        //entity->service累加待派发req的配额served
		entity->service += served;
        //st->vtime累加待派发req的配额served除以st->wsum算出的虚拟运行时间
		st->vtime += bfq_delta(served, st->wsum);
        //处理st->idle tree的first_idle和last_idle，可能会把first_idle指向的entity从st->idle tree剔除掉，释放掉它的bfqq和entity
		bfq_forget_idle(st);
	}
	bfq_log_bfqq(bfqq->bfqd, bfqq, "bfqq_served %d secs", served);
}

/**
 * bfq_bfqq_charge_time - charge an amount of service equivalent to the length
 *			  of the time interval during which bfqq has been in
 *			  service.
 * @bfqd: the device
 * @bfqq: the queue that needs a service update.
 * @time_ms: the amount of time during which the queue has received service
 *
 * If a queue does not consume its budget fast enough, then providing
 * the queue with service fairness may impair throughput, more or less
 * severely. For this reason, queues that consume their budget slowly
 * are provided with time fairness instead of service fairness. This
 * goal is achieved through the BFQ scheduling engine, even if such an
 * engine works in the service, and not in the time domain. The trick
 * is charging these queues with an inflated amount of service, equal
 * to the amount of service that they would have received during their
 * service slot if they had been fast, i.e., if their requests had
 * been dispatched at a rate equal to the estimated peak rate.
 *
 * It is worth noting that time fairness can cause important
 * distortions in terms of bandwidth distribution, on devices with
 * internal queueing. The reason is that I/O requests dispatched
 * during the service slot of a queue may be served after that service
 * slot is finished, and may have a total processing time loosely
 * correlated with the duration of the service slot. This is
 * especially true for short service slots.
 */
//会执行bfq_bfqq_served()增加entity配额entity->service和调度器虚拟运行时间st->vtime
void bfq_bfqq_charge_time(struct bfq_data *bfqd, struct bfq_queue *bfqq,
			  unsigned long time_ms)
{
	struct bfq_entity *entity = &bfqq->entity;
	unsigned long timeout_ms = jiffies_to_msecs(bfq_timeout);
	unsigned long bounded_time_ms = min(time_ms, timeout_ms);
	int serv_to_charge_for_time =
		(bfqd->bfq_max_budget * bounded_time_ms) / timeout_ms;
	int tot_serv_to_charge = max(serv_to_charge_for_time, entity->service);

	/* Increase budget to avoid inconsistencies */
	if (tot_serv_to_charge > entity->budget)
		entity->budget = tot_serv_to_charge;
    //根据tot_serv_to_charge - entity->service 增加entity配额entity->service和调度器虚拟运行时间st->vtime
	bfq_bfqq_served(bfqq,
			max_t(int, 0, tot_serv_to_charge - entity->service));
}
//按照entity->budget计算entity的虚拟运行时间并累加到entity->finish，最后把entity按照新的entity->finish插入st->active红黑树
static void bfq_update_fin_time_enqueue(struct bfq_entity *entity,
					struct bfq_service_tree *st,
					bool backshifted)
{
	struct bfq_queue *bfqq = bfq_entity_to_bfqq(entity);

	/*
	 * When this function is invoked, entity is not in any service
	 * tree, then it is safe to invoke next function with the last
	 * parameter set (see the comments on the function).
	 */
	st = __bfq_entity_update_weight_prio(st, entity, true);
    //这里简化成entity->finish=entity->start+entity->budget/entity->weight，用entity的配额得到entity->finish，注意是entity的配额
	bfq_calc_finish(entity, entity->budget);

	/*
	 * If some queues enjoy backshifting for a while, then their
	 * (virtual) finish timestamps may happen to become lower and
	 * lower than the system virtual time.	In particular, if
	 * these queues often happen to be idle for short time
	 * periods, and during such time periods other queues with
	 * higher timestamps happen to be busy, then the backshifted
	 * timestamps of the former queues can become much lower than
	 * the system virtual time. In fact, to serve the queues with
	 * higher timestamps while the ones with lower timestamps are
	 * idle, the system virtual time may be pushed-up to much
	 * higher values than the finish timestamps of the idle
	 * queues. As a consequence, the finish timestamps of all new
	 * or newly activated queues may end up being much larger than
	 * those of lucky queues with backshifted timestamps. The
	 * latter queues may then monopolize the device for a lot of
	 * time. This would simply break service guarantees.
	 *
	 * To reduce this problem, push up a little bit the
	 * backshifted timestamps of the queue associated with this
	 * entity (only a queue can happen to have the backshifted
	 * flag set): just enough to let the finish timestamp of the
	 * queue be equal to the current value of the system virtual
	 * time. This may introduce a little unfairness among queues
	 * with backshifted timestamps, but it does not break
	 * worst-case fairness guarantees.
	 *
	 * As a special case, if bfqq is weight-raised, push up
	 * timestamps much less, to keep very low the probability that
	 * this push up causes the backshifted finish timestamps of
	 * weight-raised queues to become higher than the backshifted
	 * finish timestamps of non weight-raised queues.
	 */
	if (backshifted && bfq_gt(st->vtime, entity->finish)) {
		unsigned long delta = st->vtime - entity->finish;

		if (bfqq)
			delta /= bfqq->wr_coeff;

		entity->start += delta;
		entity->finish += delta;
	}
    
    //把entity按照新的entity->finish插入st->active红黑树
	bfq_active_insert(st, entity);
}

/**
 * __bfq_activate_entity - handle activation of entity.
 * @entity: the entity being activated.
 * @non_blocking_wait_rq: true if entity was waiting for a request
 *
 * Called for a 'true' activation, i.e., if entity is not active and
 * one of its children receives a new request.
 *
 * Basically, this function updates the timestamps of entity and
 * inserts entity into its active tree, after possibly extracting it
 * from its idle tree.
 */
//计算新的entity->start，按照entity->budget计算entity的虚拟运行时间并累加到entity->finish，把entity按照entity->finish插入st->active tree
static void __bfq_activate_entity(struct bfq_entity *entity,
				  bool non_blocking_wait_rq)
{
	struct bfq_service_tree *st = bfq_entity_service_tree(entity);
	bool backshifted = false;
	unsigned long long min_vstart;

	/* See comments on bfq_fqq_update_budg_for_activation */
    //如果bfqq被设置了non_blocking_wait_rq标记，并且 st->vtime大于 entity->finish，则 min_vstart 设置为 entity->finish。
	//为什么这样做？这样可以保证下边把bfqq的entity插入st->active tree时，更靠近st->active tree左边，这样更容易被调度到。
	if (non_blocking_wait_rq && bfq_gt(st->vtime, entity->finish)) {//处于st->idle tree的走这里
		backshifted = true;
		min_vstart = entity->finish;
	} else//新分配的bfqq走这里
		min_vstart = st->vtime;

    //entity现在处于st->idle tree
	if (entity->tree == &st->idle) {
		/*
		 * Must be on the idle tree, bfq_idle_extract() will
		 * check for that.
		 */
		//entity从st->idle tree剔除掉，可能更新st->first_idle或st->last_idle
		bfq_idle_extract(st, entity);
		entity->start = bfq_gt(min_vstart, entity->finish) ?
			min_vstart : entity->finish;
	} else {//到这里说明entity之前不在st->idle或st->active红黑树。entity是新分配的也走这里
		/*
		 * The finish time of the entity may be invalid, and
		 * it is in the past for sure, otherwise the queue
		 * would have been on the idle tree.
		 */
		//更新entity->start 
		entity->start = min_vstart;
        //st->wsum调度类st的active和idle tree上所有entity的权重之和，累加每一个entity的entity->weight
		st->wsum += entity->weight;
		/*
		 * entity is about to be inserted into a service tree,
		 * and then set in service: get a reference to make
		 * sure entity does not disappear until it is no
		 * longer in service or scheduled for service.
		 */
		bfq_get_entity(entity);

		entity->on_st_or_in_serv = true;
	}

#ifdef CONFIG_BFQ_GROUP_IOSCHED
	if (!bfq_entity_to_bfqq(entity)) { /* bfq_group */
		struct bfq_group *bfqg =
			container_of(entity, struct bfq_group, entity);
		struct bfq_data *bfqd = bfqg->bfqd;

		if (!entity->in_groups_with_pending_reqs) {
			entity->in_groups_with_pending_reqs = true;
			bfqd->num_groups_with_pending_reqs++;
		}
	}
#endif

    //按照entity->budget计算entity的虚拟运行时间并累加到entity->finish，最后把entity按照新的entity->finish插入st->active红黑树
	bfq_update_fin_time_enqueue(entity, st, backshifted);
}

/**
 * __bfq_requeue_entity - handle requeueing or repositioning of an entity.
 * @entity: the entity being requeued or repositioned.
 *
 * Requeueing is needed if this entity stops being served, which
 * happens if a leaf descendant entity has expired. On the other hand,
 * repositioning is needed if the next_inservice_entity for the child
 * entity has changed. See the comments inside the function for
 * details.
 *
 * Basically, this function: 1) removes entity from its active tree if
 * present there, 2) updates the timestamps of entity and 3) inserts
 * entity back into its active tree (in the new, right position for
 * the new values of the timestamps).
 */
//根据entity->service计算entity的虚拟运行时间并累加到entity->finish，最后把entity按照新的entity->finish插入st->active红黑树
static void __bfq_requeue_entity(struct bfq_entity *entity)
{
	struct bfq_sched_data *sd = entity->sched_data;
	struct bfq_service_tree *st = bfq_entity_service_tree(entity);

	if (entity == sd->in_service_entity) {//entity是IO调度器正在使用的entity
		/*
		 * We are requeueing the current in-service entity,
		 * which may have to be done for one of the following
		 * reasons:
		 * - entity represents the in-service queue, and the
		 *   in-service queue is being requeued after an
		 *   expiration;
		 * - entity represents a group, and its budget has
		 *   changed because one of its child entities has
		 *   just been either activated or requeued for some
		 *   reason; the timestamps of the entity need then to
		 *   be updated, and the entity needs to be enqueued
		 *   or repositioned accordingly.
		 *
		 * In particular, before requeueing, the start time of
		 * the entity must be moved forward to account for the
		 * service that the entity has received while in
		 * service. This is done by the next instructions. The
		 * finish time will then be updated according to this
		 * new value of the start time, and to the budget of
		 * the entity.
		 */
		 
		//根据entity->service/entity->weight计算entity已经消耗的虚拟运行时间，entity->start加上这段虚拟运行时间就是entity->finish
		bfq_calc_finish(entity, entity->service);
        //用entity->finish更新entity->start，重置了
		entity->start = entity->finish;
		/*
		 * In addition, if the entity had more than one child
		 * when set in service, then it was not extracted from
		 * the active tree. This implies that the position of
		 * the entity in the active tree may need to be
		 * changed now, because we have just updated the start
		 * time of the entity, and we will update its finish
		 * time in a moment (the requeueing is then, more
		 * precisely, a repositioning in this case). To
		 * implement this repositioning, we: 1) dequeue the
		 * entity here, 2) update the finish time and requeue
		 * the entity according to the new timestamps below.
		 */
		//如果entity在st->active树，则从st->acitve tree剔除entity，下边再把entity加入st->active树
		if (entity->tree)
			bfq_active_extract(st, entity);
	} else { /* The entity is already active, and not in service */
		/*
		 * In this case, this function gets called only if the
		 * next_in_service entity below this entity has
		 * changed, and this change has caused the budget of
		 * this entity to change, which, finally implies that
		 * the finish time of this entity must be
		 * updated. Such an update may cause the scheduling,
		 * i.e., the position in the active tree, of this
		 * entity to change. We handle this change by: 1)
		 * dequeueing the entity here, 2) updating the finish
		 * time and requeueing the entity according to the new
		 * timestamps below. This is the same approach as the
		 * non-extracted-entity sub-case above.
		 */
		//从st->acitve tree剔除entity。这里不禁有疑问，上边也是执行bfq_active_extract()，有什么区别?上边更新了entity->finish，这里没有
		//下边把entity重新插入st->active树时，就是按照最新entity->finish把entity插入st->active
		bfq_active_extract(st, entity);
	}
    
    //按照entity->budget计算entity的虚拟运行时间并累加到entity->finish，最后把entity按照新的entity->finish插入st->active红黑树
	bfq_update_fin_time_enqueue(entity, st, false);
}

//如果entity是调度器正在使用的entiry或entity处于st->active树,计算entity的虚拟运行时间并累加到entity->finish，把entity按照新的
//entity->finish插入st->active红黑树。否则计算新的entity->start，按照entity->budget计算entity的虚拟运行时间并累加到entity->finish，
//最后把entity按照entity->finish插入st->active tree
static void __bfq_activate_requeue_entity(struct bfq_entity *entity,
					  struct bfq_sched_data *sd,
					  bool non_blocking_wait_rq)
{
    //返回entity或bfqq所属调度类对应的bfq_service_tree结构
	struct bfq_service_tree *st = bfq_entity_service_tree(entity);

    //如果entity是调度器正在使用的entiry或entity处于st->active树
	if (sd->in_service_entity == entity || entity->tree == &st->active)
		 /*
		  * in service or already queued on the active tree,
		  * requeue or reposition
		  */
		//根据entity->service计算entity的虚拟运行时间并累加到entity->finish，最后把entity按照新的entity->finish插入st->active红黑树
		__bfq_requeue_entity(entity);
	else
		/*
		 * Not in service and not queued on its active tree:
		 * the activity is idle and this is a true activation.
		 */
		//计算新的entity->start，按照entity->budget计算entity的虚拟运行时间并累加到entity->finish，把entity按照entity->finish插入st->active tree
		__bfq_activate_entity(entity, non_blocking_wait_rq);
}


/**
 * bfq_activate_requeue_entity - activate or requeue an entity representing a
 *				 bfq_queue, and activate, requeue or reposition
 *				 all ancestors for which such an update becomes
 *				 necessary.
 * @entity: the entity to activate.
 * @non_blocking_wait_rq: true if this entity was waiting for a request
 * @requeue: true if this is a requeue, which implies that bfqq is
 *	     being expired; thus ALL its ancestors stop being served and must
 *	     therefore be requeued
 * @expiration: true if this function is being invoked in the expiration path
 *             of the in-service queue
 */
//如果entity是调度器正在使用的entiry或entity处于st->active树,计算entity的虚拟运行时间并累加到entity->finish，把entity按照新的
//entity->finish插入st->active红黑树。否则计算新的entity->start，按照entity->budget计算entity的虚拟运行时间并累加到entity->finish，
//最后把entity按照entity->finish插入st->active tree。最后从st查找合适的entity并更新到sd->next_in_service
static void bfq_activate_requeue_entity(struct bfq_entity *entity,
					bool non_blocking_wait_rq,
					bool requeue, bool expiration)
{
	struct bfq_sched_data *sd;

	for_each_entity(entity) {
		sd = entity->sched_data;
      //如果entity是调度器正在使用的entiry或entity处于st->active树,计算entity的虚拟运行时间并累加到entity->finish，把entity按照新的
      //entity->finish插入st->active红黑树。否则计算新的entity->start，按照entity->budget计算entity的虚拟运行时间并累加到entity->finish，
      //最后把entity按照entity->finish插入st->active tree
		__bfq_activate_requeue_entity(entity, sd, non_blocking_wait_rq);

        //从st查找合适的entity并更新到sd->next_in_service，没有找到则返回NULL
		if (!bfq_update_next_in_service(sd, entity, expiration) &&
		    !requeue)
			break;
	}
}

/**
 * __bfq_deactivate_entity - deactivate an entity from its service tree.
 * @entity: the entity to deactivate.
 * @ins_into_idle_tree: if false, the entity will not be put into the
 *			idle tree.
 *
 * Deactivates an entity, independently of its previous state.  Must
 * be invoked only if entity is on a service tree. Extracts the entity
 * from that tree, and if necessary and allowed, puts it into the idle
 * tree.
 */
//把entity从st->active或st->idle红黑树踢掉，ins_into_idle_tree为true则把entity再插入st->idle树，否则直接释放掉entity及bfqq.剔除返回true
bool __bfq_deactivate_entity(struct bfq_entity *entity, bool ins_into_idle_tree)
{
	struct bfq_sched_data *sd = entity->sched_data;
	struct bfq_service_tree *st;
	bool is_in_service;

	if (!entity->on_st_or_in_serv) /*
					* entity never activated, or
					* already inactive
					*/
		return false;

	/*
	 * If we get here, then entity is active, which implies that
	 * bfq_group_set_parent has already been invoked for the group
	 * represented by entity. Therefore, the field
	 * entity->sched_data has been set, and we can safely use it.
	 */
	//找到entiry所属st
	st = bfq_entity_service_tree(entity);
    //entity是sd正在使用的entity则is_in_service为true
	is_in_service = entity == sd->in_service_entity;
    
    //service是entity已经消耗的配额，除以entity权重weight得到entity已经消耗的虚拟运行时间，再加上entity->start得到entity->finish
	bfq_calc_finish(entity, entity->service);

	if (is_in_service)//把sd->in_service_entity指向的entity从红黑树剔除时，要把sd->in_service_entity清NULL
		sd->in_service_entity = NULL;
	else
		/*
		 * Non in-service entity: nobody will take care of
		 * resetting its service counter on expiration. Do it
		 * now.
		 */
		entity->service = 0;

	if (entity->tree == &st->active)//如果entity处于st->active树,从st->acitve tree剔除entity
		bfq_active_extract(st, entity);
	else if (!is_in_service && entity->tree == &st->idle)//如果entity处于st->idle树，并且entity不是st正在使用的entity
		bfq_idle_extract(st, entity);//entity从st->idle tree剔除掉，可能更新st->first_idle或st->last_idle

    //ins_into_idle_tree为false则不会把entity再插入到st->idle,而是释放掉entity。或者st->vtime>=entity->finish也是释放掉entity
	if (!ins_into_idle_tree || !bfq_gt(entity->finish, st->vtime))//if和else测试有成立
		bfq_forget_entity(st, entity, is_in_service);//entity不再被用到，释放bfqq,把entity剔除掉
	else
		bfq_idle_insert(st, entity);//把entity再插入st->idle树，并可能更新st->first_idle或st->last_idle

	return true;
}

/**
 * bfq_deactivate_entity - deactivate an entity representing a bfq_queue.
 * @entity: the entity to deactivate.
 * @ins_into_idle_tree: true if the entity can be put into the idle tree
 * @expiration: true if this function is being invoked in the expiration path
 *             of the in-service queue
 */
//1:把entity从st->active或st->idle红黑树踢掉，ins_into_idle_tree为true则把entity再插入st->idle树，否则直接释放掉entity及bfqq。
//2:计算entity的虚拟运行时间并累加到entity->finish，最后把entity按照新的entity->finish插入st->idle红黑树
static void bfq_deactivate_entity(struct bfq_entity *entity,
				  bool ins_into_idle_tree,
				  bool expiration)
{
	struct bfq_sched_data *sd;
	struct bfq_entity *parent = NULL;

	for_each_entity_safe(entity, parent) {
		sd = entity->sched_data;

        //把entity从st->active或st->idle红黑树踢掉，ins_into_idle_tree为true则把entity再插入st->idle树，否则直接释放掉entity
        //及bfqq。剔除成功返回true
		if (!__bfq_deactivate_entity(entity, ins_into_idle_tree)) {
			/*
			 * entity is not in any tree any more, so
			 * this deactivation is a no-op, and there is
			 * nothing to change for upper-level entities
			 * (in case of expiration, this can never
			 * happen).
			 */
			return;
		}

        //entity是正在sd->next_in_service指向的调度器正在使用的entiry，则要选择一个新的next_in_service entity
		if (sd->next_in_service == entity)
			/*
			 * entity was the next_in_service entity,
			 * then, since entity has just been
			 * deactivated, a new one must be found.
			 */
			//从st查找合适的entity并更新到sd->next_in_service，没有找到则返回NULL
			bfq_update_next_in_service(sd, NULL, expiration);

		if (sd->next_in_service || sd->in_service_entity) {
			/*
			 * The parent entity is still active, because
			 * either next_in_service or in_service_entity
			 * is not NULL. So, no further upwards
			 * deactivation must be performed.  Yet,
			 * next_in_service has changed.	Then the
			 * schedule does need to be updated upwards.
			 *
			 * NOTE If in_service_entity is not NULL, then
			 * next_in_service may happen to be NULL,
			 * although the parent entity is evidently
			 * active. This happens if 1) the entity
			 * pointed by in_service_entity is the only
			 * active entity in the parent entity, and 2)
			 * according to the definition of
			 * next_in_service, the in_service_entity
			 * cannot be considered as
			 * next_in_service. See the comments on the
			 * definition of next_in_service for details.
			 */
			break;
		}

		/*
		 * If we get here, then the parent is no more
		 * backlogged and we need to propagate the
		 * deactivation upwards. Thus let the loop go on.
		 */

		/*
		 * Also let parent be queued into the idle tree on
		 * deactivation, to preserve service guarantees, and
		 * assuming that who invoked this function does not
		 * need parent entities too to be removed completely.
		 */
		ins_into_idle_tree = true;
	}

	/*
	 * If the deactivation loop is fully executed, then there are
	 * no more entities to touch and next loop is not executed at
	 * all. Otherwise, requeue remaining entities if they are
	 * about to stop receiving service, or reposition them if this
	 * is not the case.
	 */
	entity = parent;
	for_each_entity(entity) {//普通测试时没有parent，不成立
		/*
		 * Invoke __bfq_requeue_entity on entity, even if
		 * already active, to requeue/reposition it in the
		 * active tree (because sd->next_in_service has
		 * changed)
		 */
		//计算entity的虚拟运行时间并累加到entity->finish，最后把entity按照新的entity->finish插入st->active红黑树
		__bfq_requeue_entity(entity);

		sd = entity->sched_data;
        //从st查找合适的entity并更新到sd->next_in_service，没有找到则返回NULL
		if (!bfq_update_next_in_service(sd, entity, expiration) &&
		    !expiration)
			/*
			 * next_in_service unchanged or not causing
			 * any change in entity->parent->sd, and no
			 * requeueing needed for expiration: stop
			 * here.
			 */
			break;
	}
}

/**
 * bfq_calc_vtime_jump - compute the value to which the vtime should jump,
 *                       if needed, to have at least one entity eligible.
 * @st: the service tree to act upon.
 *
 * Assumes that st is not empty.
 */
static u64 bfq_calc_vtime_jump(struct bfq_service_tree *st)
{
    //st->active 红黑树的root entity
	struct bfq_entity *root_entity = bfq_root_active_entity(&st->active);

	if (bfq_gt(root_entity->min_start, st->vtime))//st->vtime小于root_entity->min_start则返回root_entity->min_start
		return root_entity->min_start;

	return st->vtime;
}

static void bfq_update_vtime(struct bfq_service_tree *st, u64 new_value)
{
	if (new_value > st->vtime) {
        //更新st->vtime
		st->vtime = new_value;
		bfq_forget_idle(st);
	}
}

/**
 * bfq_first_active_entity - find the eligible entity with
 *                           the smallest finish time
 * @st: the service tree to select from.
 * @vtime: the system virtual to use as a reference for eligibility
 *
 * This function searches the first schedulable entity, starting from the
 * root of the tree and going on the left every time on this side there is
 * a subtree with at least one eligible (start <= vtime) entity. The path on
 * the right is followed only if a) the left subtree contains no eligible
 * entities and b) no eligible entity has been found yet.
 */
//从st->active tree红黑树最左边查找entity->start最小并且entity->start小于vtime的entity
static struct bfq_entity *bfq_first_active_entity(struct bfq_service_tree *st,
						  u64 vtime)
{
	struct bfq_entity *entry, *first = NULL;
	struct rb_node *node = st->active.rb_node;

	while (node) {
		entry = rb_entry(node, struct bfq_entity, rb_node);
left:
		if (!bfq_gt(entry->start, vtime))//entry->start < vtime 则if成立
			first = entry;

		if (node->rb_left) {//如果entry有左节点，一直向左查找，有靠左的entry，entry->start和entry->min_start越小
			entry = rb_entry(node->rb_left,
					 struct bfq_entity, rb_node);
			if (!bfq_gt(entry->min_start, vtime)) {
				node = node->rb_left;
				goto left;
			}
		}
		if (first)
			break;
		node = node->rb_right;
	}

	return first;
}

/**
 * __bfq_lookup_next_entity - return the first eligible entity in @st.
 * @st: the service tree.
 *
 * If there is no in-service entity for the sched_data st belongs to,
 * then return the entity that will be set in service if:
 * 1) the parent entity this st belongs to is set in service;
 * 2) no entity belonging to such parent entity undergoes a state change
 * that would influence the timestamps of the entity (e.g., becomes idle,
 * becomes backlogged, changes its budget, ...).
 *
 * In this first case, update the virtual time in @st too (see the
 * comments on this update inside the function).
 *
 * In constrast, if there is an in-service entity, then return the
 * entity that would be set in service if not only the above
 * conditions, but also the next one held true: the currently
 * in-service entity, on expiration,
 * 1) gets a finish time equal to the current one, or
 * 2) is not eligible any more, or
 * 3) is idle.
 */
static struct bfq_entity *
__bfq_lookup_next_entity(struct bfq_service_tree *st, bool in_service)
{
	struct bfq_entity *entity;
	u64 new_vtime;

    //st->active tree没有entiry则返回NULL。next entity必须从st->active的红黑树查询一个合适的entity
	if (RB_EMPTY_ROOT(&st->active))
		return NULL;

	/*
	 * Get the value of the system virtual time for which at
	 * least one entity is eligible.
	 */
	//返回st虚拟运行时间，返回root_entity->min_start或者st->vtime
	new_vtime = bfq_calc_vtime_jump(st);

	/*
	 * If there is no in-service entity for the sched_data this
	 * active tree belongs to, then push the system virtual time
	 * up to the value that guarantees that at least one entity is
	 * eligible. If, instead, there is an in-service entity, then
	 * do not make any such update, because there is already an
	 * eligible entity, namely the in-service one (even if the
	 * entity is not on st, because it was extracted when set in
	 * service).
	 */
	//st没有正在service 的entity则可能st->vtime = new_vtime
	if (!in_service)
		bfq_update_vtime(st, new_vtime);
    
    //从st->active tree红黑树最左边查找entity->start最小并且entity->start小于vtime的entity
	entity = bfq_first_active_entity(st, new_vtime);

	return entity;
}

/**
 * bfq_lookup_next_entity - return the first eligible entity in @sd.
 * @sd: the sched_data.
 * @expiration: true if we are on the expiration path of the in-service queue
 *
 * This function is invoked when there has been a change in the trees
 * for sd, and we need to know what is the new next entity to serve
 * after this change.
 */
static struct bfq_entity *bfq_lookup_next_entity(struct bfq_sched_data *sd,
						 bool expiration)
{
	struct bfq_service_tree *st = sd->service_tree;
	struct bfq_service_tree *idle_class_st = st + (BFQ_IOPRIO_CLASSES - 1);
	struct bfq_entity *entity = NULL;
	int class_idx = 0;

	/*
	 * Choose from idle class, if needed to guarantee a minimum
	 * bandwidth to this class (and if there is some active entity
	 * in idle class). This should also mitigate
	 * priority-inversion problems in case a low priority task is
	 * holding file system resources.
	 */
	if (time_is_before_jiffies(sd->bfq_class_idle_last_service +
				   BFQ_CL_IDLE_TIMEOUT)) {
		if (!RB_EMPTY_ROOT(&idle_class_st->active))
			class_idx = BFQ_IOPRIO_CLASSES - 1;
		/* About to be served if backlogged, or not yet backlogged */
		sd->bfq_class_idle_last_service = jiffies;
	}

	/*
	 * Find the next entity to serve for the highest-priority
	 * class, unless the idle class needs to be served.
	 */
	//在sd调度器每个调度策略st里都搜符合条件的 entity，都找不到返回NULL
	for (; class_idx < BFQ_IOPRIO_CLASSES; class_idx++) {
		/*
		 * If expiration is true, then bfq_lookup_next_entity
		 * is being invoked as a part of the expiration path
		 * of the in-service queue. In this case, even if
		 * sd->in_service_entity is not NULL,
		 * sd->in_service_entiy at this point is actually not
		 * in service any more, and, if needed, has already
		 * been properly queued or requeued into the right
		 * tree. The reason why sd->in_service_entity is still
		 * not NULL here, even if expiration is true, is that
		 * sd->in_service_entiy is reset as a last step in the
		 * expiration path. So, if expiration is true, tell
		 * __bfq_lookup_next_entity that there is no
		 * sd->in_service_entity.
		 */
		entity = __bfq_lookup_next_entity(st + class_idx,
						  sd->in_service_entity &&
						  !expiration);

		if (entity)
			break;
	}

	if (!entity)
		return NULL;

	return entity;
}

bool next_queue_may_preempt(struct bfq_data *bfqd)
{
	struct bfq_sched_data *sd = &bfqd->root_group->sched_data;

	return sd->next_in_service != sd->in_service_entity;
}

/*
 * Get next queue for service.
 */
//核心是取出sd->next_in_service赋于sd->in_service_entity作为马上要使用的entity，这就是新选择的bfq_queue的entity。最后从st查找合适的
//entity并更新到sd->next_in_service，没有找到则设置sd->next_in_service为NULL
struct bfq_queue *bfq_get_next_queue(struct bfq_data *bfqd)
{
	struct bfq_entity *entity = NULL;
	struct bfq_sched_data *sd;
	struct bfq_queue *bfqq;
    //如果没有bfqq直接return NULL
	if (bfq_tot_busy_queues(bfqd) == 0)
		return NULL;

	/*
	 * Traverse the path from the root to the leaf entity to
	 * serve. Set in service all the entities visited along the
	 * way.
	 */
	sd = &bfqd->root_group->sched_data;
	for (; sd ; sd = entity->my_sched_data) {//测试时这个循环只执行了一次
		/*
		 * WARNING. We are about to set the in-service entity
		 * to sd->next_in_service, i.e., to the (cached) value
		 * returned by bfq_lookup_next_entity(sd) the last
		 * time it was invoked, i.e., the last time when the
		 * service order in sd changed as a consequence of the
		 * activation or deactivation of an entity. In this
		 * respect, if we execute bfq_lookup_next_entity(sd)
		 * in this very moment, it may, although with low
		 * probability, yield a different entity than that
		 * pointed to by sd->next_in_service. This rare event
		 * happens in case there was no CLASS_IDLE entity to
		 * serve for sd when bfq_lookup_next_entity(sd) was
		 * invoked for the last time, while there is now one
		 * such entity.
		 *
		 * If the above event happens, then the scheduling of
		 * such entity in CLASS_IDLE is postponed until the
		 * service of the sd->next_in_service entity
		 * finishes. In fact, when the latter is expired,
		 * bfq_lookup_next_entity(sd) gets called again,
		 * exactly to update sd->next_in_service.
		 */

		/* Make next_in_service entity become in_service_entity */
        //取出sd->next_in_service作为当前要使用的entity，赋值给sd->in_service_entity
		entity = sd->next_in_service;
		sd->in_service_entity = entity;

		/*
		 * If entity is no longer a candidate for next
		 * service, then it must be extracted from its active
		 * tree, so as to make sure that it won't be
		 * considered when computing next_in_service. See the
		 * comments on the function
		 * bfq_no_longer_next_in_service() for details.
		 */
		//如果没有多余的entity作为sd->next_in_service，则要从st->acitve tree剔除entity。这个if很容易成立，只要entity不是NULL
		//if就成立。就是说，sd->in_service_entity这个entity作为bfq当前正在使用的entity，就要把entity从st->actvie tree剔除
		if (bfq_no_longer_next_in_service(entity))
			bfq_active_extract(bfq_entity_service_tree(entity),
					   entity);

		/*
		 * Even if entity is not to be extracted according to
		 * the above check, a descendant entity may get
		 * extracted in one of the next iterations of this
		 * loop. Such an event could cause a change in
		 * next_in_service for the level of the descendant
		 * entity, and thus possibly back to this level.
		 *
		 * However, we cannot perform the resulting needed
		 * update of next_in_service for this level before the
		 * end of the whole loop, because, to know which is
		 * the correct next-to-serve candidate entity for each
		 * level, we need first to find the leaf entity to set
		 * in service. In fact, only after we know which is
		 * the next-to-serve leaf entity, we can discover
		 * whether the parent entity of the leaf entity
		 * becomes the next-to-serve, and so on.
		 */
	}

	bfqq = bfq_entity_to_bfqq(entity);

	/*
	 * We can finally update all next-to-serve entities along the
	 * path from the leaf entity just set in service to the root.
	 */
	for_each_entity(entity) {//for循环只进行一次
		struct bfq_sched_data *sd = entity->sched_data;
        
        //从st查找合适的entity并更新到sd->next_in_service，没有找到则设置sd->next_in_service为NULL
		if (!bfq_update_next_in_service(sd, NULL, false))
			break;
	}

	return bfqq;
}

/* returns true if the in-service queue gets freed */
//主要是对bfqd->in_service_queue置NULL
bool __bfq_bfqd_reset_in_service(struct bfq_data *bfqd)
{
	struct bfq_queue *in_serv_bfqq = bfqd->in_service_queue;
	struct bfq_entity *in_serv_entity = &in_serv_bfqq->entity;
	struct bfq_entity *entity = in_serv_entity;

	bfq_clear_bfqq_wait_request(in_serv_bfqq);
	hrtimer_try_to_cancel(&bfqd->idle_slice_timer);
	bfqd->in_service_queue = NULL;

	/*
	 * When this function is called, all in-service entities have
	 * been properly deactivated or requeued, so we can safely
	 * execute the final step: reset in_service_entity along the
	 * path from entity to the root.
	 */
	for_each_entity(entity)
		entity->sched_data->in_service_entity = NULL;

	/*
	 * in_serv_entity is no longer in service, so, if it is in no
	 * service tree either, then release the service reference to
	 * the queue it represents (taken with bfq_get_entity).
	 */
	if (!in_serv_entity->on_st_or_in_serv) {
		/*
		 * If no process is referencing in_serv_bfqq any
		 * longer, then the service reference may be the only
		 * reference to the queue. If this is the case, then
		 * bfqq gets freed here.
		 */
		int ref = in_serv_bfqq->ref;
		bfq_put_queue(in_serv_bfqq);
		if (ref == 1)
			return true;
	}

	return false;
}

void bfq_deactivate_bfqq(struct bfq_data *bfqd, struct bfq_queue *bfqq,
			 bool ins_into_idle_tree, bool expiration)
{
	struct bfq_entity *entity = &bfqq->entity;
    //1:把entity从st->active或st->idle红黑树踢掉，ins_into_idle_tree为true则把entity再插入st->idle树，否则直接释放掉entity及bfqq。
    //2:计算entity的虚拟运行时间并累加到entity->finish，最后把entity按照新的entity->finish插入st->idle红黑树
	bfq_deactivate_entity(entity, ins_into_idle_tree, expiration);
}

void bfq_activate_bfqq(struct bfq_data *bfqd, struct bfq_queue *bfqq)
{
	struct bfq_entity *entity = &bfqq->entity;
    //把bfqq插入到st->active红黑树
	bfq_activate_requeue_entity(entity, bfq_bfqq_non_blocking_wait_rq(bfqq),
				    false, false);
	bfq_clear_bfqq_non_blocking_wait_rq(bfqq);
}

void bfq_requeue_bfqq(struct bfq_data *bfqd, struct bfq_queue *bfqq,
		      bool expiration)
{
	struct bfq_entity *entity = &bfqq->entity;
   //如果entity是调度器正在使用的entiry或entity处于st->active树,计算entity的虚拟运行时间并累加到entity->finish，把entity按照新的
   //entity->finish插入st->active红黑树。否则计算新的entity->start，按照entity->budget计算entity的虚拟运行时间并累加到entity->finish，
   //最后把entity按照entity->finish插入st->active tree。最后从st查找合适的entity并更新到sd->next_in_service
	bfq_activate_requeue_entity(entity, false,
				    bfqq == bfqd->in_service_queue, expiration);
}

/*
 * Called when the bfqq no longer has requests pending, remove it from
 * the service tree. As a special case, it can be invoked during an
 * expiration.
 */
//bfqq没有req派发了或者bfqq失效了，清理bfqq的busy状态，把bfqq(entity)从st->active或st->idle红黑树踢掉，有可能再把bfqq插入到st->idle红黑树
void bfq_del_bfqq_busy(struct bfq_data *bfqd, struct bfq_queue *bfqq,
		       bool expiration)
{
	bfq_log_bfqq(bfqd, bfqq, "del from busy");
    //清理bfqq busy状态
	bfq_clear_bfqq_busy(bfqq);
    //每从st->active红黑树剔除一个bfqq，bfqq对应调度算法的bfqd->busy_queues[]成员见1
	bfqd->busy_queues[bfqq->ioprio_class - 1]--;

	if (bfqq->wr_coeff > 1)
		bfqd->wr_busy_queues--;

	bfqg_stats_update_dequeue(bfqq_group(bfqq));
    //1:把entity从st->active或st->idle红黑树踢掉，ins_into_idle_tree为true则把entity再插入st->idle树，否则直接释放掉entity及bfqq。
    //2:计算entity的虚拟运行时间并累加到entity->finish，最后把entity按照新的entity->finish插入st->idle红黑树
	bfq_deactivate_bfqq(bfqd, bfqq, true, expiration);

    //bfqq上没有派发req时,对bfq_weight_counter的成员从queue_weights_tree中执行remove
	if (!bfqq->dispatched)
		bfq_weights_tree_remove(bfqd, bfqq);
}

/*
 * Called when an inactive queue receives a new request.
 */
//把bfqq插入到st->active红黑树，标记bfqq busy
void bfq_add_bfqq_busy(struct bfq_data *bfqd, struct bfq_queue *bfqq)
{
	bfq_log_bfqq(bfqd, bfqq, "add to busy");
    //把bfqq插入到st->active红黑树
	bfq_activate_bfqq(bfqd, bfqq);

	bfq_mark_bfqq_busy(bfqq);//标记bfqq busy
	//每向st->active红黑树添加一个bfqq，bfqq对应调度算法的bfqd->busy_queues[]成员加1
	bfqd->busy_queues[bfqq->ioprio_class - 1]++;

	if (!bfqq->dispatched)
		if (bfqq->wr_coeff == 1)
			bfq_weights_tree_add(bfqd, bfqq,
					     &bfqd->queue_weights_tree);

	if (bfqq->wr_coeff > 1)
		bfqd->wr_busy_queues++;
}
