/* SPDX-License-Identifier: GPL-2.0 */
#ifndef __LINUX_BRIDGE_NETFILTER_H
#define __LINUX_BRIDGE_NETFILTER_H

#include <uapi/linux/netfilter_bridge.h>
#include <linux/skbuff.h>

enum nf_br_hook_priorities {
	NF_BR_PRI_FIRST = INT_MIN,
	NF_BR_PRI_NAT_DST_BRIDGED = -300,
	NF_BR_PRI_FILTER_BRIDGED = -200,
	NF_BR_PRI_BRNF = 0,
	NF_BR_PRI_NAT_DST_OTHER = 100,
	NF_BR_PRI_FILTER_OTHER = 200,
	NF_BR_PRI_NAT_SRC = 300,
	NF_BR_PRI_LAST = INT_MAX,
};

#if IS_ENABLED(CONFIG_BRIDGE_NETFILTER)

int br_handle_frame_finish(struct net *net, struct sock *sk, struct sk_buff *skb);

static inline void br_drop_fake_rtable(struct sk_buff *skb)
{
	struct dst_entry *dst = skb_dst(skb);

	if (dst && (dst->flags & DST_FAKE_RTABLE))
		skb_dst_drop(skb);
}

static inline struct nf_bridge_info *
nf_bridge_info_get(const struct sk_buff *skb)
{
	return skb->nf_bridge;
}

static inline bool nf_bridge_info_exists(const struct sk_buff *skb)
{
	return skb->nf_bridge != NULL;
}

static inline int nf_bridge_get_physinif(const struct sk_buff *skb)
{
	const struct nf_bridge_info *nf_bridge = nf_bridge_info_get(skb);

	if (!nf_bridge)
		return 0;

	return nf_bridge->physindev ? nf_bridge->physindev->ifindex : 0;
}

static inline int nf_bridge_get_physoutif(const struct sk_buff *skb)
{
	const struct nf_bridge_info *nf_bridge = nf_bridge_info_get(skb);

	if (!nf_bridge)
		return 0;

	return nf_bridge->physoutdev ? nf_bridge->physoutdev->ifindex : 0;
}

static inline struct net_device *
nf_bridge_get_physindev(const struct sk_buff *skb)
{
	const struct nf_bridge_info *nf_bridge = nf_bridge_info_get(skb);

	return nf_bridge ? nf_bridge->physindev : NULL;
}

static inline struct net_device *
nf_bridge_get_physoutdev(const struct sk_buff *skb)
{
	const struct nf_bridge_info *nf_bridge = nf_bridge_info_get(skb);

	return nf_bridge ? nf_bridge->physoutdev : NULL;
}

static inline bool nf_bridge_in_prerouting(const struct sk_buff *skb)
{
	const struct nf_bridge_info *nf_bridge = nf_bridge_info_get(skb);

	return nf_bridge && nf_bridge->in_prerouting;
}
#else
#define br_drop_fake_rtable(skb)	        do { } while (0)
static inline bool nf_bridge_in_prerouting(const struct sk_buff *skb)
{
	return false;
}
#endif /* CONFIG_BRIDGE_NETFILTER */

#endif
